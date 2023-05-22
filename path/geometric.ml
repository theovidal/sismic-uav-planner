(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
Tracing of paths for reconnaissance of an earthquake zone by drone
          Geometric partition algorithm to distringuish zones
                   VIDAL Théo - 962 MPI*
*)

open Utils

(* Matrix operations *)

let point_of_matrix m = {
  x = m.(0).(0);
  y = m.(1).(0);
  z = m.(2).(0);
  weight = 1.
}

let matrix_of_point p = [|
  [|p.x|];
  [|p.y|];
  [|p.z|]
|]

let add_points a b = {
  x = a.x +. b.x;
  y = a.y +. b.y;
  z = a.z +. b.z;
  weight = a.weight +. b.weight
}

(* Shorthand for the matrix product *)
let (<>*) = Matrix.product

(* Do the inverse stereographic projection ℝ² -> S³ based on the north pole point pole (center of the sphere on z=0) *)
let inverse_stereographic pole pt  =
  let t = 2. *. (pole.z ** 2.) /. ((pt.x -. pole.x) ** 2. +. (pt.y -. pole.y) ** 2. +. pole.z ** 2.) in
  {
    x = pole.x +. t *. (pt.x -. pole.x);
    y = pole.y +. t *. (pt.y -. pole.y);
    z = pole.z -. t *. pole.z;
    weight = pt.weight
  }

(* Do the projection S³ -> ℝ² based on the north pole point pole *)
let stereographic pole pt : Utils.point = 
  let t = pole.z /. (pole.z -. pt.z) in
  {
    x = pole.x +. t *. (pt.x -. pole.x);
    y = pole.y +. t *. (pt.y -. pole.y);
    z = 0.;
    weight = pt.weight
  }

(* Do the inverse stereographic projection and calculate the centroid *)
let project_to_sphere pole pts =
  let num_pts = ref 0 in
  let cx = ref 0. in
  let cy = ref 0. in
  let cz = ref 0. in
  let projected = List.map (fun pt ->
    let pt' = inverse_stereographic pole pt in
    incr num_pts;
    cx := pt'.x +. !cx;
    cy := pt'.y +. !cy;
    cz := pt'.z +. !cz;
    pt'
  ) pts in
  let n = float_of_int !num_pts in
  Printf.printf "centroid: (%.3f, %.3f, %.3f)\n" (!cx /. n) (!cy /. n) (!cz /. n);
  projected, {
    x = !cx /. n;
    y = !cy /. n;
    z = !cz /. n;
    weight = 1.
  }

(* Conformally map the points so that their centroid lies at the origin *)
let confort pole centroid =
  let local_centroid = {
    x = centroid.x -. pole.x;
    y = centroid.y -. pole.y;
    z = centroid.z;
    weight = 1.
  } in
  (* Spheric oordinates of the centroid *)
  let theta = atan ((hypot local_centroid.x  local_centroid.y) /. local_centroid.z) in
  let phi = atan (local_centroid.y /. local_centroid.x) in
  let r = sqrt (local_centroid.x ** 2. +. local_centroid.y ** 2. +. local_centroid.z ** 2.) in
  Printf.printf "theta: %.3f, phi: %.3f, r: %.3f\n" theta phi r;

  theta, phi, r
  (*, (List.map (fun (x, y, z) ->
    (* Performing rotations to have centroid at (0, 0, r) *)
    let rho =
    Matrix.y_rotation (-1. *. theta) <>*
      (Matrix.z_rotation (-1. *. phi) <>* Matrix.from_coords (x, y, z))
    |> Matrix.to_coords
    (* Performing dilation to have centroid at (0, 0, 0) *)
    in
    let x, y = stereographic rho in
    inverse_stereographic (r *. x, r *. y)
  ) pts)*)


(* Revert the operations done for conforting (dilation, rotation, inverse stereographic projection) *)
(* Made relative to the origin (0,0,0) and translated back to the working space *)
let unmap pole r theta phi =
  let flat_pole = {
    x = pole.x;
    y = pole.y;
    z = 0.;
    weight = 1.
  } in
  List.map (fun pt ->
    Matrix.product
      (Matrix.z_rotation phi)
      (
        Matrix.product
        (Matrix.y_rotation theta)
        (matrix_of_point {
          x = pt.x;
          y = pt.y;
          z = pt.z +. r;
          weight = pt.weight
          }
        )
      )
      |> point_of_matrix
      |> add_points flat_pole
      |> stereographic pole
    (*
    let pt' = stereographic local_pole pt in
    let pt'' =
    inverse_stereographic local_pole {
      x = pt'.x /. r;
      y = pt'.y /. r;
      z = 0.;
      weight = pt'.weight
    }
    |> stereographic local_pole in
    let coef = sqrt ((1. -. r)/.(1. +. r)) in
    Matrix.product
      (Matrix.z_rotation phi)
      (
        Matrix.product
        (Matrix.y_rotation theta)
        (matrix_of_point (inverse_stereographic local_pole {
          x = pt''.x /. coef;
          y = pt''.y /. coef;
          z = 0.;
          weight = pt''.weight
          })
        )
      )
    |> point_of_matrix
    |> stereographic pole*)
  )

(* To find a great circle, we take a point of the unit sphere and move it randomly,
  so we can define our circle by a center (the origin) and another point *)
let generate_great_circle r =
  Random.self_init ();
  let rand_theta = Random.float Float.pi in
  let rand_phi = Random.float (2. *. Float.pi) in
  Matrix.product
    (Matrix.product (Matrix.y_rotation rand_theta) (Matrix.z_rotation rand_phi))
    (Matrix.from_coords (0., 0., r))
  |> point_of_matrix

(* Geometric bisection algorithm *)
let geometric_bisection pole rolls points =
  let _, centroid = project_to_sphere pole points in
  let theta, phi, r = confort pole centroid in

  (* We calculate many great circles and find the one which best separates the set into half *)
  let better_in = ref [] in
  let better_diff = ref max_int in
  let better_out = ref [] in
  let better_pt = ref (Utils.null ()) in
  for _ = 1 to rolls do
    let great_circle_pt = generate_great_circle pole.z in

    (* Center of the circle is the first element of the array ; the other point belongs to C and is the helper to draw it *)
    let circle = unmap pole r theta phi [Utils.null (); great_circle_pt] in
    let circle_origin = List.hd circle in
    let circle_end = List.tl circle |> List.hd in
    let radius = Utils.distance2 circle_end circle_origin in
    Printf.printf "found radius: %.3f\n" radius;

    (* Make the two groups: in/out of the circle ; diff = card(in) - card(out) *)
    let rec discriminate group_in group_out diff = function
    | [] -> group_in, group_out, diff
    | point :: tl ->
      if Utils.distance2 point circle_origin < radius then discriminate (point :: group_in) group_out (diff + 1) tl
      else discriminate group_in (point :: group_out) (diff - 1) tl

    in let group_in, group_out, diff = discriminate [] [] 0 points in

    if abs diff < !better_diff then begin
      better_in := group_in;
      better_out := group_out;
      better_diff := abs diff;
      better_pt := circle_end
    end
  done;
  !better_in, !better_out
  
