(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
Tracing of paths for reconnaissance of an earthquake zone by drone
                 "Zone partitioning" part
                   VIDAL Théo - 962 MPI*
*)

let (<>*) = Matrix.product

(* Do the inverse stereographic projection ℝ² -> S³ on one point *)
let inverse_stereographic (x, y) =
  let norm = x *. x +. y *. y +. 1. in
  2. *. x /. norm, 2. *. y /. norm, (x *. x +. y *. y -. 1.) /. norm

(* Do the projection S³ -> ℝ² on one point *)
let stereographic (x, y, z) = x /. (1. -. z), y /. (1. -. z)

(* Do the inverse stereographic projection and calculate the centroid *)
let project_to_sphere pts =
  let num_pts = ref 0 in
  let cx = ref 0. in
  let cy = ref 0. in
  let cz = ref 0. in
  let projected = List.map (fun pt ->
    let x, y, z = inverse_stereographic pt in
    incr num_pts; cx := x +. !cx; cy := y +. !cy; cz := z +. !cz;
    x, y, z
  ) pts in
  let n = float_of_int !num_pts in
  projected, (!cx/.n, !cy/.n, !cz/.n)

(* Conformally map the points so that their centroid lies at the origin *)
let confort _ (cx, cy, cz) =
  (* Spheric oordinates of the centroid *)
  let theta = atan (hypot cx cy /. cz) in
  let phi = atan (cy /. cx) in
  let r = sqrt (cx *. cx +. cy *. cy +. cz *. cz) in

  theta, phi, r(*, (List.map (fun (x, y, z) ->
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
let unmap r theta phi =
  List.map (fun pt ->
    let x, y = stereographic pt in
    let xp, yp =
    inverse_stereographic (x /. r, y /. r)
    |> stereographic in
    let coef = sqrt ((1. -. r)/.(1. +. r)) in
    Matrix.z_rotation phi <>*
      (Matrix.y_rotation theta <>* (Matrix.from_coords (inverse_stereographic (xp /. coef, yp /. coef))))
    |> Matrix.to_coords
    |> stereographic
  )

let geometric_bisection points =
  let projected, centroid = project_to_sphere points in
  let theta, phi, r = confort projected centroid in

  (* To find a great circle, we take a point of the unit sphere and move it randomly, so we can define our circle by a center (the origin) and another point *)
  Random.self_init ();
  let rand_theta = Random.float (2. *. Float.pi) in
  let rand_phi = Random.float (2. *. Float.pi) in
  let great_circle_pt = (Matrix.y_rotation rand_theta <>* Matrix.z_rotation rand_phi) <>* Matrix.from_coords (0.,0.,1.) in

  Printf.printf "r = %f\n" r;

  (* Center of the circle is the first element of the array ; the other point belongs to C and is the helper to draw it *)
  let circle = unmap r theta phi [(0., 0., 0.); Matrix.to_coords great_circle_pt] in
  let o = List.hd circle in
  let a = List.tl circle |> List.hd in
  Printf.printf "Circle C: %.3f,%.3f — %.3f,%.3f — " (fst o) (snd o) (fst a) (snd a);
  let radius = Utils.distance a o in

  (* Make the two groups: in/out of the circle *)
  let group_a = ref [] in
  let group_b = ref [] in
  List.iter (fun p ->
    if Utils.distance p o < radius then group_a := p :: !group_a
    else group_b := p :: !group_b
    ) points;
  !group_a, !group_b, a
