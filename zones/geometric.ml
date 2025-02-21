open Utils

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
let project_to_sphere pole points zone =
  let num_pts = ref 0 in
  let cx = ref 0. in
  let cy = ref 0. in
  let cz = ref 0. in
  let projected = List.map (fun id ->
    let pt = points.(id) in
    let pt' = inverse_stereographic pole pt in
    incr num_pts;
    cx := pt'.x +. !cx;
    cy := pt'.y +. !cy;
    cz := pt'.z +. !cz;
    pt'
  ) zone
  in
  let n = float_of_int !num_pts in
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

  theta, phi, r


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
        (Matrix.of_point {
          x = pt.x;
          y = pt.y;
          z = pt.z +. r;
          weight = pt.weight
          }
        )
      )
      |> Matrix.to_point
      |> Utils.add_points flat_pole
      |> stereographic pole
  )

(* To find a great circle, we take a point of the unit sphere and move it randomly,
  so we can define our circle by a center (the origin) and another point (the normal) *)
let generate_great_circle r =
  Random.self_init ();
  let rand_theta = Random.float Float.pi in
  let rand_phi = Random.float (2. *. Float.pi) in
  Matrix.product
    (Matrix.product (Matrix.y_rotation rand_theta) (Matrix.z_rotation rand_phi))
    (Matrix.from_coords (0., 0., r))
  |> Matrix.to_point

(* Geometric bisection algorithm *)
let geometric_bisection pole rolls points zone =
  let _, centroid = project_to_sphere pole points zone in
  let theta, phi, r = confort pole centroid in

  (* We calculate many great circles and find the one which best separates the set into half *)
  let zone_in = {
    ids = [];
    average_distance = 0.;
    max_distance = 0.
  } in
  let zone_out = {
    ids = [];
    average_distance = 0.;
    max_distance = 0.
  } in
  let better_diff = ref infinity in
  for _ = 1 to rolls do
    let great_circle_pt = generate_great_circle pole.z in

    (* Center of the circle is the first element of the array ; the other point belongs to C and is the helper to draw it *)
    let circle = unmap pole r theta phi [Utils.null (); great_circle_pt] in
    let circle_origin = List.hd circle in
    let circle_end = List.tl circle |> List.hd in
    let radius = Utils.distance2 circle_end circle_origin in

    (* Make the two groups: in/out of the circle ; diff = card(in) - card(out) *)
    let rec discriminate group_in group_out diff = function
    | [] -> group_in, group_out, diff
    | point :: tl ->
      if Utils.distance2 points.(point) circle_origin < radius then discriminate (point :: group_in) group_out (diff +. points.(point).weight) tl
      else discriminate group_in (point :: group_out) (diff -. points.(point).weight) tl

    in
    let group_in, group_out, diff = discriminate [] [] 0. zone in

    if abs_float diff < !better_diff then begin
      zone_in.ids <- group_in;
      zone_out.ids <- group_out;
      better_diff := abs_float diff
    end
  done;

  let max_in, avg_in = Utils.max_and_average_distance points zone_in.ids in
  zone_in.average_distance <- avg_in;
  zone_in.max_distance <- max_in;

  let max_out, avg_out = Utils.max_and_average_distance points zone_out.ids in
  zone_out.average_distance <- avg_out;
  zone_out.max_distance <- max_out;

  zone_in, zone_out
  
let rec range a b =
  if a >= b then []
  else a :: range (a + 1) b

(* Execute geometric bisection until enough zones are determined *)
let generate_zones points nb_zones rolls =
  let n = Array.length points in

  let pole = Utils.calculate_sphere_pole points in
  Printf.printf "Pole: %.3f %.3f %.3f\n" pole.x pole.y pole.z;
  let nb = ref 1 in
  let zones = ref [range 0 n] in
  let classes = Array.make n 0 in

  while !nb < nb_zones do
    let new_zones = ref [] in
    List.iter (fun zone ->
      let group_in, group_out = geometric_bisection pole rolls points zone in
      new_zones := group_in.ids :: group_out.ids :: !new_zones
    ) !zones;
    zones := !new_zones;
    nb := !nb * 2
  done;
  List.iteri (fun i zone ->
    List.iter (fun pt -> classes.(pt) <- i) zone
    ) !zones;
  classes, !nb
