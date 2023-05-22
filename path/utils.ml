type point = {
  x: float;
  y: float;
  z: float;
  weight: float
}

let null () = {
  x = 0.;
  y = 0.;
  z = 0.;
  weight = 0.
}

(* Euclidian distance in R² *)
let distance2 p p' = sqrt ((p'.x -. p.x) ** 2. +. (p'.y -. p.y) ** 2.)

(* Calculate the north pole of a set of points for stereographic projection *)
let calculate_sphere_pole points =
  let num_pts = ref 0 in
  let cx = ref 0. in
  let cy = ref 0. in
  let max_dist = ref 0. in
  List.iter (fun pt ->
    incr num_pts;
    cx := pt.x +. !cx;
    cy := pt.y +. !cy;
    List.iter (fun pt' ->
      let dist = distance2 pt pt' in
      max_dist := max !max_dist dist
    ) points
  ) points;
  let n = float_of_int !num_pts in
  {
    x = !cx /. n;
    y = !cy /. n;
    z = !max_dist;
    weight = 1.
  }

(* Read a CSV file and return a list of points *)
(* expected format: x,y,weight *)
let open_points filename =
  let stream = open_in filename in
  let rec aux acc =
    try
      let next = input_line stream in
      Scanf.sscanf next "%f,%f,%f" (fun x y weight ->
        aux ({
          x = x;
          y = y;
          z = 0.;
          weight = weight
        } :: acc)
      )
    with
    | End_of_file -> close_in stream; acc
  in aux []

let print_point file point = Printf.fprintf file "%f %f\n" point.x point.y
