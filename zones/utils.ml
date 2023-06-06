type zone = {
  mutable ids : int list;
  mutable average_distance : float;
  mutable max_distance : float;
}

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

let add_points a b = {
  x = a.x +. b.x;
  y = a.y +. b.y;
  z = a.z +. b.z;
  weight = a.weight +. b.weight
}

(* Maximum and average distance between two points in a list *)
let max_and_average_distance points ids =
  let max_dist = ref 0. in
  let avg_dist = ref 0. in
  let num_pts = ref 0 in
  List.iter (fun id ->
    incr num_pts;
    let pt = points.(id) in
    List.iter (fun id' ->
      let pt' = points.(id') in
      let dist = distance2 pt pt' in
      max_dist := max !max_dist dist;
      avg_dist := !avg_dist +. dist
    ) ids
  ) ids;
  let n = float_of_int !num_pts in
  !max_dist, !avg_dist /. (n *. (n -. 1.))

(* Calculate the north pole of a set of points for stereographic projection *)
let calculate_sphere_pole points =
  let n = Array.length points in
  let cx = ref 0. in
  let cy = ref 0. in
  let max_dist = ref 0. in
  for i = 0 to Array.length points - 1 do
    let pt = points.(i) in
      cx := pt.x +. !cx;
      cy := pt.y +. !cy;
      for j = 0 to n - 1 do
        let pt' = points.(j) in
        let dist = distance2 pt pt' in
        max_dist := max !max_dist dist
      done
  done;
  {
    x = !cx /. (float_of_int n);
    y = !cy /. (float_of_int n);
    z = !max_dist;
    weight = 1.
  }

(* Read a CSV file and return a list of points *)
(* expected format: x,y,weight *)
let open_points filename =
  let stream = open_in filename in
  let n = int_of_string (input_line stream) in
  let points = Array.make n (null ()) in
  for i = 0 to n - 1 do
      let next = input_line stream in
      Scanf.sscanf next "%f,%f,%f" (fun x y weight ->
        points.(i) <- { x; y; z = 0.; weight }
      )
  done;
  points
