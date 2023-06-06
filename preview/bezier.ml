open Owl_plplot
open Owl_dense_matrix

(* calculate the binomial (k, n) *)
let binomial k n =
  let rec aux acc k n =
    if k = 0. then acc
    else aux (acc *. n /. k) (k -. 1.) (n -. 1.)
  in
  aux 1. k n

(* get the point on the Bezier curve for the parameter t *)
let bezier_point t points =
  if t < 0. || t > 1. then failwith "bezier point calculation: t not in [0;1]";
  let n = Array.length points - 1 in
  let x = ref 0. in
  let y = ref 0. in
  for i = 0 to n do
    let i' = float_of_int i in
    let n' = float_of_int n in
    let bernstein = binomial i' n' *. (t ** i') *. ((1. -. t) ** (n' -. i')) in
    let xi, yi = points.(i) in
    x := !x +. bernstein *. xi;
    y := !y +. bernstein *. yi 
  done;
  Printf.printf "%.3f,%.3f\n" !x !y;
  !x, !y

let bounds pts =
  let x_min = ref infinity in
  let x_max = ref (infinity *. (-.1.)) in
  let y_min = ref infinity in
  let y_max = ref (infinity *. (-.1.)) in
  for i = 0 to Array.length pts - 1 do
    let (x, y) = pts.(i) in
    x_min := min !x_min x;
    x_max := max !x_max x;
    y_min := min !y_min y;
    y_max := max !y_max y
  done;
  !x_min, !x_max, !y_min, !y_max

(* Plot the bezier curve associated to the points with `precision` points for each *)
let plot_bezier input_file output_file x_min x_max y_min y_max = 
  let precision = 100 in
  let h = Plot.create output_file in
  Plot.set_pen_size h 2.;
  let perms, _nb_zones, _nb_points = Utils.read_permutation input_file in

  let add_zone (_, points) =
    let zone_xmin, zone_xmax, zone_ymin, zone_ymax = bounds points in
    if zone_xmin < x_min || zone_xmax > x_max || zone_ymin < y_min || zone_ymax > y_max then
      ()
    else
    let color = Plot.RGB (Random.int 255, Random.int 255, Random.int 255) in
    let n = Array.length points in
    let curb_x = D.zeros 1 (precision * n * 4) in
    let curb_y = D.zeros 1 (precision * n * 4) in
    let pt_i = ref 0 in
    let register_point (x, y) =
      D.set curb_x 0 !pt_i x;
      D.set curb_y 0 !pt_i y;
      incr pt_i
    in

    let points_x = D.zeros 1 n in
    let points_y = D.zeros 1 n in
    for i = 0 to n - 1 do
      let (x, y) = points.(i) in
      if x > x_min && x < x_max && y > y_min && y < y_max then begin
        D.set points_x 0 i x;
        D.set points_y 0 i y
      end
    done;
    Plot.(scatter ~h ~spec:[ MarkerSize 4.7 ] points_x points_y);

    for i = 0 to precision do
      let t = float_of_int i /. float_of_int precision in
      bezier_point t points
      |> register_point
    done;

    (*let k = ref 0 in

    while !k + 4 < n do
      for i = 0 to precision do
        let t = float_of_int i /. float_of_int precision in
        Array.sub points !k 4
        |> bezier_point t
        |> register_point
      done;
      k := !k + 3 (* Last point of previous curb must match first point of the next one *)
    done;*)

    Plot.(scatter ~h ~spec:[ color; MarkerSize 2.7 ] curb_x curb_y);
  in
  List.iter add_zone perms;
  
  Plot.set_xrange h x_min x_max;
  Plot.set_yrange h y_min y_max;
  Plot.output h
