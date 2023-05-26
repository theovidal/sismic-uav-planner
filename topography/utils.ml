(* Properties such as labels and weights are defined in a separate file *)
let props = Yojson.Basic.from_file "topography/settings.json"

(* Structure to hold a point with its data *)
type point = {
  x : float;
  y : float;
  nature : string;
  use : string;
  state : string;
  nb_housing : int;
  height : float;
}

type parsed_point = {
  x : float;
  y : float;
  weight : float;
}

let new_parsed_point x y weight = {
  x; y; weight
}

(* Euclidian distance in R² *)
let distance p p' = sqrt ((p'.x -. p.x) ** 2. +. (p'.y -. p.y) ** 2.)

(* Read a CSV file and return a list of points *)
(* expected format: x,y,nature,use,state,nb_housing,height *)
let open_points filename =
  let stream = open_in filename |> Csv.of_channel in
  let rec aux acc i =
    try
      let next = Csv.next stream |> Array.of_list in
      Printf.printf "\rReading points: %d" i;
      aux ({
          x =
            ((float_of_string next.(0)
            |> int_of_float) mod 800000) / 10
            |> float_of_int;
          y =
            ((float_of_string next.(1)
            |> int_of_float) mod 6000000) / 10
            |> float_of_int;
          nature = next.(2);
          use = next.(3);
          state = next.(4);
          nb_housing = if next.(5) = "" then 0 else int_of_string next.(5);
          height = if next.(6) = "" then 3. else float_of_string next.(6)
        } :: acc) (i + 1)
    with
    | End_of_file -> Csv.close_in stream; Array.of_list acc
  in aux [] 0

(* Calculates weight of a point with its data and the properties defined in the settings file *)
let parse_point (point : point) : parsed_point =
  let open Yojson.Basic.Util in
  let initial = 
    if point.nature = (props |> member "tower_label" |> to_string) then point.height *. point.height
    else point.height in

  let coef =
    if point.use = (props |> member "residential_label" |> to_string) then
      (props |> member "residential_weight" |> to_float) +. (props |> member "residential_housing_weight" |> to_float) *. (float_of_int point.nb_housing)
    else if point.use = (props |> member "commercial_label" |> to_string) then
      props |> member "commercial_weight" |> to_float
    else if point.use = (props |> member "industrial_label" |> to_string) then
      props |> member "industrial_weight" |> to_float
    else props |> member "default_weight" |> to_float in

  {
    x = point.x;
    y = point.y;
    weight = initial *. coef
  }

let gather points classes =
  Array.map (fun l ->
    let xc, yc, wc = ref 0., ref 0., ref 0. in
    List.iter (fun i ->
      let pt = points.(i) in
      xc := !xc +. pt.x;
      yc := !yc +. pt.y;
      wc := !wc +. pt.weight
      ) l;
    xc := !xc /. float_of_int (List.length l);
    yc := !yc /. float_of_int (List.length l);
    {
      x = !xc;
      y = !yc;
      weight = !wc
    }
    ) classes

(* Print a point in a file in CSV format *)
let print_point file point = Printf.fprintf file "%f,%f,%f\n" point.x point.y point.weight
