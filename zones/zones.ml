let methods = [
  "kmeans", Kmeans.kmeans;
  "kcenters", Kcenters.kcenters;
  "geometric", Geometric.generate_zones;
]

let () =
  if Array.length Sys.argv < 5 then begin
    print_string "Missing arguments. Valid syntax: program (input) [NB_ZONES] [ALPHA] [NB_ROLLS] (method) (output=classes.txt)\n";
    exit (-1)
  end;
  let input = Sys.argv.(1) in
  let zones = int_of_string Sys.argv.(2) in
  let alpha = float_of_string Sys.argv.(3) in
  let rolls = int_of_string Sys.argv.(4) in
  let meth = Sys.argv.(5) in
  let output = if Array.length Sys.argv > 6 then Sys.argv.(6) else "classes.txt" in

  let output_file = open_out output in

  let points = Utils.open_points input in
  
  let rec choose_method = function
  | [] -> failwith (Printf.sprintf "Invalid method ; valide methods are %s" (String.concat ", " (List.map fst methods)))
  | (name, f) :: _ when name = meth ->
    let start = Sys.time () in
    let res = f points zones rolls alpha in
    let stop = Sys.time () in
    Printf.printf "Time spent (in seconds): %f\n" (stop -. start);
    res
  | _ :: tl -> choose_method tl in
  
  let classes, nb_classes = choose_method methods in
  Printf.fprintf output_file "%d\n" nb_classes;
  Array.iter (fun c -> Printf.fprintf output_file "%d\n" c) classes;

  close_out output_file;
  print_string "Zones: Done.\n"

