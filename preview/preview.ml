let methods = [
  ("geojson", "Write a GeoJSON file", "output.geojson", Geojson.output_geojson);
  (*("bezier", "Plot a bezier curve", "output.svg", Bezier.plot_bezier);*)
  (*("chart", "Plot a chart", "output.svg", Chart.print_chart);*)
]

let () =
  if Array.length Sys.argv < 2 then begin
    print_string "Missing arguments. Valid syntax: program [input] [XMIN] [XMAX] [YMIN] [YMAX] (method) (output=output.geojson)";
    exit (-1)
  end;

  let input_file = Sys.argv.(1) in
  let x_min = float_of_string Sys.argv.(2) in
  let x_max = float_of_string Sys.argv.(3) in
  let y_min = float_of_string Sys.argv.(4) in
  let y_max = float_of_string Sys.argv.(5) in
  let meth = Sys.argv.(6) in
  
  let rec choose_method = function
    | [] ->
      print_string "Invalid method ; please choose among: ";
      List.iter (fun (name, _, _, _) -> print_string name; print_string " ") methods;
      exit (-1) |> ignore;
    | (name, _, default_file, f) :: _ when meth = name ->
      let output_file = if Array.length Sys.argv > 7 then Sys.argv.(7) else default_file in
      f input_file output_file x_min x_max y_min y_max
    | _ :: tl -> choose_method tl

  in choose_method methods;
  
  print_string "Done.\n"

