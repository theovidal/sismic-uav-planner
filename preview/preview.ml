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
  let output_file = if Array.length Sys.argv > 7 then Sys.argv.(7) else "output.svg" in

  if meth = "bezier" then
    Bezier.plot_bezier input_file output_file 200 x_min x_max y_min y_max
  else if meth = "chart" then
    Chart.print_chart input_file output_file x_min x_max y_min y_max
  else begin
    print_string "Invalid method. Valid methods: bezier, chart";
    exit (-1)
  end;
  
  print_string "Done.\n"

