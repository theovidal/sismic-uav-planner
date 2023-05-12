open Owl_plplot
open Owl_dense_matrix

(* Read a permutation that the path program wrote *)
let read_permutation filename =
  let file = open_in filename in
  let rec aux acc nb_tot =
    try
      Scanf.sscanf (input_line file) "%d" (fun nb_sigma ->
          let sigma = Array.make nb_sigma (0., 0.) in
          for i = 0 to nb_sigma - 1 do
            Scanf.sscanf (input_line file) "%f %f" (fun x y -> sigma.(i) <- (x, y))
          done;
        aux (sigma :: acc) (nb_tot + nb_sigma)
      )
    with End_of_file -> close_in file; acc, nb_tot
  in aux [] 0

let () =
  if Array.length Sys.argv < 6 then begin
    print_string "Missing arguments. Valid syntax: program [input] [X_MIN] [X_MAX] [Y_MIN] [Y_MAX] (output=output.svg)";
    exit (-1)
  end;

  let input_file = Sys.argv.(1) in
  let x_min = Float.of_string Sys.argv.(2) in
  let x_max = Float.of_string Sys.argv.(3) in
  let y_min = Float.of_string Sys.argv.(4) in
  let y_max = Float.of_string Sys.argv.(5) in
  let output_file = if Array.length Sys.argv > 6 then Sys.argv.(6) else "output.svg" in
  
  let perms, n = read_permutation input_file in
  
  let h = Plot.create output_file in
  Plot.set_pen_size h 2.;
  let x_values = D.zeros 1 n in
  let y_values = D.zeros 1 n in
  let pt_i = ref 0 in
  let register_point (x, y) =
    D.set x_values 0 !pt_i x;
    D.set y_values 0 !pt_i y;
    incr pt_i
  in

  let add_to_plot pts =
    Random.self_init ();
    let color = Plot.RGB (Random.int 255, Random.int 255, Random.int 255) in

    let draw_line (xA, yA) (xB, yB) =
      Plot.(draw_line ~h ~spec:[ LineStyle 4; color ] xA yA xB yB)
    in

    let n = Array.length pts in
    for i = 0 to n - 2 do
      register_point pts.(i);
      draw_line pts.(i) pts.(i + 1);
    done;
    draw_line pts.(n - 1) pts.(0);
    register_point pts.(n - 1)
  in

  List.iter (fun sigma -> add_to_plot sigma) perms;

  Plot.(scatter ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 1. ] x_values y_values);
  Plot.set_xrange h x_min x_max;
  Plot.set_yrange h y_min y_max;
  Plot.output h
