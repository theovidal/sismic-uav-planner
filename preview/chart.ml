open Owl_plplot
open Owl_dense_matrix

let print_chart input_file output_file x_min x_max y_min y_max =
  let perms, _nb_zones, nb_points = Utils.read_permutation input_file in
  let h = Plot.create output_file in
  Plot.set_pen_size h 2.;
  let x_values = D.zeros 1 nb_points in
  let y_values = D.zeros 1 nb_points in
  let pt_i = ref 0 in
  let register_point (x, y) =
    D.set x_values 0 !pt_i x;
    D.set y_values 0 !pt_i y;
    incr pt_i
  in

  let add_to_plot (zone_id, pts) =
    Random.self_init ();
    let (x, y) = Utils.barycenter pts in
    Plot.(text ~h ~spec:[ RGB (0,255,0) ] x y (string_of_int zone_id));
    let color = Plot.RGB (Random.int 255, Random.int 255, Random.int 255) in

    let draw_line (xA, yA) (xB, yB) =
      Plot.(draw_line ~h ~spec:[ LineStyle 4; color; LineWidth 1.7 ] xA yA xB yB)
    in

    let n = Array.length pts in
    for i = 0 to n - 2 do
      register_point pts.(i);
      draw_line pts.(i) pts.(i + 1);
    done;
    draw_line pts.(n - 1) pts.(0);
    register_point pts.(n - 1)
  in

  List.iter add_to_plot perms;

  Plot.(scatter ~h ~spec:[ Marker "#[0x25CF]"; MarkerSize 2.7 ] x_values y_values);
  Plot.set_xrange h x_min x_max;
  Plot.set_yrange h y_min y_max;
  Plot.output h
