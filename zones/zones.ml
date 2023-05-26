(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
      Tracing of paths for reconnaissance of an earthquake zone by drone
Entry point of the zones program, to distinguish zones considering density and weight parameters
                           VIDAL Théo - 962 MPI*
*)

open Utils

let _generate_zones pole initial alpha n rolls =
  let nb = ref 0 in
  let zones = ref [] in

  let rec split zone =
    print_newline ();
    Printf.printf "Splitting zone %d\n" !nb;
    print_newline ();
    if !nb < n && [] <> zone then begin
      let better_in, better_out = Geometric.geometric_bisection pole rolls zone in
      if better_in.max_distance >= alpha *. better_in.average_distance
        then split better_in.points
        else (
          zones := better_in.points :: !zones;
          incr nb
        );
      if better_out.max_distance >= alpha *. better_out.average_distance
        then split better_out.points
      else (
        zones := better_out.points :: !zones;
        incr nb
      );
      end
    in
  split initial;
  Array.of_list !zones


let () =
  if Array.length Sys.argv < 5 then begin
    print_string "Missing arguments. Valid syntax: program [file] [NB_ZONES] [ALPHA] [NB_ROLLS] (method) (output=classes.txt)\n";
    exit (-1)
  end;
  let input = Sys.argv.(1) in
  let zones = int_of_string Sys.argv.(2) in
  let _alpha = float_of_string Sys.argv.(3) in
  let rolls = int_of_string Sys.argv.(4) in
  let _meth = Sys.argv.(5) in
  let output = if Array.length Sys.argv > 6 then Sys.argv.(6) else "classes.txt" in

  let output_file = open_out output in

  let points = Utils.open_points input in
  let pole = Utils.calculate_sphere_pole points in
  Printf.printf "Pole: %.3f %.3f %.3f\n" pole.x pole.y pole.z;
  Printf.fprintf output_file "%d\n" zones;

  Array.iter (fun c -> Printf.fprintf output_file "%d\n" c) (Kmeans.kmeans points zones rolls);

  close_out output_file;
  print_string "Done.\n"

