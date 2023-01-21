(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
Tracing of paths for reconnaissance of an earthquake zone by drone
                Entry point of the program
                   VIDAL Théo - 962 MPI*
*)

open Owl_plplot
open Owl_dense_matrix

let colors = [| Plot.RGB (251, 140, 0); Plot.RGB (76, 175, 80); Plot.RGB (63, 81, 181); Plot.RGB (233, 30, 99) |]

let generate_zones initial n rolls =
  let nb = ref 1 in
  let zones = ref initial in

  while !nb < n do
    let new_zones = ref [] in
    List.iter (fun g ->
      let better_a = ref [] in
      let better_diff = ref max_int in
      let better_b = ref [] in
      let better_pt = ref (0., 0.) in
      for _ = 1 to rolls do
        let group_a, group_b, pt = Partition.geometric_bisection g in
        let diff =
          List.length group_a - List.length group_b
          |> abs
        in
        if diff < !better_diff then begin
          better_a := group_a;
          better_b := group_b;
          better_diff := diff;
          better_pt := pt
        end
      done;
      new_zones := !better_a :: !better_b :: !new_zones
    ) !zones;
    zones := !new_zones;
    nb := !nb * 2
  done;
  !zones

let () =
  if Array.length Sys.argv < 9 then begin
    print_string "Missing arguments. Valid syntax: program [FILE] [ZONES] [ROLLS] [X_MIN] [X_MAX] [Y_MIN] [Y_MAX] [HEURISTIC]";
    exit (-1)
  end;
  let filename = Sys.argv.(1) in
  let zones = int_of_string Sys.argv.(2) in
  let rolls = int_of_string Sys.argv.(3) in
  let x_min = Float.of_string Sys.argv.(4) in
  let x_max = Float.of_string Sys.argv.(5) in
  let y_min = Float.of_string Sys.argv.(6) in
  let y_max = Float.of_string Sys.argv.(7) in
  let heuristic = Float.of_string Sys.argv.(8) in
  let g, n = Utils.open_points filename in

  let h = Plot.create "plot.svg" in
  Plot.set_pen_size h 2.;
  let x_values = D.zeros 1 n in
  let y_values = D.zeros 1 n in
  let pt_i = ref 0 in
  let register_point x y =
    D.set x_values 0 !pt_i x;
    D.set y_values 0 !pt_i y;
    incr pt_i
  in
  let lock = Mutex.create () in
  let file = open_out "permutation.txt" in

  let add_to_plot pts sigma zone_id =
    Printf.printf "Thread %d finished\n" zone_id;
    let i = ref sigma.(0) in
    Mutex.lock lock;
    Printf.fprintf file "%d %f %f" zone_id (fst pts.(0)) (snd pts.(0));
    register_point (fst pts.(0)) (snd pts.(0));
      
    while !i <> 0 do
      let xA, yA = fst pts.(!i), snd pts.(!i) in
      register_point xA yA;
      let xB, yB = fst pts.(sigma.(!i)), snd pts.(sigma.(!i)) in
      Printf.fprintf file " %f %f" xB yB;
      Plot.(draw_line ~h ~spec:[ LineStyle 4; colors.(zone_id mod (Array.length colors)) ] xA yA xB yB);
      i := sigma.(!i)
    done;
    Printf.fprintf file "\n";
    Mutex.unlock lock
  in

  let threads = ref [] in 

  List.iteri (fun zone_id g ->
    threads := Thread.create (fun (zone_id, g) ->
      let pts = Array.of_list g in
      (* Covering tree method *)
      (*let succ, root = Prim.prim pts 0 in
      let path = Prim.path succ root in
      
      Array.iteri (fun i pt ->
        let xA, yA = fst pts.(pt), snd pts.(pt) in
        register_point xA yA;
        if i < Array.length path - 1 then
          let xB, yB = fst pts.(path.(i+1)), snd pts.(path.(i+1)) in
          Plot.(draw_line ~h ~spec:[ LineStyle 4; colors.(zone_id mod (Array.length colors)) ] xA yA xB yB)
      ) path *)

      (* TSP and Simulated anealing method *)
      let sigma = Annealing.annealing pts heuristic in
      add_to_plot pts sigma zone_id
      ) (zone_id, g) :: !threads
    
  ) (generate_zones [g] zones rolls);

  List.iter Thread.join !threads;

  Plot.(scatter ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 1. ] x_values y_values);
  Plot.set_xrange h x_min x_max;
  Plot.set_yrange h y_min y_max;
  Plot.output h;
  close_out file
  