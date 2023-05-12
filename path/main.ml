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
  if Array.length Sys.argv < 5 then begin
    print_string "Missing arguments. Valid syntax: program [file] [NB_ZONES] [NB_ROLLS] [HEURISTIC] (output=permutation.txt)\n";
    exit (-1)
  end;
  let filename = Sys.argv.(1) in
  let zones = int_of_string Sys.argv.(2) in
  let rolls = int_of_string Sys.argv.(3) in
  let heuristic = Float.of_string Sys.argv.(4) in
  let output = if Array.length Sys.argv > 5 then Sys.argv.(5) else "permutation.txt" in
  let g, _ = Utils.open_points filename in

  let lock = Mutex.create () in
  let file = open_out output in

  let print_point (x, y) = Printf.fprintf file "%f %f\n" x y in

  let save_zone pts sigma zone_id =
    let i = ref sigma.(0) in
    Mutex.lock lock;
    Printf.fprintf file "%d\n" (Array.length sigma);
    print_point pts.(0);
    
    while !i <> 0 do
      print_point pts.(!i);
      i := sigma.(!i) 
    done;
    Mutex.unlock lock;
    Printf.printf "Thread %d finished\n" zone_id;
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
      save_zone pts sigma zone_id
      ) (zone_id, g) :: !threads
    
  ) (generate_zones [g] zones rolls);

  List.iter Thread.join !threads;
  close_out file;
  Printf.printf "Done.\n"
  