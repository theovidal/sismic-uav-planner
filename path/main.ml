(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
      Tracing of paths for reconnaissance of an earthquake zone by drone
Entry point of the path program, to distinguish zones and trace paths inside them
                           VIDAL Théo - 962 MPI*
*)

let generate_zones pole initial n rolls =
  let nb = ref 1 in
  let zones = ref initial in

  while !nb < n do
    let new_zones = ref [] in
    List.iter (fun g ->
      if List.length g = 0 then () else
      let better_in, better_out = Geometric.geometric_bisection pole rolls g in
      new_zones := better_in :: better_out :: !new_zones
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
  let heuristic = int_of_string Sys.argv.(4) in
  let output = if Array.length Sys.argv > 5 then Sys.argv.(5) else "permutation.txt" in
  let g = Utils.open_points filename in
  let pole = Utils.calculate_sphere_pole g in
  Printf.printf "Pole: %.3f %.3f %.3f\n" pole.x pole.y pole.z;

  let lock = Mutex.create () in
  let file = open_out output in

  let save_zone pts sigma zone_id =
    let i = ref sigma.(0) in
    Mutex.lock lock;
    Printf.fprintf file "%d\n" (Array.length sigma);
    Utils.print_point file pts.(0);
    
    while !i <> 0 do
      Utils.print_point file pts.(!i);
      i := sigma.(!i) 
    done;
    Mutex.unlock lock;
    Printf.printf "Thread %d finished\n" zone_id;
  in

  let threads = ref [] in 

  List.iteri (fun zone_id g ->
    threads := Thread.create (fun (zone_id, g) ->
      let pts = Array.of_list g in
      if Array.length pts = 0 then () else begin
        Printf.printf "%d\n" (Array.length pts);
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
      end
      ) (zone_id, g) :: !threads
    
  ) (generate_zones pole [g] zones rolls);

  List.iter Thread.join !threads;
  close_out file;
  Printf.printf "Done.\n"
  