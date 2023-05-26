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

let print_point file (x, y) = Printf.fprintf file "%f %f\n" x y

let open_points filename =
  let stream = open_in filename in
  let n = int_of_string (input_line stream) in
  let points = Array.make n (0.,0.) in
  for i = 0 to n - 1 do
      let next = input_line stream in
      Scanf.sscanf next "%f,%f,%f" (fun x y _ ->
        points.(i) <- (x, y)
      )
  done;
  points

let open_classes n filename =
  let stream = open_in filename in
  let k = int_of_string (input_line stream) in
  let classes = Array.make n 0 in
  for i = 0 to n - 1 do
    classes.(i) <- int_of_string (input_line stream)
  done;
  classes, k


let () =
  if Array.length Sys.argv < 4 then begin
    print_string "Missing arguments. Valid syntax: program [points] [classes] [HEURISTIC] (output=permutation.txt)\n";
    exit (-1)
  end;
  let points_filename = Sys.argv.(1) in
  let classes_filename = Sys.argv.(2) in
  let heuristic = int_of_string Sys.argv.(3) in
  let output = if Array.length Sys.argv > 4 then Sys.argv.(4) else "permutation.txt" in

  let lock = Mutex.create () in
  let points = open_points points_filename in
  let refs, k = open_classes (Array.length points) classes_filename in
  let classes = Array.make k [] in
  Array.iteri (fun i c ->
    classes.(c) <- points.(i) :: classes.(c)
  ) refs;

  let output_file = open_out output in

  let save_zone pts sigma zone_id =
    let i = ref sigma.(0) in
    Mutex.lock lock;
    Printf.fprintf output_file "%d\n" (Array.length sigma);
    print_point output_file pts.(0);
    
    while !i <> 0 do
      print_point output_file pts.(!i);
      i := sigma.(!i) 
    done;
    Mutex.unlock lock;
    Printf.printf "Thread %d finished\n" zone_id;
  in

  let threads = ref [] in 

  Array.iteri (fun zone_id points ->
    threads := Thread.create (fun (zone_id, g) ->
      let points = Array.of_list g in
      if Array.length points = 0 then () else begin
        Printf.printf "%d\n" (Array.length points);
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
        let sigma = Annealing.annealing points heuristic in
        save_zone points sigma zone_id
      end
      ) (zone_id, points) :: !threads
    
  ) classes;
  (* (generate_zones pole g alpha zones rolls);*)

  List.iter Thread.join !threads;
  close_out output_file;
  Printf.printf "Done.\n"
  