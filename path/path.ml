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
  let classes = Array.make n 0 in
  if filename = "null" then classes, 1 else
  let stream = open_in filename in
  let k = int_of_string (input_line stream) in
  for i = 0 to n - 1 do
    classes.(i) <- int_of_string (input_line stream)
  done;
  classes, k


let () =
  if Array.length Sys.argv < 4 then begin
    print_string "Missing arguments. Valid syntax: program [points] [classes?] [HEURISTIC] (method:annealing/covering) (output=permutation.txt) (data=none)\n";
    exit (-1)
  end;
  let points_filename = Sys.argv.(1) in
  let classes_filename = Sys.argv.(2) in
  let heuristic = int_of_string Sys.argv.(3) in
  let meth = Sys.argv.(4) in
  let output = if Array.length Sys.argv > 5 then Sys.argv.(5) else "permutation.txt" in
  let data_folder = if Array.length Sys.argv > 6 then Sys.argv.(6) else "" in
  let benchmark_output = if data_folder <> "" then open_out (data_folder ^ "/benchmark.csv") else stdout in

  let lock = Mutex.create () in
  let points = open_points points_filename in
  let refs, k = open_classes (Array.length points) classes_filename in
  let classes = Array.make k [] in
  Array.iteri (fun i c ->
    classes.(c) <- points.(i) :: classes.(c)
  ) refs;

  let output_file = open_out output in

  let save_zone pts zone_id sigma score time =
    let i = ref sigma.(0) in
    Mutex.lock lock;
    Printf.fprintf output_file "%d %d\n" zone_id (Array.length sigma);
    Printf.fprintf benchmark_output "%d,%d,%f,%f\n" zone_id heuristic score time;
    print_point output_file pts.(0);
    
    while !i <> 0 do
      print_point output_file pts.(!i);
      i := sigma.(!i) 
    done;
    Mutex.unlock lock
  in

  let threads = ref [] in

  (* Precalculating the average delta to adjust well the temperature for the algorithm *)
  let start_time = Sys.time () in
  let delta = if meth = "annealing" then Annealing.average_delta points (Array.length classes) heuristic else 0. in
  let end_time = Sys.time () in
  Printf.fprintf benchmark_output "delta,%d,%f\n" heuristic (end_time -. start_time);

  Array.iteri (fun zone_id points ->
    threads := Thread.create (fun (zone_id, g) ->
      let points = Array.of_list g in
      let n = Array.length points in
      if n = 0 then () else begin
        if meth = "annealing" then 
          (* TSP with Simulated anealing method *)
          let sigma, score, time = Annealing.annealing zone_id points (heuristic * n) delta data_folder in
          save_zone points zone_id sigma score time

        (*else if meth = "covering" then
          (* Covering tree method *)
          let succ, root = Prim.prim pts 0 in
          let path = Prim.path succ root in*)
          
        else failwith "Unknown method"
      end
      ) (zone_id, points) :: !threads
    
  ) classes;

  List.iter Thread.join !threads;
  close_out output_file;
  Printf.printf "Path: Done.\n"
  