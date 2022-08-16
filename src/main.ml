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
                   VIDAL Théo - 932 MPI*
*)

let () =
  if Array.length Sys.argv < 3 then begin
    print_string "Please specify the file containing the points, and the number of generations";
    exit (-1)
  end;
  let filename = Sys.argv.(1) in
  match int_of_string_opt Sys.argv.(2) with
  | Some n ->
    let g = Utils.open_points filename in
    let better_a = ref [] in
    let better_diff = ref max_int in
    let better_b = ref [] in
    let better_pt = ref (0., 0.) in
    for _ = 1 to n do
      let group_a, group_b, pt = Partition.geometric_bisection g in
      let diff = abs (List.length group_a - List.length group_b) in
      Printf.printf "Diff: %d\n" diff;
      if diff < !better_diff then begin
        better_a := group_a;
        better_b := group_b;
        better_diff := diff;
        better_pt := pt
      end
    done;
    Printf.printf "-----------------------\nBetter point: %.3f,%.3f" (fst !better_pt) (snd !better_pt);
  | None -> print_string "Please enter a valid integer"

  (* let succ, root = prim g 0 in
  List.iter (fun i -> Printf.printf "%d - " i) (parcours succ root) *)