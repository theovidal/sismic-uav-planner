(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
Tracing of paths for reconnaissance of an earthquake zone by drone
               Prim algorithm to trace paths
                   VIDAL Théo - 962 MPI*                       
*)

(* Transforms a tree stored under a predecessors array into an adjacence list *)
let tree_succ_from_pred pred =
  let n = Array.length pred in
  let succ = Array.make n [] in
  let root = ref (-1) in
  for i = 0 to n - 1 do
    if pred.(i) == i then root := i
    else if pred.(i) <> -1 then succ.(pred.(i)) <- i :: succ.(pred.(i));
  done;
  (succ, !root)

(* Prim algorithm: calculate a minimum weight covering tree *)
let prim g s =
  let n = Array.length g in
  let queue = Prioq.make_empty n in
  let cost = Array.make n infinity in
  let pred = Array.make n (-1) in
  cost.(s) <- 0.;
  pred.(s) <- s;
  for i = 0 to n - 1 do
    if i == s then Prioq.insert queue (i, 0.)
    else Prioq.insert queue (i, infinity)
  done;
  while Prioq.length queue <> 0 do
    let t, _ = Prioq.extract_min queue in
    for i = 0 to n - 1 do
      if Prioq.mem queue i then begin
        let d = Utils.distance g.(t) g.(i) in
        if cost.(i) > d then begin
          pred.(i) <- t;
          cost.(i) <- d;
          Prioq.decrease_priority queue (i, d)
        end
      end
    done;
  done;
  tree_succ_from_pred pred

(* Get the final path by browsing the tree successors array *)
let path t root =
  let res = ref [] in
  let rec aux x =
    res := x :: !res;
    List.iter (fun i -> aux i) t.(x);
  in aux root;
  Array.of_list (List.rev !res)
