(* A permutation is a tuple of an integer n and two arrays of size n *)
type permutation = int * int array * int array

(* Applies sigma to i *)
let next_in ((_, sigma, _) : permutation) i = sigma.(i)
let prev_in ((_, _, sigmainv) : permutation) i = sigmainv.(i)

(* Composes the sigma application n times *)
let rec next_at (perm : permutation) i n =
  if n = 0 then i
  else next_at perm (next_in perm i) (n - 1)

(* Composes the inverse sigma application n times *)
let rec prev_at (perm : permutation) i n =
  if n = 0 then i
  else prev_at perm (prev_in perm i) (n - 1)

let set_destination ((_, sigma, sigmainv) : permutation) i j =
  sigma.(i) <- j;
  sigmainv.(j) <- i

let size ((n, _, _) : permutation) = n

let distance (xA, yA) (xB, yB) =
  (xB -. xA) ** 2. +. (yB -. yA) ** 2.

(* Generate a random permutation with the Fisher-Yates shuffle *)
let random_permutation n : permutation =
  let sigma = Array.init n (fun i -> i + 1) in
  let sigmainv = Array.init n (fun i -> i - 1) in
  sigma.(n - 1) <- 0;
  sigmainv.(0) <- n - 1;
  (n, sigma, sigmainv)

(* Generate a random integer but not a, b or c *)
let random_except n a b c =
  let rec aux () =
    let x = Random.int n in
    if x <> a && x <> b && x <> c then x
    else aux ()
  in aux ()

(* Generate a random integer between a (inclusive) and b (exclusive) *)
let random_between a b = Random.int (b - a) + a

(* Copy an array into another one *)
let copy src dest =
  Array.iteri (fun i x -> dest.(i) <- x) src

(* Elementary transformation applied at each step of the algorithm *)
type transformation = INVERT | SWAP | INSERT of int | INSERT_ROUTE of int * int

(* Compute the difference between the tour without and with the transformation *)
let delta transformation a b points perm =
  let invA = prev_in perm a in
  let nextB = next_in perm b in

  match transformation with
  | INVERT ->
    distance points.(invA) points.(b)
    +. distance points.(a) points.(nextB)
    -. distance points.(invA) points.(a)
    -. distance points.(b) points.(nextB)

  | SWAP ->
    let nextA, invB = next_in perm a, prev_in perm b in
    distance points.(invA) points.(b)
    +. distance points.(b) points.(nextA)
    +. distance points.(invB) points.(a)
    +. distance points.(a) points.(nextB)
    -. distance points.(invA) points.(a)
    -. distance points.(a) points.(nextA)
    -. distance points.(invB) points.(b)
    -. distance points.(b) points.(nextB)

  | INSERT c ->
    let invC, nextC = prev_in perm c, next_in perm c in
    distance points.(a) points.(c)
    +. distance points.(c) points.(b)
    +. distance points.(invC) points.(nextC)
    -. distance points.(a) points.(b)
    -. distance points.(invC) points.(c)
    -. distance points.(c) points.(nextC)

  | INSERT_ROUTE (start_point, length) ->
    let end_point = next_at perm start_point length in
    let invStart, nextEnd = prev_in perm start_point, next_in perm end_point in
    distance points.(a) points.(start_point)
    +. distance points.(end_point) points.(b)
    +. distance points.(invStart) points.(nextEnd)
    -. distance points.(a) points.(b)
    -. distance points.(invStart) points.(start_point)
    -. distance points.(end_point) points.(nextEnd)

let get_random_transformation a points perm : float * transformation * int =
  match Random.int 4 with
  | 0 ->
    delta INVERT a (next_in perm a) points perm, INVERT, (next_in perm a)
  | 1 ->
    let c = random_except (size perm) a (next_in perm a) (prev_in perm a) in
    delta SWAP a c points perm, SWAP, c
  | 2 ->
    let c = random_except (size perm) a (next_in perm a) (-1) in
    let transformation = INSERT c in
    let b = next_in perm a in
    delta transformation a b points perm, transformation, b
  | 3 ->
    let length = Random.int (size perm - 3) + 1 in (* adding one to avoid length 0 *)
    let c =
      random_between (length + 1) (size perm - 1)
      |> prev_at perm a in
    let transformation = INSERT_ROUTE (c, length) in
    let b = next_in perm a in
    delta transformation a b points perm, transformation, b
  | _ -> failwith "impossible"

let permute transformation a b perm =
  match transformation with
  | INVERT ->
    let invA, nextB = prev_in perm a, next_in perm b in
    set_destination perm invA b;
    set_destination perm b a;
    set_destination perm a nextB
    
  | SWAP -> 
    let invA, nextA = prev_in perm a, next_in perm a in
    let invB, nextB = prev_in perm b, next_in perm b in
    set_destination perm invA b;
    set_destination perm b nextA;
    set_destination perm invB a;
    set_destination perm a nextB

  | INSERT c ->
    let invC, nextC = prev_in perm c, next_in perm c in
    set_destination perm a c;
    set_destination perm c b;
    set_destination perm invC nextC

  | INSERT_ROUTE (start_point, length) ->
    let end_point = next_at perm start_point length in
    let invStart, nextEnd = prev_in perm start_point, next_in perm end_point in
    set_destination perm a start_point;
    set_destination perm end_point b;
    set_destination perm invStart nextEnd

(* Compute the distance of a circuit *)
let circuit_distance points ((_, sigma, _) : permutation) =
  let d = ref (distance points.(0) points.(sigma.(0))) in
  let i = ref sigma.(0) in
  while !i <> 0 do
    d := distance points.(!i) points.(sigma.(!i)) +. !d;
    i := sigma.(!i);
  done;
  !d

(* Calculate the average delta of n circuits after p random permutations *)
let average_delta points n p =
  Printf.printf "Calculating average delta...\n";
  let m = ref 0. in
  for _i = 1 to n do
    let perm = random_permutation (Array.length points) in
    let d = ref 0. in
    for _j = 1 to p do
      let a = Random.int (Array.length points) in
      let delta, transformation, b = get_random_transformation a points perm in
      permute transformation a b perm |> ignore;
      d := !d +. delta;
    done;
    m := !m +. (!d /. float_of_int p);
  done;
  Printf.printf "Average delta: %f\n" (!m /. float_of_int n);
  !m /. float_of_int n

(* Simulated annealing algorithm *)
let annealing zone_id points nb_operations delta data_folder =
  let output = open_out (Printf.sprintf "%s/annealing-data-%d.txt" data_folder zone_id) in
  Random.self_init ();
  let n = Array.length points in
  let invT = ref (1. /. (100. *. delta)) in
  let reason = 0.0001 ** (-. 1. /. float_of_int nb_operations) in
  let perm = random_permutation n in
  let f = ref (circuit_distance points perm) in
  let nb_ops = ref 0 in
  
  let start_time = Sys.time () in
  while !invT < 100. *. delta do
    if !nb_ops mod 1000 = 0 then Printf.fprintf output "%f\n" !f;
    (* Manipulating the inverse in order not to manipulate floats *)
    invT := !invT *. reason;

    let a = Random.int n in
    let delta, transformation, b = get_random_transformation a points perm in
    let p = exp (-.delta *. !invT) in

    (* Metropolis rule *)
    if delta < 0. || Random.float 1. < p then (
      permute transformation a b perm
      |> ignore;
      f := !f +. delta;
      incr nb_ops;
    )
  done;
  let end_time = Sys.time () in
  let _, sigma, _ = perm in
  close_out output;
  sigma, (end_time -. start_time)
