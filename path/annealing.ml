type permutation = (int * int array * int array)
type points = (float * float) array

let random_except n a b =
  let c = ref a in
  while !c == a || !c == b do
    c := Random.int n
  done;
  !c

let copy src dest =
  Array.iteri (fun i x -> dest.(i) <- x) src

(* Generate a random permutation with the Fisher-Yates shuffle *)
let basic_permutation n : permutation =
  let sigma = Array.init n (fun i -> i + 1) in
  let sigmainv = Array.init n (fun i -> i - 1) in
  sigma.(n - 1) <- 0;
  sigmainv.(0) <- n - 1;
  (n, sigma, sigmainv)

let permute ?(transformation = -2) ?(a = -1) pts (n, sigma, sigmainv : permutation) : float * int * int =
  let a = (if a = -1 then Random.int n else a) in
  let b = sigma.(a) in
  let invA = sigmainv.(a) in
  let nextB = sigma.(b) in

  match transformation with
  | -2 ->
    if Random.int 2 == 0 then (* REVERSE *)
         Utils.distance pts.(invA) pts.(b)
      +. Utils.distance pts.(a) pts.(nextB)
      -. Utils.distance pts.(invA) pts.(a)
      -. Utils.distance pts.(b) pts.(nextB),     -1, a
    else (* TRANSPORT *)
      let c = random_except n a b in
      let invC, nextC = sigmainv.(c), sigma.(c) in
         Utils.distance pts.(a) pts.(c)
      +. Utils.distance pts.(c) pts.(b)
      +. Utils.distance pts.(invC) pts.(nextC)
      -. Utils.distance pts.(a) pts.(b)
      -. Utils.distance pts.(invC) pts.(c)
      -. Utils.distance pts.(c) pts.(nextC),     c, a

  | -1 -> (* REVERSE*)
    sigma.(invA) <- b;
    sigma.(b) <- a;
    sigma.(a) <- nextB;

    sigmainv.(nextB) <- a;
    sigmainv.(a) <- b;
    sigmainv.(b) <- invA;
    0., -1, -1
    
  | c -> (* TRANSPORT *)
    let invC, nextC = sigmainv.(c), sigma.(c) in
    sigma.(a) <- c;
    sigma.(c) <- b;
    sigmainv.(b) <- c;
    sigmainv.(c) <- a;

    sigma.(invC) <- nextC;
    sigmainv.(nextC) <- invC;
    0., c, -1

let circuit_distance (pts : points) sigma =
  let d = ref (Utils.distance pts.(0) pts.(sigma.(0))) in
  let i = ref sigma.(0) in
  while !i <> 0 do
    d := Utils.distance pts.(!i) pts.(sigma.(!i)) +. !d;
    i := sigma.(!i);
  done;
  !d

exception Too_few_changes

let annealing (pts : points) h =
  let n = Array.length pts in
  let invT = ref 0. in
  let (_, sigma, sigmainv) = basic_permutation n in
  let f = ref (circuit_distance pts sigma) in

  let changes = ref n in

  while !changes > 0 do
    changes := 0;
    invT := !invT +. 1.; (* astuce pour éviter de calculer des flottants lourds *)
    let _threshold = exp (!invT *. h) in
    for _k = 0 to int_of_float h (*ceil threshold*) do
      (* Printf.printf "%d/%d\n" k (int_of_float (ceil threshold)); *)
      let delta, transformation, a = permute pts (n, sigma, sigmainv) in
      let p = exp (-.delta *. !invT) in
      (* Metropolis rule *)
      if delta < 0. || Random.float 1. < p then (
        incr changes;
        let _, _, _ = permute ~transformation ~a pts (n, sigma, sigmainv) in
        f := !f +. delta
      )
    done;
  done;
  sigma
