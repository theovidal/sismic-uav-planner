let left i = 2 * i + 1
let right i = 2 * i + 2
let up i = (i - 1) / 2

type t =
    {mutable last : int;
     priorities : float array;
     keys : int array;
     mapping : int array}

  let length q = q.last + 1

  let capacity q = Array.length q.keys

  let make_empty n =
    {last = -1;
     priorities = Array.make n nan;
     keys = Array.make n 0;
     mapping = Array.make n (-1)}

  let mem q x =
    q.mapping.(x) >= 0

  let swap t i j =
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp

  let full_swap q i j =
    swap q.keys i j;
    swap q.priorities i j;
    swap q.mapping q.keys.(i) q.keys.(j) (* Bien échanger par rapport aux indices des clés dans mapping *)

  let get_min q = (q.keys.(0), q.priorities.(0))

  (* Attention, les indices correspondent bien aux clés *)
  let rec sift_up q i =
    let up = up i in
    if i > 0 && q.priorities.(i) < q.priorities.(up) then begin
      full_swap q i up;
      sift_up q up
    end

  (* Bien vérifier la capacité du tableau + faire le changement d'indice avant le sift_up *)
  let insert q (x, prio) =
    if length q = capacity q then failwith "dépassement de capacité";

    let i = q.last + 1 in
    q.priorities.(i) <- prio;
    q.keys.(i) <- x;
    q.mapping.(x) <- i;
    q.last <- i;
    sift_up q i

  let rec sift_down q i =
    let l = left i in
    let r = right i in
    let i_min = ref i in

    if l <= q.last && q.priorities.(l) < q.priorities.(!i_min) then i_min := l;
    if r <= q.last && q.priorities.(r) < q.priorities.(!i_min) then i_min := r;

    if !i_min <> i then begin
      full_swap q i !i_min;
      sift_down q !i_min
    end

  let extract_min q =
    if q.last < 0 then failwith "vide";

    let min = q.keys.(0) in
    let prio = q.priorities.(0) in
    full_swap q 0 q.last; (* d'abord faire le full swap *)
    q.mapping.(min) <- -1;
    q.last <- q.last - 1;
    sift_down q 0;
    min, prio

  (* Vérifier qu'on a bien une diminution de priorité *)
  let decrease_priority q (x, prio) =
    let i = q.mapping.(x) in
    assert (mem q x && prio <= q.priorities.(i));
    q.priorities.(i) <- prio;
    sift_down q i
