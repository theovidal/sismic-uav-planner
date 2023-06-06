open Utils

let distance_to_center pt center = sqrt ((pt.x -. center.x) ** 2. +. (pt.y -. center.y) ** 2.)

let affect_classes points centers =
  let n = Array.length points in
  let k = Array.length centers in
  let classes = Array.make n 0 in

  for i = 0 to n - 1 do
    let d_min = ref infinity in
    let c_min = ref (-1) in
    for c = 0 to k - 1 do
      let d = distance_to_center points.(i) centers.(c) in
      if d < !d_min then begin
        d_min := d;
        c_min := c
      end 
    done;
    classes.(i) <- !c_min
  done;
  classes

let minimal_distance points i centers =
  let rec aux min_d = function
    | [] -> min_d
    | c :: cs -> aux (min min_d (distance_to_center points.(i) c)) cs
  in aux infinity centers

let get_candidate points is_center centers =
  let max_d = ref 0. in
  let max_i = ref 0 in
  for i = 0 to Array.length points - 1 do
    if not is_center.(i) then begin
      let d = minimal_distance points i centers in
      if d > !max_d then begin
        max_d := d;
        max_i := i
      end
    end
  done;
  !max_i

(* k-centers 2-approximation algorithm *)
let kcenters points k _ _ =
  let n = Array.length points in
  let is_center = Array.make n false in
  let rec aux nb centers =
    if nb = k then affect_classes points (Array.of_list centers)
    else
      let c = get_candidate points is_center centers in
      is_center.(c) <- true;
      aux (nb + 1) (points.(c) :: centers)
  in
  let first = Random.int n in
  is_center.(first) <- true;
  aux 1 [points.(first)], k
  