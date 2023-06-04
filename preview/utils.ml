let calibrate_coordinates (x, y) =
  (x *. 10. +. 800000., y *. 10. +. 6000000.)

(* Read a permutation that the path program wrote *)
let read_permutation filename =
  let file = open_in filename in
  let rec aux acc nb_zones nb_tot =
    try
      Scanf.sscanf (input_line file) "%d %d" (fun zone_id nb_sigma ->
          let sigma = Array.make nb_sigma (0., 0.) in
          for i = 0 to nb_sigma - 1 do
            Scanf.sscanf (input_line file) "%f %f" (fun x y -> sigma.(i) <- calibrate_coordinates (x, y))
          done;
        aux ((zone_id, sigma) :: acc) (nb_zones + 1) (nb_tot + nb_sigma)
      )
    with End_of_file -> close_in file; acc, nb_zones, nb_tot
  in aux [] 0 0

(* Calculate the barycenter of a set of points *)
let barycenter points =
  let n = Array.length points in
  if n = 0 then (0., 0.) else
  let x = ref 0. in
  let y = ref 0. in
  for i = 0 to n - 1 do
    x := !x +. fst points.(i);
    y := !y +. snd points.(i)
  done;
  (!x /. float_of_int n, !y /. float_of_int n)
