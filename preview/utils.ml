let calibrate_coordinates (x, y) =
  (x *. 10. +. 800000., y *. 10. +. 6000000.)

(* Read a permutation that the path program wrote *)
let read_permutation filename =
  let file = open_in filename in
  let rec aux acc nb_zones nb_tot =
    try
      Scanf.sscanf (input_line file) "%d" (fun nb_sigma ->
          let sigma = Array.make nb_sigma (0., 0.) in
          for i = 0 to nb_sigma - 1 do
            Scanf.sscanf (input_line file) "%f %f" (fun x y -> sigma.(i) <- calibrate_coordinates (x, y))
          done;
        aux (sigma :: acc) (nb_zones + 1) (nb_tot + nb_sigma)
      )
    with End_of_file -> close_in file; acc, nb_zones, nb_tot
  in aux [] 0 0
