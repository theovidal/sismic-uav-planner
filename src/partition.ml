(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
Tracing of paths for reconnaissance of an earthquake zone by drone
                 "Zone partitioning" part
                   VIDAL Théo - 932 MPI*
*)

(* TODO :
   - Utilitaires pour les coordonnées (matrices ou non) pour rendre le code plus clair 
*)

let matrix_product a b =
  if Array.length b == 0 || Array.length a == 0 || Array.length a.(0) <> Array.length b then
    failwith "Multiplication matricielle impossible"
  else
    let n = Array.length a in
    let p = Array.length b in
    let q = Array.length b.(0) in
    let res = Array.make_matrix n q 0. in
    for i = 0 to n - 1 do
      for j = 0 to q - 1 do
        for k = 0 to p - 1 do 
          res.(i).(j) <- res.(i).(j) +. a.(i).(k) *. b.(k).(j)
        done;
      done;
    done;
    res

let y_rotation_matrix theta = 
  [|
    [| cos theta; 0.; sin theta |];
    [| 0.; 1.; 0. |];
    [| -1. *. sin theta; 0.; cos theta |];
  |]

let z_rotation_matrix phi =
  [|
    [| cos phi; -1. *. sin phi; 0. |];
    [| sin phi; cos phi; 0. |];
    [| 0.; 0.; 1. |]
  |]

(* Do the inverse projection ℝ² -> S³ *)
let inverse_stereographic =
  List.map (fun (x, y) ->
    let norm = x *. x +. y *. y +. 1. in
    (2. *. x) /. norm, (2. *. y) /. norm, (x *. x +. y *. y -. 1.) /. norm
  )

(* Do the projection S³ -> ℝ² *)
let stereographic =
  List.map (fun (x, y, z) ->
    x /. (1. -. z), y /. (1. -. z)
  )

(* Calculate the centroid of a group of points *)
let centroid pts =
  let num_pts = ref 0 in
  let cx = ref 0. in
  let cy = ref 0. in
  let cz = ref 0. in
  List.iter (fun (x, y, z) ->
    incr num_pts;
    cx := x +. !cx;
    cy := y +. !cy;
    cz := z +. !cz
  ) pts;
  let n = float_of_int !num_pts in
  !cx/.n, !cy/.n, !cz/.n

(* Conformally map the points so that their centroid lies at the origin *)
let confort pts (cx, cy, cz) =
  (* Spheric oordinates of the centroid *)
  let theta = atan ((hypot cx cy) /. cz) in
  let phi = atan (cy /. cx) in
  let r = sqrt (cx *. cx +. cy *. cy +. cz *. cz) in

  theta, phi, r, (List.map (fun (x, y, z) ->
    (* Performing rotations to have centroid at (0, 0, r) *)
    let res = matrix_product (y_rotation_matrix (-1. *. theta))
      (matrix_product (z_rotation_matrix (-1. *. phi)) [| [|x|]; [|y|]; [|z|] |])
    in
    (* Performing dilation to have centroid at (0, 0, 0) *)
    res.(0).(0), res.(1).(0), res.(2).(0) -. r
  ) pts)


(* Revert the operations done for conforting (dilation, rotation, inverse stereographic projection) *)
let unmap pts r theta phi =
  stereographic (List.map (fun (x, y, z) ->
    let res = matrix_product (z_rotation_matrix (phi))
      (matrix_product (y_rotation_matrix (theta)) [| [|x|]; [|y|]; [|z +. r|] |])
    in res.(0).(0), res.(1).(0), res.(2).(0)
  ) pts)

let geometric_bisection points =
  let projected = inverse_stereographic points in
  let theta, phi, r, _ = confort projected (centroid projected) in

  (* To find a great circle, we take a points of the unit-sphere and move it randomly *)
  Random.self_init ();
  let rand_theta = Random.float (2. *. Float.pi) in
  let rand_phi = Random.float (2. *. Float.pi) in
  let great_circle_pt = matrix_product (matrix_product (y_rotation_matrix rand_theta) (z_rotation_matrix rand_phi)) [| [|0.|]; [|0.|]; [|1.|] |] in

  (* Center of the circle is the first element of the array ; the other point belongs to C and is the helper to draw it *)
  let circle = unmap [(0., 0., 0.); (great_circle_pt.(0).(0), great_circle_pt.(1).(0), great_circle_pt.(2).(0))] r theta phi in
  let o = List.hd circle in
  let a = List.hd (List.tl circle) in
  Printf.printf "Circle C: %.3f,%.3f  -  %.3f,%.3f\n" (fst o) (snd o) (fst a) (snd a);
  let radius = Utils.distance a o in

  (* Make the two groups: in/out of the circle *)
  let group_a = ref [] in
  let group_b = ref [] in
  List.iter (fun p ->
    if Utils.distance p o < radius then group_a := p :: !group_a
    else group_b := p :: !group_b
    ) points;
  !group_a, !group_b, a
