(*
       _________  ___  ________  _______      
      |\___   ___\\  \|\   __  \|\  ___ \     
      \|___ \  \_\ \  \ \  \|\  \ \   __/|    
           \ \  \ \ \  \ \   ____\ \  \_|/__  
            \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
             \ \__\ \ \__\ \__\    \ \_______\
              \|__|  \|__|\|__|     \|_______|
              
                  Matrices module
*)

let product a b =
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

let y_rotation theta = 
  [|
    [| cos theta; 0.; sin theta |];
    [| 0.; 1.; 0. |];
    [| -1. *. sin theta; 0.; cos theta |];
  |]

let z_rotation phi =
  [|
    [| cos phi; -1. *. sin phi; 0. |];
    [| sin phi; cos phi; 0. |];
    [| 0.; 0.; 1. |]
  |]

let from_coords (x, y, z) = [| [|x|]; [|y|]; [|z|] |]

let to_coords mat = mat.(0).(0), mat.(1).(0), mat.(2).(0)
