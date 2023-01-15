(*
       _________  ___  ________  _______      
      |\___   ___\\  \|\   __  \|\  ___ \     
      \|___ \  \_\ \  \ \  \|\  \ \   __/|    
           \ \  \ \ \  \ \   ____\ \  \_|/__  
            \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
             \ \__\ \ \__\ \__\    \ \_______\
              \|__|  \|__|\|__|     \|_______|
              
                Utilitary functions
*)

let distance (x, y) (x', y') = sqrt ((x' -. x) ** 2. +. (y' -. y) ** 2.)

let open_points filename =
  let pts = ref [] in
  let n = ref 0 in
  let stream = open_in filename in
  try
    while true do
      let next = input_line stream in
      Scanf.sscanf next "%f %f" (fun x y ->
        Printf.printf "%f %f\n" x y;
        pts := (x/.100000., y/.1000000.) :: !pts;
        incr n
      )
    done;
    [], 0
  with
  | End_of_file -> !pts, !n

