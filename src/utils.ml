(*
       _________  ___  ________  _______      
      |\___   ___\\  \|\   __  \|\  ___ \     
      \|___ \  \_\ \  \ \  \|\  \ \   __/|    
           \ \  \ \ \  \ \   ____\ \  \_|/__  
            \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
             \ \__\ \ \__\ \__\    \ \_______\
              \|__|  \|__|\|__|     \|_______|
              
              Module de fonctions utilitaires
*)

let distance (x, y) (x', y') = sqrt ((x' -. x) ** 2. +. (y' -. y) ** 2.)

let open_points filename =
  let pts = ref [] in
  let stream = open_in filename in
  try
    let i = ref 0 in
    while true do
      let next = input_line stream in
      Scanf.sscanf next "%f %f" (fun x y ->
        pts := (x, y) :: !pts;
        incr i
      )
    done;
    []
  with
  | End_of_file -> !pts

