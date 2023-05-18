(*
         _________  ___  ________  _______      
        |\___   ___\\  \|\   __  \|\  ___ \     
        \|___ \  \_\ \  \ \  \|\  \ \   __/|    
             \ \  \ \ \  \ \   ____\ \  \_|/__  
              \ \  \ \ \  \ \  \___|\ \  \_|\ \ 
               \ \__\ \ \__\ \__\    \ \_______\
                \|__|  \|__|\|__|     \|_______|
                                        
       Tracing of paths for reconnaissance of an earthquake zone by drone
Entry point of the topography program, to parse raw data into weighted classes
                            VIDAL Théo - 962 MPI*
*)

let () =
  if Array.length Sys.argv < 3 then begin
    print_string "Missing arguments. Valid syntax: program [input] [THRESHOLD] (output=selection.csv)";
    exit (-1)
  end;
  let input_file = Sys.argv.(1) in
  let threshold = int_of_string Sys.argv.(2) in
  let output_file = if Array.length Sys.argv > 3 then Sys.argv.(3) else "selection.csv" in

  let output = open_out output_file in
  let points = 
    Utils.open_points input_file
    |> Array.map Utils.parse_point in
  points
    |> Clustering.ahc threshold
    |> Utils.gather points
    |> Array.iter (Utils.print_point output);
  close_out output
