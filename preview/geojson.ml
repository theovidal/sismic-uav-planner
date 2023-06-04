let output_geojson input_file output_file =
  let output = open_out output_file in
  Printf.fprintf output "{ \"name\": \"Parcours de drones\", \"type\": \"FeatureCollection\", \"features\": [\n";

  let print_point (x, y) =
    Printf.fprintf output "{
        \"type\": \"Feature\",
        \"properties\": {},
        \"geometry\": {
          \"type\": \"Point\",
          \"coordinates\": [%f, %f]
        }
      },
      " x y in

  let perms, nb_zones, _nb_points = Utils.read_permutation input_file in

  List.iteri (fun i pts ->
    let line_string = Printf.sprintf "{
        \"type\": \"Feature\",
        \"properties\": {
          \"class\": %d
        },
        \"geometry\": {
          \"type\": \"LineString\",
          \"coordinates\": [
    " i 
    |> ref in
    let n = Array.length pts in
    let add_to_line j is_end =
      let (x, y) = pts.(j) in
      line_string := Printf.sprintf "%s[%f, %f]%c\n" !line_string x y (if is_end then ' ' else ',')
    in

    for j = 0 to n - 2 do
      print_point pts.(j);
      add_to_line j false;
    done;
    print_point pts.(n - 1);
    add_to_line (n - 1) false;
    add_to_line 0 true;

    Printf.fprintf output "%s
          ]
        }
      }%c\n" !line_string (if i = nb_zones - 1 then ' ' else ',');
  ) perms;
  Printf.fprintf output "]}";
  close_out output;
