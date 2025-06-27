open! Core

(* let find_neighbors =
   function that puts string to coordinates and returns the LRUD coordintate
   if it isn't a hedge*)

let parse_point (s : string) : int * int =
  match String.split ~on:',' s with
  | [ row; col ] -> Int.of_string row, Int.of_string col
  | _ -> failwith "something went wrong with your point"
;;

let find_neighbors (maze : string list) (point : string) : string list =
  let row, col = parse_point point in
  let height = List.length maze in
  let width = String.length (List.hd_exn maze) in
  let candidates =
    [ row - 1, col; row + 1, col; row, col - 1; row, col + 1 ]
  in
  List.filter_map candidates ~f:(fun (r, c) ->
    if r >= 0 && r < height && c >= 0 && c < width
    then Some (Printf.sprintf "%d,%d" r c)
    else None)
;;

let coordinate_value (maze : string list) (point : string) : char =
  let row, col = parse_point point in
  String.get (List.nth_exn maze row) col
;;

let dfs graph start_node =
  let visited = String.Hash_set.create () in
  let rec traverse current =
    if Hash_set.mem visited current
    then None
    else (
      Hash_set.add visited current;
      match coordinate_value graph current with
      | 'E' -> Some [ current ]
      | '#' -> None
      | _ ->
        let neighbors = find_neighbors graph current in
        List.find_map neighbors ~f:(fun next_node ->
          match traverse next_node with
          | Some path -> Some (current :: path)
          | None -> None))
  in
  match traverse start_node with Some path -> path | None -> []
;;

(* let dfs graph start_node =
  let visited = String.Hash_set.create () in
  let rec traverse start =
    match coordinate_value start with 
    | "E" -> Some []
    | "#" -> None
    | _ -> match (List.find_map (find_neighbors start) ~f(fun: next_node -> 
      match traverse next_node with 
      | Some list -> Some list
      |None -> None
      )) with 
      | None -> None 
      |Some node -> Some  start @@ (match traverse next_node with |Some list -> list) *)

(* let final_path = [] in *)
(* let visited = String.Hash_set.create () in
   let to_visit = Stack.create () in
   Stack.push to_visit start_node;
   let rec traverse _node path =
   match Stack.pop to_visit with
   | Some "E" -> path
   | None -> None
   | Some current_node ->
   if not (Hash_set.mem visited current_node)
   then (
   Hash_set.add visited current_node;
   printf "%s\n" current_node;
   let adjacent_nodes = Map.find_exn graph current_node in
   List.iter adjacent_nodes ~f:(fun next_node ->
   Stack.push to_visit next_node;
   );
   List.concat_map adjacent_nodes ~f:(fun next_node->
    match (traverse next_node path @ [next_node]) with
    |[] -> []
    |_ -> path @ [next_node]
    )
   )
   else ([])
   in
   traverse start_node [] in
   final_path *)

(* let dfs start_node =
   let visited = String.Hash_set.create () in
   let to_visit = Stack.create () in
   Stack.push to_visit start_node;
   let rec traverse _parent_node path_to =
   match Stack.pop_exn to_visit with
   | None -> []
   | Some current_node ->
   if not (Hash_set.mem visited current_node)
   then (
   Hash_set.add visited current_node;
   let adjacent_nodes = find_neighbors current_node in
   List.iter adjacent_nodes ~f:(fun next_node ->
   Stack.push to_visit next_node);
   List.concat_map adjacent_nodes ~f:(fun next_node ->
   List.append
   (traverse next_node (depth + 1))
   [ current_node, next_node ]))
   else []
   in
   traverse start_node 0 *)

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    (* ~summary:
       "Play wiki game by finding a link between the origin and destination \
       pages"
       [%map_open let how_to_fetch = File_fetcher.How_to_fetch.param and origin = flag "origin" (required string) ~doc:" the starting page" and destination = flag "destination" (required string) ~doc:" the destination page" and max_depth = flag "max-depth" (optional_with_default 10 int) ~doc:"INT maximum length of path to search for (default 10)" in fun () -> match find_path ~max_depth ~origin ~destination ~how_to_fetch () with | None -> print_endline "No path found!" | Some trace -> List.iter trace ~f:print_endline] *)
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let lines = In_channel.read_lines (File_path.to_string input_file) in
        let solution_array = dfs lines "1,0" in
        List.iter solution_array ~f:print_endline]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
