open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  (* have more descriptive variable name *)
  let contains_notable_class_name class_list =
    (* use String.equals function instead of matching *)
    List.exists class_list ~f:(fun class_element ->
      match class_element with
      | "ipc-primary-image-list-card__title" -> true
      | _ -> false)
  in
  let open Soup in
  parse contents
  $$ "a"
  |> to_list
  |> List.filter ~f:(fun a -> contains_notable_class_name (classes a))
  |> List.map ~f:(fun a -> texts a |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "get_credits" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/imdb"))
      ~resource:"Kerry"
  in
  List.iter (get_credits contents) ~f:print_endline;
  [%expect
    {|
    Scandal
    Ray
    Save the Last Dance
    Django Unchained
    |}]
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
