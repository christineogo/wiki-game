open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  (* plan: get all links, filter to ones that sart with en.wikipedia.org, filter to no namespaces *)
  let open Soup in
  let is_wiki link = String.sub link ~pos:0 ~len:5 in
  parse contents
  $$ "a"
  |> to_list
  |> List.filter ~f:(fun a -> String.length (R.attribute "href" a) > 4)
  |> List.map ~f:(fun a ->
    match is_wiki (R.attribute "href" a) with
    | "/wiki" -> R.attribute "href" a
    | _ -> " ")
  |> List.filter ~f:(fun a -> match a with " " -> false | _ -> true)
  |> List.filter ~f:(fun a ->
    match Wikipedia_namespace.namespace a with None -> true | _ -> false)
  |> List.dedup_and_sort ~compare:String.compare
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    All feliforms, such as domestic cats, big cats, hyenas, mongooses, civets
    Almost all caniforms, such as the dogs, wolves, foxes, ferrets, seals and walruses
    All cetaceans, such as dolphins, whales and porpoises
    All bats except fruitbats
    The carnivorous marsupials, such as the Tasmania devil
    All birds of prey, such as hawks, eagles, falcons and owls
    All vultures, both old world and new
    Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (output_file : File_path.t);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
