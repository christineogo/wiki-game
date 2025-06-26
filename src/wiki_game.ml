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

module Article = String

module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let _of_string s =
      match String.split s ~on:',' with
      | [ x; y ] -> Some (Article.of_string x, Article.of_string y)
      | _ -> None
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]
end

module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* type t = {
  name: string;
  url: string;
} *)
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
    /wiki/Animal
    /wiki/Caniformia
    /wiki/Feliformia
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

(* let add_connection (connections : (string * string) list) (node1: string) (node2: string) =
   List.append connections [(node1,node2)] in
   print_endline("added connections"); *)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let contents article =
    File_fetcher.fetch_exn how_to_fetch ~resource:article
  in
  let find_neighbors article = get_linked_articles (contents article) in
  (* let connections = [] in *)
  (* BFS *)
  let visited = String.Hash_set.create () in
  let bfs start_node =
    let to_visit = Queue.create () in
    Queue.enqueue to_visit start_node;
    let rec traverse _parent_node depth =
      if equal depth max_depth
      then []
      else (
        match Queue.dequeue to_visit with
        | None -> []
        | Some current_node ->
          if not (Hash_set.mem visited current_node)
          then (
            Hash_set.add visited current_node;
            let adjacent_nodes = find_neighbors current_node in
            List.iter adjacent_nodes ~f:(fun next_node ->
              Queue.enqueue to_visit next_node);
            List.concat_map adjacent_nodes ~f:(fun next_node ->
              List.append
                (traverse next_node (depth + 1))
                [ current_node, next_node ]))
          else [])
    in
    traverse start_node 0
    (* in *)
    (* traverse start_node *)
  in
  let total_connections = bfs origin in
  let network = Network.Connection.Set.of_list total_connections in
  let graph = G.create () in
  Set.iter network ~f:(fun (article1, article2) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
       they don't already exist. *)
    G.add_edge graph article1 article2);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
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
