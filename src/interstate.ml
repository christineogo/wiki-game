open! Core
module City = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let of_string s = List.tl_exn (String.split s ~on:',')
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  (*
     [seattle; portland; LA]
     seatle + portland
     portland + LA

     nested list.map

     list.map cities ~f:( fun city_a->
     list.map cities ~f:(fun city_b))
  *)
  (* let of_file input_file =
    let cities = In_channel.read_lines (File_path.to_string input_file) in
    let routes =
      List.concat_map cities ~f:(fun line -> Connection.of_string line)
    in
    let connections =
  List.concat_map routes ~f:(fun route ->
    List.concat_map (List.zip_exn route (List.tl_exn route)) ~f:(fun (a, b) ->
      [a, b; b, a])))
    in
    Connection.Set.of_list connections
  ;;
end *)

  let of_file input_file =
    let lines = In_channel.read_lines (File_path.to_string input_file) in
    (* Convert each line like "Chicago,Milwaukee,Madison" into ["Chicago"; "Milwaukee"; "Madison"] *)
    let routes = List.map lines ~f:(fun line -> Connection.of_string line) in
    (* For each route, connect adjacent city pairs bidirectionally *)
    let connections =
      List.concat_map routes ~f:(fun route ->
        match route with
        | [] | [ _ ] -> [] (* skip empty or single-city routes *)
        | _ ->
          List.concat_mapi route ~f:(fun i city_a ->
            if i + 1 < List.length route
            then (
              let city_b = List.nth_exn route (i + 1) in
              [ city_a, city_b; city_b, city_a ])
            else []))
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2) ->
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
