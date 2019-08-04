open Core
open Hack_parallel

module Time = Core_kernel.Time_ns.Span
module Json = Yojson.Safe

let debug = Option.is_some (Sys.getenv "DEBUG_OCAML_LSIF")
let parallel = true

let i : int ref = ref 1

let fresh () =
  let id = !i in
  i := !i + 1;
  id

(* skip or continue directory descent *)
type 'a next =
  | Skip of 'a
  | Continue of 'a

let fold_directory root ~init ~f =
  let rec aux acc absolute_path depth =
    if Sys.is_file absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:true with
      | Continue acc
      | Skip acc -> acc
    else if Sys.is_directory absolute_path = `Yes then
      match f acc ~depth ~absolute_path ~is_file:false with
      | Skip acc -> acc
      | Continue acc ->
        Sys.ls_dir absolute_path
        |> List.fold ~init:acc ~f:(fun acc subdir ->
            aux acc (Filename.concat absolute_path subdir) (depth + 1))
    else
      acc
  in
  aux init root (-1)

module Export = struct
  type tool_info =
    { name : string
    ; version : string
    }
  [@@deriving yojson]

  type content =
    { language : string
    ; value : string
    }
  [@@deriving yojson]

  type hover =
    { contents : content list
    }
  [@@deriving yojson]

  type location =
    { line : int
    ; character : int
    }
  [@@deriving yojson]

  type result =
    | Hover of hover

  let result_to_yojson = function
    | Hover contents -> hover_to_yojson contents

  let result_of_yojson _ = assert false

  type entry =
    { id : string
    ; entry_type : string [@key "type"]
    ; label : string
    ; result : result option
          [@default None]
    ; start : location option
          [@default None]
    ; end_ : location option
          [@key "end"]
          [@default None]
    ; version : string option
          [@default None]
    ; project_root : string option
          [@key "projectRoot"]
          [@default None]
    ; position_encoding : string option
          [@key "positionEncoding"]
          [@default None]
    ; tool_info : tool_info option
          [@key "toolInfo"]
          [@default None]
    ; kind : string option
          [@default None]
    ; uri : string option
          [@default None]
    ; language_id : string option
          [@key "languageId"]
          [@default None]
    ; contents : string option
          [@default None]
    ; out_v : string option
          [@key "outV"]
          [@default None]
    ; in_v : string option
          [@key "inV"]
          [@default None]
    ; out_vs : string list option
          [@key "outVs"]
          [@default None]
    ; in_vs : string list option
          [@key "inVs"]
          [@default None]
    }
  [@@deriving yojson]

  let default =
    { id = "-1"
    ; entry_type = ""
    ; label = ""
    ; result = None
    ; start = None
    ; end_ = None
    ; version = None
    ; project_root = None
    ; position_encoding = None
    ; tool_info = None
    ; kind = None
    ; uri = None
    ; language_id = None
    ; contents = None
    ; out_v = None
    ; in_v = None
    ; out_vs = None
    ; in_vs = None
    }

  module Vertex = struct
    let range start_line start_character end_line end_character =
      { default with
        entry_type = "vertex"
      ; label = "range"
      ; start =
          Some
            { line = start_line
            ; character = start_character
            }
      ; end_ =
          Some
            { line = end_line
            ; character = end_character
            }
      }

    let hover_result value =
      { default with
        entry_type = "vertex"
      ; label = "hoverResult"
      ; result =
          Some
            (Hover
               { contents = [
                     { language = "OCaml"
                     ; value
                     }
                   ]
               })
      }

    let definition_result =
      { default with
        entry_type = "vertex"
      ; label = "definitionResult"
      ; result = None
      }

    let result_set () =
      { default with
        entry_type = "vertex"
      ; label = "resultSet"
      ; result = None
      }

  end
end

(** Merlin responses. *)
module Import = struct
  type location =
    { line : int
    ; col : int
    }
  [@@deriving of_yojson]

  type type_info =
    { start: location
    ; end_: location [@key "end"]
    ; type_ : string [@key "type"]
    }
  [@@deriving of_yojson]

  type definition_content =
    { file : string
    ; pos : location
    }
  [@@deriving of_yojson]

  type definition_info =
    { start: location
    ; end_: location [@key "end"]
    ; definition : definition_content
    }
  [@@deriving of_yojson]

  type t =
    | Type_info of type_info
    | Definition of definition_info
end

(** Intermediate type so that edges and IDs can be connected after parallel vertex generation. *)
type hover_result_vertices =
  { result_set_vertex : Export.entry
  ; range_vertex : Export.entry
  ; type_info_vertex : Export.entry
  }

type filepath_hover_results =
  { filepath : string
  ; hovers : hover_result_vertices list
  }

let connect ?out_v ?in_v ?in_vs ~label () =
  if Option.is_some in_v then
    { Export.default with
      id = Int.to_string (fresh ())
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_v
    }
  else if Option.is_some in_vs then
    { Export.default with
      id = Int.to_string (fresh ())
    ; entry_type = "edge"
    ; label
    ; out_v
    ; in_vs
    }
  else
    failwith "Do not call with both in_v and in_vs"

let read_with_timeout read_from_channels =
  let read_from_fds = List.map ~f:Unix.descr_of_in_channel read_from_channels in
  let read_from_channels =
    Unix.select
      ~restart:true
      ~read:read_from_fds
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 1))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> read)
    |> List.map ~f:Unix.in_channel_of_descr
  in
  List.map read_from_channels ~f:In_channel.input_all
  |> String.concat ~sep:"\n"

let read_source_from_stdin args source =
  let Unix.Process_info.{ stdin; stdout; stderr; pid } =
    Unix.create_process ~prog:"/Users/rvt/merlin/ocamlmerlin" ~args
  in
  let stdin = Unix.out_channel_of_descr stdin in
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let result = read_with_timeout [stdout] in
  In_channel.close stdout;
  In_channel.close stderr;
  Out_channel.close stdin;
  let _finished = Unix.waitpid pid in
  result

let lookup_dot_merlin filename =
  let dot_merlin_path = Filename.dirname filename ^/ ".merlin" in
  if Sys.is_file dot_merlin_path = `Yes then
    begin
      if debug then Format.printf "Merlin: %s@." dot_merlin_path;
      Some dot_merlin_path
    end
  else
    begin
      if debug then Format.printf "NO MERLIN: %s@." dot_merlin_path;
      None
    end

let call_merlin ~filename ~source ~dot_merlin =
  let args =
    [ "server"
    ; "lsif"
    ; filename
    ] @ dot_merlin
  in
  read_source_from_stdin args source

let to_lsif merlin_results : hover_result_vertices list =
  let open Export in
  let open Import in
  let open Option in
  List.fold merlin_results ~init:[] ~f:(fun acc result ->
      if debug then Format.printf "Merlin result: %s@." result;
      let json = Json.from_string result in
      let result =
        match type_info_of_yojson json with
        | Ok t -> Some (Type_info t)
        | Error _ ->
          match definition_info_of_yojson json with
          | Ok d -> Some (Definition d)
          | Error _ ->
            if debug then Format.eprintf "Ignoring other json: %s@." @@ Json.pretty_to_string json;
            None
      in
      let exported =
        result >>= function
        | Type_info { start; end_; type_ } ->
          let result_set_vertex = Vertex.result_set () in
          let range_vertex = Vertex.range (start.line - 1) start.col (end_.line - 1) end_.col in
          let type_info_vertex = Vertex.hover_result type_ in
          if debug then Format.printf "XXX@.";
          return { result_set_vertex; range_vertex; type_info_vertex }
        | Definition { start = _; end_ = _; definition = _ } ->
          None
      in
      match exported with
      | Some result -> result::acc
      | None -> acc)
  |> List.rev

let process_filepath filename =
  if debug then Format.printf "File: %s@." filename;
  let dot_merlin =
    match lookup_dot_merlin filename with
    | Some dot_merlin -> ["-dot-merlin"; dot_merlin]
    | None -> []
  in
  let source = In_channel.read_all filename in
  let merlin_result = call_merlin ~filename ~source ~dot_merlin in
  let merlin_result = String.split_lines merlin_result in
  to_lsif merlin_result

let header host root =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "metaData"
  ; version = Some "0.4.0"
  ; project_root = Some ("file:///"^host^/root)
  ; tool_info = Some { name = "lsif-ocaml"; version = "0.1.0" }
  ; position_encoding = Some "utf-16"
  }

let project () =
  { Export.default with
    id = Int.to_string (fresh ())
  ; entry_type = "vertex"
  ; label = "project"
  ; kind = Some "OCaml"
  }

let document host project_root relative_filepath absolute_filepath =
  let _contents_base64 =
    In_channel.read_all absolute_filepath
    |> Base64.Websafe.encode
  in
  { Export.default with
    entry_type = "vertex"
  ; label = "document"
  ; uri = Some ("file:///"^host^/project_root^/relative_filepath)
  ; language_id = Some "OCaml"
  (* FIXME *)
  ; contents = None
  }

let connect_ranges results document_id =
  let open Export in
  let in_vs = List.filter_map results ~f:(function
      | { id; label = "range"; _ } -> Some id
      | _ -> None)
  in
  connect ~out_v:document_id ~in_vs ~label:"contains" ()

let paths root =
  let f acc ~depth:_ ~absolute_path ~is_file =
    let is_ml_or_re_file =
      if is_file then
        [".ml"; ".mli" (*; ".re"; ".rei" *) ]
        |> List.exists ~f:(fun suffix -> String.is_suffix ~suffix absolute_path)
      else
        false
    in
    if is_ml_or_re_file then
      Continue (absolute_path::acc)
    else if Filename.basename absolute_path = "_build" then
      (* Don't descend into the _build directory. *)
      Skip acc
    else
      Continue acc
  in
  fold_directory root ~init:[] ~f

let print =
  Fn.compose
    Json.to_string
    Export.entry_to_yojson

(* scheme:
   file:///<host>/<project_root>/prefx-stripped-from-local-absolute-root
*)
type flags =
  { host : string (* e.g., github.com. *)
  ; project_root : string (* under the host, e.g., github.com/project/root *)
  ; local_absolute_root : string (* absolute path of local root or file to index *)
  ; strip_prefix : string (* the prefix to strip from the absolute root *)
  (* ; type_info_only : bool *)
  (* ; include_base64 : bool *)
  (* ; number_of_workers : int *)
  }

let () =
  Scheduler.Daemon.check_entry_point ();
  match Sys.argv |> Array.to_list with
  | _ :: local_absolute_root :: strip_prefix :: host :: project_root :: _ ->
    let n = "1" in (* FIXME *)
    let number_of_workers = if parallel then Int.of_string n else 1 in
    let scheduler = Scheduler.create ~number_of_workers () in
    let paths = paths local_absolute_root in
    let header = header host project_root in
    let project = project () in
    Format.printf "%s@." @@ print header;
    Format.printf "%s@." @@ print project;
    (* Get type information in parallel. *)
    let results =
      List.map paths ~f:(fun filepath -> { filepath; hovers = process_filepath filepath })
    in
(*
    let results =
      Scheduler.map_reduce
        scheduler
        paths
        ~init:[]
        ~map:(fun all_document_results document_paths ->
            let documents_result =
              List.map document_paths ~f:(fun document_path ->
                  { filepath = document_path
                  ; hovers = process_filepath project.id document_path
                  })
            in
            documents_result@all_document_results)
        ~reduce:(@)
    in
   *)
    (* Generate IDs and connect vertices sequentially. *)
    List.iter results ~f:(fun { filepath = absolute_filepath; hovers } ->
        let relative_filepath = String.chop_prefix_exn absolute_filepath ~prefix:strip_prefix in
        let document = document host project_root relative_filepath absolute_filepath in
        let document = { document with id = Int.to_string (fresh ()) } in
        Format.printf "%s@." @@ print document;
        let document_in_project_edge =
          connect ~out_v:project.id ~in_vs:[document.id] ~label:"contains" ()
        in
        Format.printf "%s@." @@ print document_in_project_edge;
        let hovers =
          List.concat_map hovers ~f:(fun { result_set_vertex; range_vertex; type_info_vertex } ->
              let result_set_vertex = { result_set_vertex with id = Int.to_string (fresh ()) } in
              let range_vertex = { range_vertex with id = Int.to_string (fresh ()) } in
              (* Connect range (outV) to resultSet (inV). *)
              let result_set_edge =
                connect ~out_v:range_vertex.id ~in_v:result_set_vertex.id ~label:"next" ()
              in
              let type_info_vertex = { type_info_vertex with id = Int.to_string (fresh ()) } in
              (* Connect resultSet (outV) to hoverResult (inV). *)
              let hover_edge =
                connect ~in_v:type_info_vertex.id ~out_v:result_set_vertex.id ~label:"textDocument/hover" ()
              in
              [result_set_vertex; range_vertex; result_set_edge; type_info_vertex; hover_edge]
            )
        in
        List.iter hovers ~f:(fun entry -> Format.printf "%s@." @@ print entry);
        let edges_entry = connect_ranges hovers document.id in
        Format.printf "%s@." @@ print edges_entry);
    begin
      try Scheduler.destroy scheduler
      with Unix.Unix_error (_,"kill",_) -> ()
    end
  | _ ->
    Format.eprintf "local_root(/Users/merlin/src) strip_prefix(/Users/.../) host(github.com) project(ocaml/merlin)"
