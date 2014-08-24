open Std

module Sexp = struct
  type t =
    | Cons   of t * t
    | Sym    of string
    | String of string
    | Int    of int
    | Float  of float
  let nil = Sym "nil"

  let rec sexp_of_list = function
    | [] -> nil
    | a :: tl -> Cons (a, sexp_of_list tl)

  let rec tell_sexp tell = function
    | Cons (a,b) ->
      tell "(";
      tell_sexp tell a;
      tell_cons tell b
    | Sym s    -> tell s
    | String s -> tell ("\"" ^ String.escaped s ^ "\"")
    | Int i    -> tell (string_of_int i)
    | Float f  -> tell (string_of_float f)

  and tell_cons tell = function
    | Sym "nil" -> tell ")"
    | Cons (a,b) ->
      tell " ";
      tell_sexp tell a;
      tell_cons tell b
    | sexp ->
      tell " . ";
      tell_sexp tell sexp;
      tell ")"

  let is_alpha c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')

  let is_num c =
      (c >= '0' && c <= '9')

  let is_alphanum c = is_alpha c || is_num c

  let read_sexp getch =
    let buf = Buffer.create 10 in
    let rec read_sexp getch = function
      | ' ' | '\t' | '\n' ->
        read_sexp getch (getch ())

      | c when is_num c ->
        read_num getch c

      | '\'' | ':' | '_' as c -> read_sym getch (Some c)
      | c when is_alpha c -> read_sym getch (Some c)

      | '"' ->
        read_string getch
      | '\000' -> raise End_of_file
      | '(' ->
        let lhs, next = read_sexp getch (getch ()) in
        read_cons getch (fun rhs -> Cons (lhs, rhs)) next
      | _ -> failwith "Invalid parse"

    and read_cons getch k next =
      match (match next with Some c -> c | None -> getch ()) with
      | ' ' | '\t' | '\n' -> read_cons getch k None
      | ')' -> k nil, None
      | '.' ->
        let rhs, next = read_sexp getch (getch ()) in
        let rec aux = function
          | ')' -> k rhs
          | ' ' | '\t' | '\n' -> aux (getch ())
          | _ -> failwith "Invalid parse"
        in
        begin match next with
          | Some c -> aux c
          | None -> aux (getch ())
        end, None
      | c ->
        let cell, next = read_sexp getch c in
        read_cons getch (fun rhs -> k (Cons (cell, rhs))) next

    and read_num getch c =
      Buffer.clear buf;
      Buffer.add_char buf c;
      let is_float = ref false in
      let rec aux () =
        match getch () with
        | c when c >= '0' && c <= '9' ->
          Buffer.add_char buf c; aux ()
        | '.' | 'e' | 'E' as c ->
          is_float := true;
          Buffer.add_char buf c; aux ()
        | c ->
          let s = Buffer.contents buf in
          (if !is_float
           then Float (float_of_string s)
           else Int (int_of_string s)),
          Some c
      in
      aux ()

    and read_string getch =
      Buffer.clear buf;
      let rec aux () =
        match getch () with
        | '\000' -> failwith "Unterminated string"
        | '\\' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf (getch ());
          aux ()
        | '"' ->
          String (Scanf.unescaped (Buffer.contents buf)), None
        | c ->
          Buffer.add_char buf c;
          aux ()
      in
      aux ()

    and read_sym getch next =
      Buffer.clear buf;
      let rec aux next =
        match (match next with Some c -> c | None -> getch ()) with
        | ('\'' | '-' | ':' | '_') as c ->
          Buffer.add_char buf c;
          aux None
        | c when is_alphanum c ->
          Buffer.add_char buf c;
          aux None
        | c -> Sym (Buffer.contents buf), Some c
      in
      aux next
    in
    read_sexp getch (getch ())

  let to_buf sexp buf =
    tell_sexp (Buffer.add_string buf) sexp

  let to_string sexp =
    let buf = Buffer.create 100 in
    to_buf sexp buf;
    Buffer.contents buf

  let getch_of_substring str pos len =
    let len = pos + len in
    if pos < 0 || len > String.length str then
      invalid_arg "Sexp.getch_of_substring";
    let pos = ref pos in
    let getch () =
      if !pos < len then
        let r = str.[!pos] in
        incr pos;
        r
      else '\000'
    in
    getch

  let getch_of_string str =
    getch_of_substring str 0 (String.length str)

  let of_string str =
    fst (read_sexp (getch_of_string str))

  let of_channel ic =
    let fd = Unix.descr_of_in_channel ic in
    let getch = ref (fun () -> '\000') in
    let rest = ref None in
    let buffer = Bytes.create 1024 in
    let getch () =
      match !rest with
      | Some r ->
        rest := None;
        r
      | None ->
        match !getch () with
        | '\000' ->
          let read = Unix.read fd buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring (Bytes.to_string buffer) 0 read;
              !getch ()
            end
        | c -> c
    in
    fun () ->
      try
        let sexp, rest' = read_sexp getch in
        rest := rest';
        Some sexp
      with End_of_file -> None
end

let rec sexp_of_json =
  let open Sexp in
  let assoc_item (a,b) = Cons (Sym a, sexp_of_json b) in
  function
  | `Null       -> Sym "null"
  | `Int i      -> Int i
  | `Float f    -> Float f
  | `String s   -> String s
  | `Bool true  -> Sym "true"
  | `Bool false -> Sym "false"
  | `Assoc lst  -> Cons (Cons (Sym "assoc", Sym "nil"), sexp_of_list (List.map assoc_item lst))
  | `List lst   -> sexp_of_list (List.map sexp_of_json lst)

let rec json_of_sexp =
  let open Sexp in
  let fail msg sexp =
    failwith (msg ^ ", got: \n" ^ Sexp.to_string sexp)
  in
  let rec assoc_item = function
    | Cons (Cons (Sym a, b), c) -> (a, json_of_sexp b) :: assoc_item c
    | Sym "nil" -> []
    | sexp -> fail "expecting association (key . value)" sexp
  in
  let rec list_items = function
    | Sym "nil" -> []
    | Cons (hd, tl) -> json_of_sexp hd :: list_items tl
    | sexp -> fail "expecting list" sexp
  in
  function
  | Sym "null"  -> `Null
  | Sym "true"  -> `Bool true
  | Sym "false" -> `Bool false
  | Int i    -> `Int i
  | Float f  -> `Float f
  | String s -> `String s
  | Cons (Cons (Sym "assoc", Sym "nil"), assocs) ->
    `Assoc (assoc_item assocs)
  | Sym "nil" -> `List []
  | Cons (hd, tl) -> `List (json_of_sexp hd :: list_items tl)
  | Sym s -> `String s

let sexp_make ~input ~output =
  let input' = Sexp.of_channel input in
  let input' = Stream.from (fun _ -> Option.map json_of_sexp (input' ())) in
  let output' = Unix.descr_of_out_channel output in
  let buf = Buffer.create 8192 in
  let output json =
    let sexp = sexp_of_json json in
    Sexp.to_buf sexp buf;
    Buffer.add_char buf '\n';
    let contents = Buffer.contents buf in
    let rec write_contents n l =
      if l > 0 then
        let l' = Unix.write_substring output' contents n l in
        if l' > 0 then
          write_contents (n + l') (l - l')
    in
    write_contents 0 (String.length contents);
    if Buffer.length buf > 100_000
    then Buffer.reset buf
    else Buffer.clear buf
  in
  (input', output : IO.low_io)

let () = IO.register_protocol
      ~name:"sexp"
      ~desc:"Simple encoding of json over sexpr"
      sexp_make