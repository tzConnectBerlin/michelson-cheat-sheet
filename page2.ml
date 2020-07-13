#require "core";;
open Core_kernel;;
#require "yojson";;
open Yojson;;

exception Except of string;;

let minted = fun x -> sprintf "\\texttt{%s }" x;;

let unwrap = fun x -> match x with Some c -> c | None -> raise(Except "unwrapped None!");;

let json = unwrap (List.hd [ Yojson.Basic.from_file "michelson.json" ]);;

let json_element =
  fun x json ->
    unwrap (List.Assoc.find  ~equal:String.equal (Yojson.Basic.Util.to_assoc json) x);;

let json_str_element =
  fun x json ->
    Yojson.Basic.Util.to_string (json_element x json);;

let json_list_element =
  fun x json ->
    Yojson.Basic.Util.to_list (json_element x json);;

let handle_ty = fun ty ->
  String.concat (" " ::
                   (List.map ty (fun x ->
                     sprintf "%s|"
                       (minted (json_str_element "conclusion" x)))));;

let handle_instruction = fun instr ->
  let op = json_str_element "op" instr in
  let op_args = json_str_element "op_args" instr in
  let ty = json_list_element "ty" instr in
  printf {|
  \textbf{%s} %s|} op (handle_ty ty)
;;

let instructions = json_list_element "instructions" json;;

printf{|
  \noindent\sectiontitle{Instructions (detail)}
  \raggedright

  \small\noindent|}
let sorted = List.sort instructions (fun x y ->
  String.compare (json_str_element "op" x) (json_str_element "op" y));;

List.map sorted (fun x -> handle_instruction x);;
