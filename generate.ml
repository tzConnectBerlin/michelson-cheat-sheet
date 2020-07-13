#require "core";;
open Core_kernel;;
open Printf;;
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

let handle_instruction_detail = fun instr ->
  let op = json_str_element "op" instr in
  let ty = json_list_element "ty" instr in
  sprintf {|
  \textbf{%s} %s|} op (handle_ty ty)
;;

let handle_instruction = fun instr ->
  let op = json_str_element "op" instr in
  let description = json_str_element "documentation_short" instr in
  sprintf {|\command{%s}{%s} |} op description;;

let instructions = json_list_element "instructions" json;;

let header = sprintf{|
  \noindent\sectiontitle{Instructions (detail)}
  \raggedright

  \small\noindent|};;

let sorted = List.sort instructions (fun x y ->
  String.compare (json_str_element "op" x) (json_str_element "op" y));;

let instruction_details_lines = List.map sorted (fun x -> handle_instruction_detail x);;
let instructions_lines = List.map sorted (fun x -> handle_instruction x);;

let instructions_detail = "instructions-detail.tex";;

let oc = open_out instructions_detail in
fprintf oc "%s %s" header (String.concat (List.map instruction_details_lines (fun x -> sprintf "%s" x)));;

let oc = open_out "instructions.tex" in
fprintf oc "%s" (String.concat instructions_lines)
