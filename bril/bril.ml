open Core
let print_int = Stdlib.print_int [@@warning "-3"]

(* Bril instruction fields. *)
type instr = {
  dest:     string option;
  op:       string option;
  typ:      string option;
  value:    string option;
  labels:   string list option;
  label:    string option;
  args:     string list option;
}

(* Bril function is a name and a list of instructions. 
   (will include arguments soon as well. *)
type func = {
  name:     string;
  instrs:   instr list;
}

(* Bril program is a list of functions. *)
type prog = {
  funcs: func list;
}

let s_of_s s = s

(* get value binded to k in j, or None if it is a json `Nil value. *)
let getMem j k convertF stringOf =
  let open Yojson.Basic.Util in 
  let v = member k j in
    match v with 
      | `Null             ->  None
      | `Int _            ->  Some (stringOf (convertF v))
      | `String _         ->  Some (stringOf (convertF v))
      | `Bool _
      | `Assoc _
      | `List _
      | `Float _  -> None

let getListMem j k = 
  let open Yojson.Basic.Util in
  let lst = member k j in 
    match lst with
      | `Null -> None
      | _     ->  let lst = to_list lst in
                  Some (List.map lst ~f:(to_string))

let jsonToInstr j = 
  let open Yojson.Basic.Util in
  let dest = getMem j "dest" to_string s_of_s in 
  let op = getMem j "op" to_string s_of_s in
  let typ = getMem j "type" to_string s_of_s in
  let value = getMem j "value" to_int string_of_int in
  let labels = getListMem j "labels" in
  let label = getMem j "label" to_string s_of_s in
  let args = getListMem j "args" in
  {dest=dest;op=op;typ=typ;value=value;labels=labels;label=label;args=args;}

let jsonToFunc j = 
  let open Yojson.Basic.Util in
  let name = member "name" j |> to_string in
  let is = j |> member "instrs" |> to_list in
  let instrs = List.map is ~f:(jsonToInstr) in
  {name=name; instrs=instrs}

let jsonToFuncs json = 
  List.map json ~f:(jsonToFunc)

let jsonToProg j = 
  let open Yojson.Basic.Util in
  let fs = j |> member "functions" |> to_list in
  let funcs = jsonToFuncs fs in
  {funcs = funcs;}

let getProg channel = 
  let json = Yojson.Basic.from_channel channel in 
  jsonToProg json

let getProgFromString s = 
  let json = Yojson.Basic.from_string s in
  jsonToProg json

(* Custom printing utils for bril programs. *)
let printOption printF = function 
  | None    ->  print_string "Nil"
  | Some s  ->  printF s

let rec printStringList = function
  | []    ->  ()
  | h::t  ->  print_string (h ^ " ,"); printStringList t

let rec printInstrs = function
  | []    ->  ()
  | h::t  ->  print_string "  dest:"; 
              printOption print_string h.dest; 
              print_string "  op:";
              printOption print_string h.op;
              print_string "  type:";
              printOption print_string h.typ;
              print_string "  value:";
              printOption print_string h.value;
              print_string "  labels:";
              printOption printStringList h.labels;
              print_string "  label:";
              printOption print_string h.label;
              print_string "  args:";
              printOption printStringList h.args;
              print_endline " ";
              printInstrs t

let rec printFuncs = function
  | []  ->  ()
  | h::t  -> print_endline h.name ;  printInstrs h.instrs; printFuncs t

let printProg prog = 
  printFuncs prog.funcs


