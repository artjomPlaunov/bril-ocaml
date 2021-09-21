open Core
module StringSet = Set.Make(String)

(* Just doing one function for now. *)
let prog  = Bril.getProg Core.In_channel.stdin
let instrs =  match prog.funcs with
              | []    ->  []
              | h::_  ->  h.instrs

(* Add a list of instruction arguments to the set. *)
let addUsed set args = 
  let rec aux set = function
    | []    ->  set 
    | h::t  ->  aux (Set.add set h) t 
  in
  match args with 
    | None      ->  set
    | Some lst  ->  aux set lst

(* Get a set of variables which are used at some point of the function.
   I.e., the variable shows up in an argument in an instrusction. *)
let getUsedSet instrs = 
  let open Bril in
  let set = StringSet.empty in 
  let rec aux set = function 
    | []    ->  set
    | h::t  ->  aux (addUsed set h.args) t
  in 
  aux set instrs

let dce_local_aux = 
let usedSet = getUsedSet instrs in



  in
  aux 

