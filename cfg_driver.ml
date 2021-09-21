open Core
open Bril

(* Just doing one function for now. *)
let prog  = Bril.getProg In_channel.stdin
let instrs =  match prog.funcs with
              | []    ->  []
              | h::_  ->  h.instrs

let blocks = Cfg.createBlocks instrs
let (blockMap,names) = Cfg.createBlockMap blocks
let cfg = Cfg.createCfg blockMap names
(*
let () = printStringList names
let () = printBlockMap blockMap
*)

let () = print_string "digraph main {\n"
let () = Cfg.printStringList names
let () = Cfg.printCfg cfg
let () = print_string "  }\n"
