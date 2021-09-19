open Cfg
let blocks = createBlocks instrs
let (blockMap,names) = createBlockMap blocks
let cfg = createCfg blockMap names
(*
let () = printStringList names
let () = printBlockMap blockMap
*)

let () = print_string "digraph main {\n"
let () = printStringList names
let () = printCfg cfg
let () = print_string "  }\n"
