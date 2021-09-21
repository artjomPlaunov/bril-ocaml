open Core
let print_int = Stdlib.print_int [@@warning "-3"]

(* Basic CFG type is a block of bril instructions. *)
type block = Bril.instr list

let createBlocks instrs = 
  let open Bril in
  let rec aux res cur_block = function
  | []    ->  if List.length cur_block > 0 then res@[cur_block] else res
  | h::t  ->  match h.op with 
              | None  ->    let (res,cur_block') = 
                              if List.length cur_block > 0
                              then (res@[cur_block],[])
                              else (res,cur_block)
                            in aux res (cur_block'@[h]) t
              | Some op ->  let cur_block' = cur_block@[h] in
                            let (res,cur_block'') = 
                              if    String.equal op "jmp" 
                                 || String.equal op "ret"
                                 || String.equal op "br"
                              then (res@[cur_block'],[])
                              else (res, cur_block')
                            in aux res cur_block'' t
  in aux [] [] instrs

let createBlockMap blocks = 
  let open Bril in
  let rec aux map names nextId = function
  | []    ->  (map,names)
  | h::t  ->  let i = (List.hd h) in
              match i with
              | None  -> (map,names)
              | Some i  -> 
                match i.label with
                | None  ->  let name = "b" ^ (string_of_int nextId) in
                            let m = map@[(name,h)] in
                            let n = names@[name] in
                            aux m n (nextId+1) t
                | Some l -> let m = map@[(l,h)] in
                            let n = names@[l] in
                            aux m n nextId t

  in aux [] [] 0 blocks


let emptyInstr = {Bril.dest=None;op=None;typ=None;value=None;labels=None;
                    label=None;args=None;}

let getStrOp = function
  | Some s  ->  s
  | None    ->  ""

let createCfg blockMap names = 
  let open Bril in
  let rec aux names res = function
  | []                ->  res
  | (name,block)::t   ->  
      let namesTl = List.tl names in
      let namesTl = match namesTl with
                    | Some n  ->  n
                    | None    ->  [] in
      let instr = List.hd (List.rev block) in
      let instr = match instr with
                  | Some i  -> i
                  | None    -> emptyInstr in
      let labels =  match instr.labels with
                    | Some l  ->  l
                    | None    ->  [] in
      match instr.op with
      | Some op ->  (match op with 
                    | "jmp" | "br"->  aux namesTl (res@[(name,labels)]) t
                    | "ret" ->  aux namesTl (res@[(name,[])]) t
                    | _     ->  
                        match t with
                        | []  -> aux namesTl (res@[(name,[])]) t
                        | _   ->  let n = List.hd namesTl in 
                                  let n = match n with 
                                  | Some x  ->  x
                                  | None    ->  "" in
                                  aux namesTl (res@[(name,[n])]) t)
      | None  -> aux namesTl (res@[(name,[])]) t

  in aux names [] blockMap

let printBlock b = Bril.printInstrs b 

let rec printBlocks = function
  | h::t  ->  print_string "[\n";
              printBlock h;
              print_string "\n]";
              printBlocks t; 
  | []    ->  ()

let rec printBlockMap = function
  | h::t  ->  print_string "[\n";
              let (name,b) = h in
              print_string (name ^ ": ");
              printBlock b;
              print_string "\n]";
              printBlockMap t;
  | []    ->  ()

let rec printStringList = function
  | []    ->  ()
  | h::t  ->  print_string ("  " ^ h ^ "\n");
              printStringList t

let rec printCfg = function
  | []  -> ()
  | (blockName,labels)::t ->
      printStringList 
          (List.map labels ~f:(fun lbl -> (blockName ^ " -> " ^ lbl ^ ";")));
      printCfg t



