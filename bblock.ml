open Batteries

type t = {insts: Ir.t list; name: Symbol.t option; pre: t list; succ: t list}

let rec make insts =
  (split_blocks insts) |> set_name |> set_succ |> set_pre

and split_blocks insts =
  let rec blocks insts blist =
    match insts with
    | hd::tl ->
       let rec next insts thisblock =
	 match insts with
	 | (Ir.Jump _ as s)::rest
	 | (Ir.JumpFalse _ as s)::rest ->
	    endblock rest (s::thisblock)
	 | (Ir.Label _)::_ as rest ->
	    endblock rest thisblock
	 | s::rest ->
	    next rest (s::thisblock)
	 | [] ->
	    endblock [] thisblock
       and endblock insts thisblock =
	 blocks insts ((List.rev thisblock)::blist) in
       next tl [hd]
    | [] ->
       List.rev blist in
  blocks insts []

and set_name blocks =
  List.map
    (function
      | (Ir.Label name)::_ as insts ->
	 {insts=insts; name=Some name; pre=[]; succ=[]}
      | insts ->
	 {insts=insts; name=None; pre=[]; succ=[]})
    blocks

and make_table bblocks =
  let table = Hashtbl.create 1000 in
  List.iter (function
	      | {name=Some (name); _} as block ->
		 Hashtbl.add table name block
	      | _ -> ())
	    bblocks;
  table

and set_succ bblocks =
  let table = make_table bblocks in
  let get_succ bblock next =
    let inst = List.last bblock.insts in
    match inst with
    | Ir.Jump (l) ->
       [Hashtbl.find table l]
    | Ir.JumpFalse (_,l) ->
       [(Hashtbl.find table l); next]
    | _ ->
       [next]
  in
  let rec set_succ = function
    | [] ->
       []
    | hd::tl ->
       match tl with
       | [] ->
	  [hd]
       | next::tl as rest ->
	  let succ = get_succ hd next in
	  let hd = {hd with succ=succ} in
	  hd::(set_succ rest) in
  set_succ bblocks

and set_pre bblocks =
  let rec set_pre = function
    | [] ->
       []
    | hd::tl ->
       match tl with
       | [] ->
	  [hd]
       | next::_ as rest ->
	  let pre = next.succ in
	  let hd = {hd with pre=pre} in
	  hd::(set_pre rest) in
  List.rev (set_pre (List.rev bblocks))
