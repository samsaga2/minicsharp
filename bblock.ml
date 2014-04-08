let rec split insts =
  blocks insts []

and blocks insts blist =
  match insts with
  | hd::tl ->
     let rec next insts thisblock =
       match insts with
       | (Ir.Jump _ as s)::rest
       | (Ir.JumpFalse _ as s)::rest
       | (Ir.Label _ as s)::rest ->
	  endblock rest (s::thisblock)
       | s::rest ->
	  next rest (s::thisblock)
       | [] ->
	  endblock [] thisblock
     and endblock insts thisblock =
       blocks insts ((List.rev thisblock)::blist) in
     next tl [hd]
  | [] ->
     List.rev blist
