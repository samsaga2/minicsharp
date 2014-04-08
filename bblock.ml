let split insts =
  let rec blocks insts blist =
    match insts with
    | hd::tl ->
       let rec next insts thisblock =
	 match insts with
	 | (Ir.Jump _ as s)::rest
	 | (Ir.Label _ as s)::rest ->
	    endblock rest (s::thisblock)
	 | (Ir.JumpFalse _ as s)::rest ->
	    let label = Label.new_label () in
	    let insts = (Ir.Label label)::rest
	    and thisblock = (Ir.Jump label)::s::thisblock in
	    endblock insts thisblock
	 | s::rest ->
	    next rest (s::thisblock)
	 | [] ->
	    endblock [] thisblock
       and endblock insts thisblock =
	 blocks insts ((List.rev thisblock)::blist) in
       next tl [hd]
    | [] ->
       List.rev blist in

  let blist = blocks insts [] in

  let done_label = Label.new_label () in
  let done_inst = Ir.Label done_label in
  (blist@[[done_inst]], done_label)
