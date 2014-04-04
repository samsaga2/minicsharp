module type Machine =
  sig
    val asm : unit -> Asm.instr list
  end
