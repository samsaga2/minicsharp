module I = Ir

let loadint num =
  let dst = Temp.new_reg() in
  [I.LoadConstInt (dst,num)]

let loadnil () =
  loadint 0
