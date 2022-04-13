let convert_int_op1 (f : int -> int) (x : char) =
  f (Char.code x) |> Char.unsafe_chr

let convert_int_op2 (f : int -> int -> int) (x : char) (y : char) =
  f (Char.code x) (Char.code y) |> Char.unsafe_chr

let pp_hex fmt c = Format.fprintf fmt "%02x" (Char.code c)

module Infix = struct
  let ( & ) = convert_int_op2 ( land )
  let ( ^ ) = convert_int_op2 ( lxor )
  let ( << ) = convert_int_op2 ( lsl )
  let ( >> ) = convert_int_op2 ( lsr )
  let lnot = convert_int_op1 lnot
end
