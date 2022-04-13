let sext i b =
  let m = 1 lsl (b - 1) in
  (i lxor m) - m

module Infix = struct
  let ( <-< ) = sext
end
