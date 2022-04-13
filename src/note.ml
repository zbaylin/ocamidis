let num_notes = 12

type note =
  | C (* 0 *)
  | C_sharp (* 1 *)
  | D (* 2 *)
  | D_sharp (* 3 *)
  | E (* 4 *)
  | F (* 5 *)
  | F_sharp (* 6 *)
  | G (* 7 *)
  | G_sharp (* 8 *)
  | A (* 9 *)
  | A_sharp (* 10 *)
  | B (* 11 *)

let pp_note fmt = function
  | C -> Format.fprintf fmt "C"
  | C_sharp -> Format.fprintf fmt "C#"
  | D -> Format.fprintf fmt "D"
  | D_sharp -> Format.fprintf fmt "D#"
  | E -> Format.fprintf fmt "E"
  | F -> Format.fprintf fmt "F"
  | F_sharp -> Format.fprintf fmt "F#"
  | G -> Format.fprintf fmt "G"
  | G_sharp -> Format.fprintf fmt "G#"
  | A -> Format.fprintf fmt "A"
  | A_sharp -> Format.fprintf fmt "A#"
  | B -> Format.fprintf fmt "B"

type t = { note : note; octave : int } [@@deriving show]

let note_of_int_unsafe = function
  | 0 -> C
  | 1 -> C_sharp
  | 2 -> D
  | 3 -> D_sharp
  | 4 -> E
  | 5 -> F
  | 6 -> F_sharp
  | 7 -> G
  | 8 -> G_sharp
  | 9 -> A
  | 10 -> A_sharp
  | 11 -> B
  | i ->
      invalid_arg
        (Printf.sprintf "Note number %d does not fall in range 0-11" i)

let note_of_int i =
  let note_i = i mod num_notes in
  note_of_int_unsafe note_i

let octave_of_int i = i / num_notes
let of_int i = { note = note_of_int i; octave = octave_of_int i }
let of_char c = c |> Char.code |> of_int
