type t =
  | Note_off of { channel : int; note : Note.t; velocity : int }
  | Note_on of { channel : int; note : Note.t; velocity : int }
  | Polyphonic_key_pressure of { channel : int; note : Note.t; pressure : int }
  | Control_change of { channel : int; controller : int; new_ : int }
  | Program_change of { channel : int; program : int }
  | Channel_pressure of { channel : int; pressure : int }
  | Pitch_wheel_change of { channel : int; value : int }
[@@deriving show]

let pp_b = Pp_binary_ints.Int.pp_int

let of_char_array (arr : char array) =
  let open Char_ex.Infix in
  match arr with
  | [| status; data0; data1 |] when (status & Mask.status) = Tag.note_off ->
      let channel = (status & Mask.channel) |> Char.code in
      let note = (data0 & Mask.note) |> Note.of_char in
      let velocity = (data1 & Mask.velocity) |> Char.code in
      Note_off { channel; note; velocity }
  | [| status; data0; data1 |] when (status & Mask.status) = Tag.note_on ->
      let channel = (status & Mask.channel) |> Char.code in
      let note = (data0 & Mask.note) |> Note.of_char in
      let velocity = (data1 & Mask.velocity) |> Char.code in
      Note_on { channel; note; velocity }
  | [| status; data0; data1 |]
    when (status & Mask.status) = Tag.polyphonic_key_pressure ->
      let channel = (status & Mask.channel) |> Char.code in
      let note = (data0 & Mask.note) |> Note.of_char in
      let pressure = (data1 & Mask.pressure) |> Char.code in
      Polyphonic_key_pressure { channel; note; pressure }
  | [| status; data0; data1 |] when (status & Mask.status) = Tag.control_change
    ->
      let channel = (status & Mask.channel) |> Char.code in
      let controller = (data0 & Mask.controller) |> Char.code in
      let new_ = (data1 & Mask.controller) |> Char.code in
      Control_change { channel; controller; new_ }
  | [| status; data0 |] when (status & Mask.status) = Tag.program_change ->
      let channel = (status & Mask.channel) |> Char.code in
      let program = (data0 & Mask.program) |> Char.code in
      Program_change { channel; program }
  | [| status; data0 |] when (status & Mask.status) = Tag.channel_pressure ->
      let channel = (status & Mask.channel) |> Char.code in
      let pressure = (data0 & Mask.pressure) |> Char.code in
      Channel_pressure { channel; pressure }
  | [| status; data0; data1 |]
    when (status & Mask.status) = Tag.pitch_wheel_change ->
      let channel = (status & Mask.channel) |> Char.code in
      let value0 = (data0 & Mask.pitch_wheel) |> Char.code in
      let value1 = (data1 & Mask.pitch_wheel) |> Char.code in
      let value = ((value1 lsl 7) lor value0) - 0x2000 in
      Pitch_wheel_change { channel; value }
  | _ ->
      invalid_arg
        (Format.asprintf "Invalid/unsupported MIDI data: 0x%a"
           (Arr_ex.pp ~sep:(fun fmt () -> Format.fprintf fmt "") Char_ex.pp_hex)
           arr)
