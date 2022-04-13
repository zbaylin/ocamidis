let tag = Char.unsafe_chr
let note_off = tag 0b1000_0000
let note_on = tag 0b1001_0000
let polyphonic_key_pressure = tag 0b1010_0000
let control_change = tag 0b1011_0000
let program_change = tag 0b1100_0000
let channel_pressure = tag 0b1101_0000
let pitch_wheel_change = tag 0b1110_0000
