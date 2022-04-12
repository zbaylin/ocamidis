(lang dune 3.0)

(name ocamidis)

(license MIT)
(maintainers "Zach Baylin <me@zachbayl.in>")
(authors "Zach Baylin <me@zachbayl.in>")

(source (github zbaylin/ocaml-rtmidi))

(generate_opam_files true)

(package
  (name ocamidis)
  (synopsis "A library for interpreting/representing MIDI data")
  (depends
    (ocaml (>= 4.08.0))))
