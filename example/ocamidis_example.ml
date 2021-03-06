open Rtmidi
open Lwt.Infix

type action = List_ports | Listen_to of int

module Cli = struct
  open Cmdliner

  let port_number_term =
    let info =
      Arg.info [ "p"; "port_num" ] ~doc:"The port number to listen to"
    in
    Arg.value (Arg.opt Arg.int 0 info)

  let listen_to_term =
    let f p = Listen_to p in
    Term.(const f $ port_number_term)

  let listen_to_info =
    Cmd.info "listen-to" ~doc:"Listen to events on a specified MIDI port"

  let listen_to = Cmd.v listen_to_info listen_to_term
  let list_ports_term = Term.const List_ports
  let list_ports_info = Cmd.info "list-ports" ~doc:"List available MIDI ports"
  let list_ports = Cmd.v list_ports_info list_ports_term
  let root_term = Term.ret (Term.const (`Help (`Pager, None)))
  let root_info = Cmd.info "ocamidis_example"

  let parse () =
    let group =
      Cmdliner.Cmd.group root_info ~default:root_term [ listen_to; list_ports ]
    in
    match Cmdliner.Cmd.eval_value group with Ok (`Ok a) -> a | _ -> exit 1
end

let listen_to p =
  let in_ = In.create_default () in
  let generic = in_ |> of_in in
  open_port generic p "RtMidi";
  let stream = Rtmidi_lwt.In.message_stream in_ in
  let rec helper () =
    Lwt_stream.get stream >>= function
    | None -> helper ()
    | Some (_, data) ->
        let event = Ocamidis.Event.of_char_array data in
        Logs_lwt.info (fun m ->
            m "Received MIDI event: %a" Ocamidis.Event.pp event)
        >>= helper
  in
  Lwt_main.run @@ helper ()

let list_ports () =
  let in_ = In.create_default () in
  let generic = in_ |> of_in in
  let port_count = get_port_count generic in
  for i = 0 to port_count - 1 do
    Logs.info (fun m -> m "Port %d: %s" i (get_port_name generic i))
  done

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  match Cli.parse () with
  | List_ports -> list_ports ()
  | Listen_to p -> listen_to p
