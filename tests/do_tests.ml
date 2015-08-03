(** Needs to be compiled because of js_of_ocaml *)

let () =
  Podge.Unix.read_process_output "uname -a" |> List.iter print_endline
