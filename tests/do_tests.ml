#require "podge"

let () =
  (* Podge.Web.get Sys.argv.(1) *)
  (* |> Podge.Yojson.show_pretty_of_in_mem *)
  Podge.Web.put Sys.argv.(1) Sys.argv.(2)
  |> Podge.Yojson.show_pretty_of_in_mem
