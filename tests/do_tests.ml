#require "podge"

let () =
  print_endline (Podge.Web.get Sys.argv.(1))
