(** Shortcuts and helpers for common tasks in OCaml *)

module Math = struct

end

module Json = struct


end


module List = struct

end

module Html = struct

end

module Js = struct

end

module Unix = struct

  let read_process_output p =
    let ic = Unix.open_process_in p in
    let all_input = ref [] in
    try
      while true do
        all_input := input_line ic :: !all_input;
      done;
      []
    with
      End_of_file ->
      close_in ic;
      !all_input
end
