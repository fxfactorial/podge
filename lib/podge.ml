(** Shortcuts and helpers for common tasks in OCaml *)

module Math = struct

  type 'a nums = Int : int nums | Float : float nums

  let random_array =
    fun (type t) n (num_t : t nums) ->
      Random.self_init ();
      match num_t with
      | Int ->
        (Array.init n (fun _ ->
             let made = Random.int (1 lsl 30 - 1) in
             if Random.bool () then made else -1 * made) : t array)
      | Float ->
        (Array.init n (fun _ -> Random.float max_float) : t array)

  let derivative ~f ~argument =
    let eps = sqrt epsilon_float in
    ((f (argument +. eps)) -. (f (argument -. eps))) /. (2. *. eps)

  let linear_regression ~xs ~ys =
    let sum xs = (Array.fold_right
                    (fun value running -> value +. running) xs 0.0) in
    let mean xs = (sum xs) /. (float_of_int (Array.length xs)) in
    let mean_x = mean xs in
    let mean_y = mean ys in
    let std xs m =
      let normalizer = (Array.length xs) - 1 in
      sqrt ((Array.fold_right
               (fun value running ->
                  ((value -. m) ** 2.0) +. running) xs 0.0) /.
            (float_of_int normalizer)) in
    let pearson_r xs ys =
      let sum_xy = ref 0.0 in
      let sum_sq_v_x = ref 0.0 in
      let sum_sq_v_y = ref 0.0 in
      let zipped = List.combine (Array.to_list xs) (Array.to_list ys) in
      List.iter (fun (i_x, i_y) ->
          let var_x = i_x -. mean_x in
          let var_y = i_y -. mean_y in
          sum_xy := !sum_xy +. (var_x *. var_y);
          sum_sq_v_x := !sum_sq_v_x +. (var_x ** 2.0);
          sum_sq_v_y := !sum_sq_v_y +. (var_y ** 2.0))
        zipped;
      !sum_xy /. (sqrt (!sum_sq_v_x *. !sum_sq_v_y)) in
    let r = pearson_r xs ys in
    let b = r *. (std ys mean_y) /. (std xs mean_x) in
    let a = mean_y -. b *. mean_x in
    let line x =
      b *. x +. a in
    line

  let rec pow ~base = function
    | 0 -> 1
    | 1 -> base
    | n ->
      let b = pow base (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else base)

  let log2 x = (log x ) /. (log 2.)

  let bit_string_of_int num =
    let rec helper a_num accum =
      match a_num with
      | 0 -> accum
      | x -> string_of_int (a_num mod 2) :: helper (a_num / 2) accum
    in
    helper num [] |> List.rev |> String.concat ""

  let bit_string_of_string str =
    let all_ints = ref [] in
    String.iter begin fun a_char ->
      all_ints := (int_of_char a_char) :: !all_ints
    end
      str;
    List.rev !all_ints |> List.map bit_string_of_int |> String.concat ""

  let sum_int_list l =
    List.fold_left ( + ) 0 l

  let sum_float_list l =
    List.fold_left ( +. ) 0.0 l

  let pi = 4.0 *. atan 1.0

  let range ?(chunk=1) ~from ~to_ =
    let rec loop lower upper =
      if lower > upper then []
      else
        (lower + chunk) :: loop (lower + chunk) upper
    in
    loop from to_

  let validate_prob p =
    if p < 0.0 || p > 1.0
    then raise (Invalid_argument "Not a valid Probability, \
                                  needs to be between 0 and 1")

  (** Computes the entropy from a list of probabilities *)
  let entropy probs =
    List.fold_left begin fun accum p ->
      validate_prob p;
      accum +. (p *. log2 (1.0 /. p))
    end
      0.0
      probs

  (** Represents the number of bits of information contained in this
      message, roughly how many number of bits we should encode this
      message with. The less likely an event is to occur, the more
      information we can say actually is contained in the event *)
  let self_information p =
    validate_prob p;
    log2 (1.0 /. p)

end

module Yojson = struct


end


module List = struct

  (** Evaluate f on each item of the given list and check if all
      evaluated to true *)
  let all ~f ~on =
    List.map f on |> List.fold_left (&&) true

  (** Evaluate f on each item of the given list and check if any
      evaluated to false *)
  let any ~f ~on =
    List.map f on |> List.fold_left (||) false

  let unique l =
    List.fold_left begin fun a e ->
      if List.mem e a
      then a
      else e :: a
    end
      []
      l
end

module Html5 = struct

  let show_tag e =
    Html5.P.print_list print_string [e]

  let element_to_string e =
    let cont = Buffer.create 1024 in
    let func = Buffer.add_string cont in
    Html5.P.print_list func [e];
    Buffer.contents cont

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

  let get_one_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

end

module Analyze = struct

  let time_it f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

  (* TODO Add a doc string explaing meaning *)
  let ratio_pair time_double time =
    let r = time_double /. time in
    (`Time_ratio r, `Time_log2_ratio (Math.log2 r))

end

module Cohttp = struct

  let show_headers hdrs = Cohttp.Header.iter begin fun key values ->
      Printf.sprintf "%s" (Printf.sprintf "%s %s" key (String.concat "" values))
      |> print_endline
    end
      hdrs

end

module Printf = struct
  let printfn str = Printf.kprintf print_endline str

end
