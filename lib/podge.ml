(** Shortcuts and helpers for common tasks in OCaml *)

module U = Unix
module P = Printf
module L = List
module Y = Yojson.Basic
module T = ANSITerminal

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

  let derivative ~f argument =
    let eps = sqrt epsilon_float in
    ((f (argument +. eps)) -. (f (argument -. eps))) /. (2. *. eps)

  let linear_regression ~xs ~ys =
    let sum xs = Array.fold_right (fun value running -> value +. running) xs 0.0 in
    let mean xs = (sum xs) /. (float_of_int (Array.length xs)) in
    let mean_x = mean xs in
    let mean_y = mean ys in
    let std xs m =
      let normalizer = (Array.length xs) - 1 in
      sqrt ((Array.fold_right begin fun value running ->
          ((value -. m) ** 2.0) +. running
        end
          xs 0.0) /.
            (float_of_int normalizer)) in
    let pearson_r xs ys =
      let sum_xy = ref 0.0 in
      let sum_sq_v_x = ref 0.0 in
      let sum_sq_v_y = ref 0.0 in
      let zipped = List.combine (Array.to_list xs) (Array.to_list ys) in
      List.iter begin fun (i_x, i_y) ->
        let var_x = i_x -. mean_x in
        let var_y = i_y -. mean_y in
        sum_xy := !sum_xy +. (var_x *. var_y);
        sum_sq_v_x := !sum_sq_v_x +. (var_x ** 2.0);
        sum_sq_v_y := !sum_sq_v_y +. (var_y ** 2.0)
      end
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
    let rec helper a_num accum = match a_num with
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

  let sum_ints l =
    List.fold_left ( + ) 0 l

  let sum_floats l =
    List.fold_left ( +. ) 0.0 l

  let average_ints l =
    float_of_int (sum_ints l) /. float_of_int (List.length l)

  let average_floats l =
    sum_floats l /. float_of_int (List.length l)

  let pi = 4.0 *. atan 1.0

  let range ?(chunk=1) ~from to_ =
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

  let rec distance l r = match l, r with
    | a_val_l :: rest_l, a_val_r :: rest_r ->
      (a_val_l -. a_val_r) ** 2.0 +. distance rest_l rest_r
    | _ -> 0.0

  let init_with_f ~f n =
    let rec init_aux n accum =
      if n <= 0 then accum else init_aux (n - 1) (f (n - 1) :: accum)
    in
    init_aux n []

  let combination n m =
    let g (k, r) = init_with_f (fun i -> k + pow 2 (n - i - 1), i) r in
    let rec aux m xs =
      if m = 1 then List.map fst xs
      else aux (m - 1) (List.map g xs |> List.concat)
    in
    aux m (init_with_f (fun i -> pow 2 i, n - i - 1) n)

end

module Yojson = struct

  type did_update = [`Updated | `No_update]

  let show_pretty_of_string s =
    Yojson.Basic.from_string s
    |> Yojson.Basic.pretty_to_string
    |> print_endline

  let show_pretty_of_in_mem j =
    Yojson.Basic.pretty_to_string j |> print_endline

  let show_pretty_of_file f =
    Yojson.Basic.from_file f
    |> Yojson.Basic.pretty_to_string
    |> print_endline

  let update ~key ~new_value j : (did_update * Yojson.Basic.json) =
    let updated = ref false in
    let as_obj = Yojson.Basic.Util.to_assoc j in
    let g = List.map begin function
        | (this_key, inner) when this_key = key -> updated := true; (this_key, new_value)
        | otherwise -> otherwise
      end
        as_obj
    in
    if !updated then (`Updated, `Assoc g) else (`No_update, `Assoc g)

  let remove ~key j : (did_update * Yojson.Basic.json) =
    let updated = ref false in
    let as_obj = Yojson.Basic.Util.to_assoc j in
    let g = List.fold_left begin fun accum ((this_key, _) as key_value) ->
        if this_key = key then (updated := true; accum) else key_value :: accum
      end
        []
        as_obj
    in
    if !updated then (`Updated, `Assoc (List.rev g))
    else (`No_update, `Assoc (List.rev g))

end

module Html5 = struct

  let show_tag e =
    Html5.P.print_list print_string [e]

  let to_string e =
    let cont = Buffer.create 1024 in
    let func = Buffer.add_string cont in
    Html5.P.print_list func [e];
    Buffer.contents cont

end

module Unix = struct

  type exn += Error of string

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
      List.rev !all_input

  (** Get a char from the terminal without waiting for the return key *)
  let get_one_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_icanon = false };
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

  let time_now () =
    let localtime = Unix.localtime (Unix.time ()) in
    Printf.sprintf "[%02u:%02u:%02u]"
      localtime.Unix.tm_hour
      localtime.Unix.tm_min
      localtime.Unix.tm_sec

  let daemonize () = match Unix.fork () with
    | pid ->
      if pid < 0 then raise (Error "Couldn't fork correctly")
      else if pid > 0 then exit (-1)
      else begin match Unix.setsid () with
        | sid ->
          if sid < 0 then raise (Error "Issue with setsid")
          else if sid > 0 then exit (-1)
          else begin
            Unix.umask 0 |> fun _ ->
            Unix.chdir "/"; List.iter Unix.close [Unix.stdin; Unix.stdout]
          end
      end

end

module Analyze = struct

  let time_it ~f x =
    let t = Sys.time() in
    let fx = f x in
    (Printf.sprintf "Execution time: %fs\n" (Sys.time() -. t), fx)

  (* TODO Add a doc string explaing meaning *)
  let ratio_pair time_double time =
    let r = time_double /. time in
    (`Time_ratio r, `Time_log2_ratio (Math.log2 r))

end

module Cohttp = struct

  let did_request_succeed resp =
    Cohttp.Response.status resp
    |> Cohttp.Code.code_of_status
    |> Cohttp.Code.is_success

  let show_headers hdrs = Cohttp.Header.iter begin fun key values ->
      Printf.sprintf "%s" (Printf.sprintf "%s %s" key (String.concat "" values))
      |> print_endline
    end
      hdrs

end

module Printf = struct

  let printfn str = Printf.kprintf print_endline str

end

module Debugging = struct

  let show_callstack n =
    Printexc.get_callstack n
    |> Printexc.raw_backtrace_to_string
    |> print_endline

end

module List = struct

  (** Evaluate f on each item of the given list and check if all
      evaluated to true *)
  let all ~f on =
    List.map f on |> List.fold_left (&&) true

  (** Evaluate f on each item of the given list and check if any
      evaluated to false *)
  let any ~f on =
    List.map f on |> List.fold_left (||) false

  let unique l =
    List.fold_left (fun a e -> if List.mem e a then a else e :: a) [] l

  let group_by ls =
    let ls' = List.fold_left begin fun accum (this_key, x1) ->
        match accum with
        | [] -> [(this_key, [x1])]
        | (that_key, ls2) :: acctl ->
          if this_key = that_key then (this_key, x1 :: ls2) :: acctl
          else (this_key, [x1]) :: accum
      end
        []
        ls
    in
    List.rev ls'

  let take ~n xs =
    let rec aux n xs accum =
      if n <= 0 || xs = [] then List.rev accum
      else aux (n - 1) (List.tl xs) (List.hd xs :: accum)
    in
    aux n xs []

  let rec drop ~n xs =
    if n <= 0 || xs = [] then xs
    else drop (n - 1) (List.tl xs)

  let equal_parts ~segs l =
    let this_much = (List.length l) / segs in
    let rec helper accum rest = match rest with
      | [] -> accum
      | rest ->
        let pull = take this_much rest in
        let remaining = drop this_much rest in
        if List.length remaining < this_much
        then (remaining @ pull) :: helper accum []
        else pull :: helper accum remaining
    in
    helper [] l

  let filter_map ~f ~g l =
    List.fold_left (fun accum value -> if f value then g value :: accum else accum) [] l

end

module Web : sig

  type exn += Not_valid_uri of string
  type exn += Bad_http_result of string

  (** Produces a socket ready for HTTP requests and returns the
      socket, host and querystring *)
  val socket_for_url : string -> U.file_descr * string * string

  (** Simple HTTP based get request, produces headers and body in json
      structure with keys "headers" and "body" *)
  val get : string -> Y.json

  (** Simple HTTP based put requests: takes url and payload and
      produces headers and body in json structure with keys "headers" and
      "body"*)
  val put : url:string -> payload:string -> Y.json

end = struct

  type exn += Not_valid_uri of string
  type exn += Bad_http_result of string

  (** Produces a socket ready for HTTP requests and returns the
      socket, host and querystring *)
  let socket_for_url url =
    let as_uri = Uri.of_string url in
    match (as_uri |> Uri.host, as_uri |> Uri.path_and_query) with
    | (None, _) -> raise (Not_valid_uri "Check your input, don't seem right")
    | (Some host, p_query) ->
      let this_inet_addr = (U.gethostbyname host).U.h_addr_list.(0) in
      let a_socket = U.socket U.PF_INET U.SOCK_STREAM 0 in
      U.connect a_socket (U.ADDR_INET (this_inet_addr, 80));
      (a_socket, host, p_query)

  let pull_all a_socket =
    let final_result = Buffer.create 4096 in
    let a_buffer = Bytes.create 1024 in
    let rec produce_data () =
      U.select [a_socket] [] [] 0.75 |>
      function
      | read_me :: _, _, _ ->
        begin
          match U.recv a_socket a_buffer 0 1024 [] with
          | 0 | -1 -> ()
          | n ->
            Buffer.add_bytes final_result (Bytes.sub a_buffer 0 n);
            produce_data ()
        end
      | _ -> ()
    in
    produce_data ();
    Buffer.to_bytes final_result |> Bytes.to_string

  let examine_result raw_result : Y.json =
    match Re_pcre.split ~rex:(Re_pcre.regexp "\r\n\r\n") raw_result with
    | headers :: body :: [] ->
      let headers_l : Y.json list =
        Re_pcre.full_split ~rex:(Re_pcre.regexp "\r\n") headers
        |> L.filter (function Re_pcre.Text s -> true | _ -> false)
        |> L.map (fun (Re_pcre.Text s) -> `String s)
      in
      `Assoc [("headers", `List headers_l); ("body", `String body)]
    | otherwise ->
      raise (Bad_http_result "Either no data or \
                              couldn't split headers from body")

  (** Simple HTTP based get request, produces headers and body in json
      structure with keys "headers" and "body" *)
  let get url : Y.json =
    let (a_socket, host, p) = socket_for_url url in
    let send_me =
      P.sprintf "GET %s HTTP/1.1\r\nHOST: %s\r\n\r\n" p host |> Bytes.of_string
    in
    let len = Bytes.length send_me in
    let _ = U.send a_socket send_me 0 len [] in
    pull_all a_socket |> examine_result

  (** Simple HTTP based put requests: takes url and payload and
      produces headers and body in json structure with keys "headers" and
      "body"*)
  let put ~url ~payload : Y.json =
    let (a_socket, _, path_query) = socket_for_url url in
    let payload_len = String.length payload in
    let send_me =
      P.sprintf "PUT %s HTTP/1.1\r\n\
                 Content-type: application/octet-stream\r\n\
                 Content-length: %d\r\n\r\n
                 %s"
        path_query
        payload_len
        payload
    in
    let total_len = String.length send_me in
    let _ = U.send a_socket send_me 0 total_len [] in
    pull_all a_socket |> examine_result

end

module Xml = struct

  open Ezxmlm

  let query_node_of_file ~key path =
    let (_, xml) = from_channel (open_in path) in
    member key xml |> data_to_string

  let query_node_of_string ~key str =
    let (_, xml) = from_string str in
    member key xml |> data_to_string

end

module ANSITerminal = struct

  (** Create a colorized message, presumably for a log message *)
  let colored_message ?(t_color=T.Yellow) ?(m_color=T.Blue) ?(with_time=true) str =
    let just_time = T.sprintf [T.Foreground t_color] "%s " (Unix.time_now ()) in
    let just_message = T.sprintf [T.Foreground m_color] "%s" str in
    if with_time then just_time ^ just_message else just_message

end
