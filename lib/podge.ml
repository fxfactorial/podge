(** A Hodgepodge of functionality in OCaml *)

module U = Unix
module P = Printf
module L = List
module Y = Yojson.Basic
module T = ANSITerminal
module S = String

(** Math and Probability functions *)
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
      let zipped = L.combine (Array.to_list xs) (Array.to_list ys) in
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

  (** Range takes:
      an optional chunk int [defaulting to 1],
      an optional inclusivity bool [defaulting to false],
      a labeled ~from int [where the list starts from]
      and finally, the "upto" int [where the list ends].

      By default, your upto is not inclusive. So for example:
      1 <--> 5 gives back [1; 2; 3; 4]
      but
      1 <---> 5 gives back [1; 2; 3; 4; 5]

      It can also handle descending amounts:
      1 <---> -10 gives you
      [1; 0; -1; -2; -3; -4; -5; -6; -7; -8; -9; -10]
      and 1 <--> 1 gives you []

      Note: <--> <---> are located in Podge.Infix
      See also: it is tail recursive. *)
  let range ?(chunk=1) ?(inclusive=false) ~from upto =
    if (upto < from)
    then begin
      let rec dec_aux count upto accum =
        if inclusive
        then begin
          if (count - chunk) < upto
          then List.rev (count::accum)
          else dec_aux (count - chunk) upto (count::accum)
        end
        else begin
          if (count - chunk) <= upto
          then List.rev (count::accum)
          else dec_aux (count - chunk) upto (count::accum)
        end
      in
      dec_aux from upto []
    end
    else if upto = from then []
    else begin
      let rec asc_aux count upto accum =
        if inclusive then begin
          if (count + chunk) > upto
          then List.rev (count::accum)
          else asc_aux (count + chunk) upto (count::accum)
        end else begin
          if (count + chunk) >= upto
          then List.rev (count::accum)
          else asc_aux (count + chunk) upto (count::accum)
        end
      in
      asc_aux from upto []
    end

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

module Infix = struct
  (** See Podge.Math.range for documentation. *)
  let (<-->) i j = Math.range ~from:i j
  (** See Podge.Math.range for documentation. *)
  let (<--->) i j = Math.range ~inclusive:true ~from:i j
end

(** Pretty printing of json and updating *)
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

  (** Update a value for a given key *)
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

  (** Remove a key-value pair *)
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

module Unix : sig

  val read_process_output : string -> string list
  val read_lines : string -> string list
  val read_all : string -> string
  val get_one_char : unit -> char
  val time_now : unit -> string
  val daemonize : unit -> unit
  val timeout:
    ?on_timeout:(unit -> unit) ->
    arg:'a ->
    timeout:int ->
    default_value:'b -> ('a -> 'b) -> 'b

end = struct

  let exhaust ic =
    let all_input = ref [] in
    (try while true do all_input := input_line ic :: !all_input; done
     with End_of_file -> ());
    close_in ic;
    List.rev !all_input

  let read_process_output p = Unix.open_process_in p |> exhaust

  let read_lines path = open_in path |> exhaust

  let read_all path = open_in path |> exhaust |> S.concat ""

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
      if pid < 0 then raise (Failure "Couldn't fork correctly")
      else if pid > 0 then exit (-1)
      else begin match Unix.setsid () with
        | sid ->
          if sid < 0 then raise (Failure "Issue with setsid")
          else if sid > 0 then exit (-1)
          else begin
            Unix.umask 0 |> fun _ ->
            Unix.chdir "/"; List.iter Unix.close [Unix.stdin; Unix.stdout]
          end
      end

  let timeout ?(on_timeout = fun () -> ()) ~arg ~timeout ~default_value f =
    let module Wrapper = struct exception Timeout end in
    let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Wrapper.Timeout) in
    let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
    let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior in
    ignore (Unix.alarm timeout);
    try
      let res = f arg in
      reset_sigalrm ();
      res
    with exc ->
      reset_sigalrm ();
      if exc = Wrapper.Timeout
      then (on_timeout (); default_value)
      else raise exc

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

  let show_headers hdrs =
    hdrs |> Cohttp.Header.iter begin fun key values ->
      Printf.sprintf "%s" (Printf.sprintf "%s %s" key (S.concat "" values))
      |> print_endline
    end
end

module Printf = struct

  let printfn str = Printf.kprintf print_endline str
  let printfn_e str = Printf.kprintf prerr_endline str

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
    List.fold_left begin fun accum value ->
      if f value then g value :: accum else accum end
      [] l

  let cut l start =
    let result = List.fold_right begin fun value (counter, (pre, after)) ->
        if counter <> start then (counter + 1, (value :: pre, after))
        else (counter, (pre, value :: after))
      end
        (List.rev l)
        (0, ([], []))
    in
    let (pre, after) = snd result in
    (List.rev pre, List.rev after)

end

module Web : sig

  type error_t =
      Can_only_handle_http | Host_lookup_failed | No_ip_from_hostname
    | Post_failed

  type reply = (string list * string, error_t) result

  (** Takes a HTTP url and gives back an optional pair of a list of
      headers and the body *)
  val get: ?trim_body:bool -> string -> reply

  (** Takes a route and a bytes for body and does a PUT, no checking
      of HTTP errors *)
  val post :  ?trim_reply:bool -> ?post_body:string -> string -> reply

end = struct

  type error_t =
      Can_only_handle_http | Host_lookup_failed | No_ip_from_hostname
    | Post_failed

  type reply = (string list * string, error_t) result

  let (>>=) x (f: 'a -> 'b option) = match x with None -> None | Some d -> (f d)

  let address host =
    U.(ADDR_INET ((gethostbyname host).h_addr_list.(0), 80))

  let really_output out_chan message =
    output_bytes out_chan message; flush out_chan

  let read_all in_chan =
    let buff = Buffer.create 4096 in
    (try while true do input_char in_chan |> Buffer.add_char buff done
     with End_of_file -> ());
    buff |> Buffer.to_bytes

  let headers_and_body ?(trim_body=true) http_resp =
    let starting_point = ref 2 in
    let end_len = Bytes.length http_resp in
    let last_two = Bytes.create 2 in
    Bytes.set last_two 0 '\x00'; Bytes.set last_two 1 '\x00';
    let current = Bytes.create 2 in
    (try
       while !starting_point < end_len - 2 do Bytes.(
           set last_two 0 (get http_resp (!starting_point - 2));
           set last_two 1 (get http_resp (!starting_point - 1));
           set current 0 (get http_resp (!starting_point));
           set current 1 (get http_resp (!starting_point + 1));
           if last_two = "\r\n" && current = "\r\n" then raise Exit;
           incr starting_point
         )
       done;
     with Exit -> ());
    let leftover = end_len - !starting_point in
    let headers =
      Bytes.sub http_resp 0 !starting_point
      |> String.split_on_char '\n'
      |> L.map String.trim
      |> L.filter (( <> ) "")
    in
    (headers,
     Bytes.sub http_resp !starting_point leftover
     |> fun body -> if trim_body then S.trim body else body)

  let get ?(trim_body=true) route =
    let error_reason = ref Can_only_handle_http in
    let uri = Uri.of_string route in
    let request = Uri.scheme uri >>= fun s ->
      match s with
      | x when x <> "http" -> None
      | _ -> Uri.host uri >>= fun host ->
        (try Some (U.open_connection (address host))
         with
           Not_found -> error_reason := Host_lookup_failed; None
         | Invalid_argument _ -> error_reason := No_ip_from_hostname; None
        ) >>= fun (in_chan, out_chan) ->
        U.(setsockopt (descr_of_out_channel out_chan) TCP_NODELAY true);
        let msg host =
          P.sprintf
            "GET / HTTP/1.1\r\n\
             Host:%s\r\n\
             User-Agent: OCaml - Podge\r\n\
             Connection: close\r\n\r\n" host
        in
        really_output out_chan (msg host);
        let all = read_all in_chan in
        (try close_in in_chan; close_out out_chan with _ -> ());
        Some (headers_and_body ~trim_body all)
    in
    match request with None -> Error !error_reason | Some x -> Ok x

  let post ?(trim_reply=true) ?post_body route =
    let uri = Uri.of_string route in
    let error_reason = ref Post_failed in
    let request =
      Uri.scheme uri >>= fun s ->
      match s with
      | x when x <> "http" -> None
      | _ -> Uri.host uri >>= fun host ->
        let (in_chan, out_chan) = U.open_connection (address host) in
        let fd_ = U.descr_of_out_channel out_chan in
        U.setsockopt fd_ U.TCP_NODELAY true;
        let post_request = match post_body with
          | Some b ->
            P.sprintf "POST %s HTTP/1.1\r\n\
                       Host:%s\r\n\
                       Content-length: %d\r\n\
                       User-Agent: OCaml - Podge\r\n\
                       Connection: close\r\n\r\n%s"
              (Uri.path_and_query uri)
              host
              (Bytes.length b)
              b
          | None ->
            P.sprintf "POST %s HTTP/1.1\r\n\
                       Host:%s\r\n\
                       User-Agent: OCaml - Podge\r\n\
                       Connection: close\r\n\r\n"
              (Uri.path_and_query uri)
              host
        in
        really_output out_chan post_request;
        let reply = read_all in_chan in
        (try close_in in_chan; close_out out_chan with _ -> ());
        Some (headers_and_body ~trim_body:trim_reply reply)
    in
    match request with None -> Error !error_reason | Some x -> Ok x

end

(** Simple querying for Xml nodes, keys order matters *)
module Xml : sig

  val query_node_of_file : keys:string list -> path:string -> string
  val query_node_of_string : keys:string list -> str:string -> string

end = struct

  open Ezxmlm

  let rec dig keys xml_doc final_result =
    match keys with
    | [] -> final_result
    | outer_most :: rest ->
      let new_xml =
        try member (S.lowercase_ascii outer_most) xml_doc
        with _ -> member (S.lowercase_ascii outer_most) xml_doc
      in
      dig rest new_xml (data_to_string new_xml)

  let query_node_of_file ~keys ~path =
    let (_, xml) = from_channel (open_in path) in
    dig keys xml ""

  let query_node_of_string ~keys ~str =
    let (_, xml) = from_string str in
    dig keys xml ""

end

(* module ANSITerminal = struct *)

(*   (\** Create a colorized message, presumably for a log message *\) *)
(*   let colored_message ?(t_color=T.Yellow) ?(m_color=T.Blue) ?(with_time=true) str = *)
(*     let just_time = T.sprintf [T.Foreground t_color] "%s " (Unix.time_now ()) in *)
(*     let just_message = T.sprintf [T.Foreground m_color] "%s" str in *)
(*     if with_time then just_time ^ just_message else just_message *)

(* end *)
