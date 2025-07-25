open Eliom_content.Html.F

[%%client
open Eliom_content.Html.F
open Js_of_ocaml]

let%client timer_running = ref false
let%client start_time = ref 0.

let%shared start_button =
  button ~a:[a_class ["btn start-game"]; a_id "start-button"] [txt "Start game"]

let%client lock_settings_panel (bool : bool) =
  let open Js_of_ocaml in
  let js_bool = Js.bool bool in
  let disable_input id =
    match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
    | Some e -> e##.disabled := js_bool
    | None ->
        Firebug.console##log
          (Js.string
             ("[lock_settings_panel] Input \"" ^ id
            ^ "\" not found or not <input>"))
  in
  let disable_button id =
    match Dom_html.getElementById_coerce id Dom_html.CoerceTo.button with
    | Some e -> e##.disabled := js_bool
    | None ->
        Firebug.console##log
          (Js.string
             ("[lock_settings_panel] Button \"" ^ id
            ^ "\" not found or not <button>"))
  in
  disable_input "input_num_creet";
  disable_input "input_acceleration";
  disable_input "input_duplication";
  disable_input "input_time_to_die";
  disable_input "input_base_radius";
  disable_button "start-button"

let%client update_settings_from_inputs () =
  let get_int_input id =
    match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
    | Some input -> input##.value |> Js.to_string |> int_of_string
    | None -> failwith ("Input \"" ^ id ^ "\"  not found or not <input> HTML")
  in
  Game.number_of_creet_at_start := get_int_input "input_num_creet";
  Game.creet_base_radius := get_int_input "input_base_radius";
  let vi = get_int_input "input_acceleration" in
  Game.game_acceleration := float vi /. Game.accel_scale;
  let vd = get_int_input "input_duplication" in
  Game.creet_duplication_chance := float vd /. Game.dup_scale;
  let vt = get_int_input "input_time_to_die" in
  Game.time_to_die := float vt /. Game.time_scale

let%shared settings_panel () =
  div
    ~a:[a_id "settings_panel"]
    [ label
        [ txt "Number of creet at start:"
        ; input
            ~a:
              [ a_input_type `Number
              ; a_value (string_of_int !Game.number_of_creet_at_start)
              ; a_id "input_num_creet"
              ; a_input_min (`Number 1)
              ; a_input_max (`Number 100) ]
            () ]
    ; label
        [ txt "Base radius: "
        ; input
            ~a:
              [ a_input_type `Number
              ; a_value (string_of_int !Game.creet_base_radius)
              ; a_id "input_base_radius"
              ; a_input_min (`Number 5)
              ; a_input_max (`Number 100) ]
            () ]
    ; label
        [ txt "Acceleration speed factor: "
        ; input
            ~a:
              [ a_input_type `Number
              ; a_id "input_acceleration"
              ; a_value
                  (string_of_int
                     (!Game.game_acceleration *. Game.accel_scale
                     |> int_of_float))
              ; a_input_min (`Number 0)
              ; a_input_max (`Number (int_of_float (Game.accel_scale *. 1.0)))
              ]
            () ]
    ; label
        [ txt " Duplication chance factor: "
        ; input
            ~a:
              [ a_input_type `Number
              ; a_id "input_duplication"
              ; a_value
                  (string_of_int
                     (!Game.creet_duplication_chance *. Game.dup_scale
                     |> int_of_float))
              ; a_input_min (`Number 0)
              ; a_input_max (`Number (int_of_float Game.dup_scale)) ]
            () ]
    ; label
        [ txt "Time before can death:"
        ; input
            ~a:
              [ a_input_type `Number
              ; a_id "input_time_to_die"
              ; a_value
                  (string_of_int
                     (!Game.time_to_die *. Game.time_scale |> int_of_float))
              ; a_input_min (`Number 1)
              ; a_input_max (`Number 600) (* max 10 minutes, par ex. *) ]
            () ]
    ; start_button ]

let%client tutorial_pages =
  [| "Booting.", 0.4
   ; "Booting..", 0.4
   ; "Booting...", 0.7
   ; "Booting.", 0.4
   ; "Booting..", 0.4
   ; "Booting...", 0.7
   ; "Welcome to H42N42 — Lab Protocol #42.", 4.0
   ; ( "You are observing a controlled population of micro-organisms under the lens."
     , 4.5 )
   ; ( "An unknown virus, codenamed <b>H42N42</b>, is spreading from the upper contamination zone — the river."
     , 5.5 )
   ; "<b>Your role:</b> interact with the sample in real-time.", 3.6
   ; "Isolate infected cells by moving them to the healing area.", 3.7
   ; ( "Protect the healthy ones — they reproduce as long as they stay uninfected."
     , 5.0 )
   ; ( "Beware: infected specimens may mutate — some grow out of control (<b>Berserk</b>), others become aggressive (<b>Mean</b>) and chase healthy cells."
     , 7.0 )
   ; ( "Delay the outbreak as long as possible.<br>When no healthy cells remain... the simulation ends."
     , 7.0 ) |]

let%client set_side_screen_page (html : string) =
  match
    Dom_html.getElementById_coerce "side-screen-content" Dom_html.CoerceTo.div
  with
  | Some elt -> elt##.innerHTML := Js.string html
  | None -> ()

let%client rec show_tutorial_pages idx =
  let open Lwt in
  let open Js_of_ocaml_lwt in
  if !timer_running
  then Lwt.return_unit
  else if idx < Array.length tutorial_pages
  then (
    let text, delay = tutorial_pages.(idx) in
    set_side_screen_page text;
    Lwt_js.sleep delay >>= fun () -> show_tutorial_pages (idx + 1))
  else Lwt.return_unit

let%client rec show_timer (start : float) =
  let open Js_of_ocaml_lwt in
  let open Lwt in
  start_time := start;
  let now = (Js_of_ocaml.Js.Unsafe.eval_string "Date.now()" : float) /. 1000. in
  let elapsed = now -. start in
  let txt = Printf.sprintf "<b>Time elapsed:</b><br>%.1f s" elapsed in
  set_side_screen_page txt;
  if !timer_running
  then Lwt_js.sleep 0.1 >>= fun () -> show_timer start
  else Lwt.return_unit

let%client reset_game () =
  timer_running := false;
  lock_settings_panel false;
  let elapsed =
    ((Js_of_ocaml.Js.Unsafe.eval_string "Date.now()" : float) /. 1000.)
    -. !start_time
  in
  set_side_screen_page
    (Printf.sprintf "<b>Simulation ended!</b><br><br>Time: %.1f s" elapsed)

let%server main_page () =
  Eliom_content.Html.F.(
    html
      (head
         (title (txt "h42n42"))
         [ meta ~a:[a_charset "utf-8"] ()
         ; css_link
             ~uri:
               (make_uri
                  ~service:(Eliom_service.static_dir ())
                  ["css"; "h42n42.css"])
             () ])
      (body
         [ div
             ~a:[a_id "introduction_contenair"; a_class ["side"]]
             [div ~a:[a_id "game_title"] []]
         ; div
             ~a:[a_id "game_contenair"]
             [ div
                 ~a:[a_id "lens-border"]
                 [ Game.game_area
                 ; div
                     ~a:[a_id "side-screen"]
                     [div ~a:[a_id "side-screen-content"] [txt "Booting..."]] ]
             ; div
                 ~a:[a_id "pillar-support-container"]
                 [div ~a:[a_id "pillar"] [settings_panel ()]] ]
         ; div ~a:[a_class ["side"]] [] ]))
