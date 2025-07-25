open Eliom_content.Html.F

[%%client
open Eliom_content.Html.F
open Js_of_ocaml]

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
        [ txt "Acceleration speed(×10000): "
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
        [ txt " Duplication chance (×10000): "
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
        [ txt "Time before death (s):"
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
         [ div ~a:[a_id "introduction_contenair"] [div ~a:[a_id "game_title"] []]
         ; div
             ~a:[a_id "game_contenair"]
             [ div ~a:[a_id "lens-border"] [Game.game_area]
             ; div
                 ~a:[a_id "pillar-support-container"]
                 [div ~a:[a_id "pillar"] []; settings_panel ()] ] ]))
