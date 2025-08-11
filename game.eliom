open Eliom_content.Html.F

[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js

type creet_state = Healthy | Infected | Berserk | Mean

type creet =
  { id : string
  ; mutable x : float
  ; mutable y : float
  ; mutable dx : float
  ; mutable dy : float
  ; mutable state : creet_state
  ; mutable available : bool
  ; mutable is_dead : bool
  ; mutable r_size : float
  ; mutable infected_time : float
  ; mutable move_listener : Dom_html.event_listener_id option
  ; mutable up_listener : Dom_html.event_listener_id option
  ; mutable dom : Dom_html.divElement Js.t
  ; mutable eye_1 : Dom_html.divElement Js.t
  ; mutable eye_2 : Dom_html.divElement Js.t
  ; mutable pupil_1 : Dom_html.divElement Js.t
  ; mutable pupil_2 : Dom_html.divElement Js.t
  ; mutable phage_list : Dom_html.divElement Js.t list
  ; mutable listener : unit Lwt.t option }

type game_state =
  { mutable creets : creet list
  ; mutable is_running : bool
  ; mutable timer : float
  ; drag_controller : creet Drag.t }]

let%shared r_growing_speed = 0.1
let%shared river_height = 50
let%shared hospital_height = 50
let%shared game_area_height = 500
let%shared game_area_width = 500
let%shared playground_height = 400
let%shared mean_color = "violet"
let%shared berserk_color = "orange"
let%shared infected_color = "green"
let%shared healthy_color = "gray"
let%shared direction_change_probability = 0.005
let%shared mean_reduce_factor = 0.85
let%shared mean_speed_acceleration = 0.0005
let%shared shrink_speed = 0.01
let%shared death_random_factor = 0.01
let%shared creet_duplication_chance = ref 0.0001
let%shared game_acceleration = ref 0.0005
let%shared creet_base_radius = ref 20
let%shared number_of_creet_at_start = ref 5
let%shared time_to_die = ref 12.0
let%shared accel_scale = 10000.
let%shared dup_scale = 10000.
let%shared time_scale = 1.
let%client on_game_end : (unit -> unit) option ref = ref None
let%client set_on_game_end_callback f = on_game_end := Some f
let%client hospital_line = float_of_int (game_area_height - hospital_height)
let%client drag_controller = ref None
let%shared creet_base_speed = ref 0.2
let%shared creet_random_speed_range = ref 0.1

let%server game_area =
  div
    ~a:
      [ a_id "game_area"
      ; a_style
          ("position: relative; width: "
          ^ string_of_int game_area_width
          ^ "px; height: "
          ^ string_of_int game_area_height
          ^ "px;") ]
    [ div
        ~a:
          [a_id "river"; a_style ("height:" ^ string_of_int river_height ^ "px")]
        []
    ; div
        ~a:
          [ a_id "playground"
          ; a_style
              ("height:"
              ^ string_of_int playground_height
              ^ "px; position:relative;") ]
        []
    ; div
        ~a:
          [ a_id "hospital"
          ; a_style ("height:" ^ string_of_int hospital_height ^ "px") ]
        [] ]

let%client check_collision c1 c2 =
  let center c = c.x +. c.r_size, c.y +. c.r_size in
  let x1, y1 = center c1 in
  let x2, y2 = center c2 in
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  let dist2 = (dx *. dx) +. (dy *. dy) in
  let rsum = c1.r_size +. c2.r_size in
  let coll = dist2 < rsum *. rsum in
  coll

let%client set_scatter_vars (elt : #Dom_html.element Js.t) : unit =
  let angle = Random.float 360. in
  let dist = 50. +. Random.float 100. in
  let dx = cos (angle *. Float.pi /. 180.) *. dist in
  let dy = sin (angle *. Float.pi /. 180.) *. dist in
  ignore
    (elt##.style##setProperty
       (Js.string "--dx")
       (Js.string (Printf.sprintf "%.1fpx" dx))
       Js.undefined);
  ignore
    (elt##.style##setProperty
       (Js.string "--dy")
       (Js.string (Printf.sprintf "%.1fpx" dy))
       Js.undefined);
  ignore
    (elt##.style##setProperty
       (Js.string "--rot")
       (Js.string (Printf.sprintf "%.1fdeg" angle))
       Js.undefined)

let%client clear_phages creet =
  List.iter
    (fun ph ->
       match Js.Opt.to_option ph##.parentNode with
       | Some parent -> Dom.removeChild parent ph
       | None -> ())
    creet.phage_list;
  creet.phage_list <- []

let%client reset_eye eye =
  eye##.style##.transform := Js.string "none";
  eye##.style##.width := Js.string "40%";
  eye##.style##.height := Js.string "40%";
  eye##.style##.background := Js.string "white"

let%client change_class_state new_state creet : unit =
  creet.state <- new_state;
  if new_state = Healthy
  then (clear_phages creet; reset_eye creet.eye_1; reset_eye creet.eye_2);
  let state_class =
    match new_state with
    | Healthy -> "healthy"
    | Infected -> "infected"
    | Berserk -> "berserk"
    | Mean -> "mean"
  in
  creet.dom##.className := Js.string ("cell-sprite " ^ state_class)

let%client spawn_phage creet =
  let creet_radius = creet.r_size in
  let phage_size = 20. in
  let center = creet_radius -. (phage_size /. 2.) in
  let angle = Random.float (2. *. Float.pi) in
  let dist = creet_radius *. 0.8 *. Random.float 1.0 in
  let x = center +. (cos angle *. dist) in
  let y = center +. (sin angle *. dist) in
  let ph_node =
    div
      ~a:
        [ a_class ["phage"]
        ; a_style
            ("left:" ^ Printf.sprintf "%fpx" x ^ ";top:"
           ^ Printf.sprintf "%fpx" y ^ ";") ]
      []
  in
  let ph_dom = To_dom.of_div ph_node in
  Dom.appendChild creet.dom ph_dom;
  creet.phage_list <- ph_dom :: creet.phage_list;
  set_scatter_vars ph_dom

let%client random_eye_deform eye =
  let should_move = Random.float 1.0 < 0.75 in
  let should_resize = Random.float 1.0 < 0.5 in
  if should_move
  then
    let dx = Random.float 32. -. 16. in
    let dy = Random.float 32. -. 16. in
    eye##.style##.transform
    := Js.string (Printf.sprintf "translate(%g%%,%g%%)" dx dy)
  else eye##.style##.transform := Js.string "none";
  if should_resize
  then (
    let base = 40. in
    let size = base +. (Random.float 40. -. 20.) in
    eye##.style##.width := Js.string (Printf.sprintf "%g%%" size);
    eye##.style##.height := Js.string (Printf.sprintf "%g%%" size))
  else (
    eye##.style##.width := Js.string "40%";
    eye##.style##.height := Js.string "40%");
  ()

let%client infect_creet game_state creet =
  let n = Random.int 10 in
  let new_state =
    if n = 0
    then Berserk (* 1/10 *)
    else if n = 1
    then Mean (* 1/10 *)
    else Infected (* 8/10 *)
  in
  change_class_state new_state creet;
  creet.infected_time <- game_state.timer;
  match new_state with
  | Infected ->
      random_eye_deform creet.eye_1;
      random_eye_deform creet.eye_2;
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85
  | Mean ->
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85;
      creet.r_size <- float_of_int !creet_base_radius *. mean_reduce_factor;
      let px = string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px" in
      creet.dom##.style##.width := Js.string px;
      creet.dom##.style##.height := Js.string px;
      random_eye_deform creet.eye_1;
      random_eye_deform creet.eye_2
  | Berserk ->
      random_eye_deform creet.eye_1;
      random_eye_deform creet.eye_2;
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85
  | Healthy -> ()

let%client propagate_infection game_state creet =
  if creet.state = Infected || creet.state = Berserk || creet.state = Mean
  then
    List.iter
      (fun target ->
         if
           target != creet && target.state = Healthy && target.available
           && (not target.is_dead)
           && check_collision creet target
           && Random.int 100 < 2
         then infect_creet game_state target)
      game_state.creets

let%client closest_healthy_creet creet creets =
  List.filter (fun c -> c.state = Healthy) creets
  |> List.fold_left
       (fun (min_c, min_dist) c ->
          let dx = c.x -. creet.x in
          let dy = c.y -. creet.y in
          let dist = (dx *. dx) +. (dy *. dy) in
          match min_c with
          | None -> Some c, dist
          | Some _ when dist < min_dist -> Some c, dist
          | _ -> min_c, min_dist)
       (None, max_float)
  |> fst

let%client check_border_collision creet =
  if creet.x < 0.
  then (
    creet.x <- 0.;
    creet.dx <- -.creet.dx)
  else if creet.x > float_of_int game_area_width -. (creet.r_size *. 2.)
  then (
    creet.x <- float_of_int game_area_width -. (creet.r_size *. 2.);
    creet.dx <- -.creet.dx);
  if creet.y < 0.
  then (
    creet.y <- 0.;
    creet.dy <- -.creet.dy)
  else if creet.y > float_of_int game_area_height -. (creet.r_size *. 2.)
  then (
    creet.y <- float_of_int game_area_height -. (creet.r_size *. 2.);
    creet.dy <- -.creet.dy)

let%client handle_infected_creet game_state creet =
  match creet.state with
  | Berserk ->
      if creet.r_size < float_of_int (4 * !creet_base_radius)
      then (
        creet.r_size <- creet.r_size +. r_growing_speed;
        creet.dom##.style##.width
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px");
        creet.dom##.style##.height
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px"));
      if Random.float 1.0 < 0.01 then spawn_phage creet
  | Mean -> (
      if Random.float 1.0 < 0.005 then spawn_phage creet;
      if creet.r_size > float_of_int !creet_base_radius *. mean_reduce_factor
      then (
        creet.dom##.style##.width
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px");
        creet.dom##.style##.height
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px"));
      match closest_healthy_creet creet game_state.creets with
      | Some target ->
          let dx = target.x -. creet.x in
          let dy = target.y -. creet.y in
          let dist = sqrt ((dx *. dx) +. (dy *. dy)) +. 0.0001 in
          let base_speed =
            sqrt ((creet.dx *. creet.dx) +. (creet.dy *. creet.dy))
          in
          let speed = base_speed +. mean_speed_acceleration in
          creet.dx <- dx /. dist *. speed;
          creet.dy <- dy /. dist *. speed
      | None -> ())
  | Infected ->
      if creet.state = Infected && Random.float 1.0 < 0.005
      then spawn_phage creet
  | _ -> ()

let%client check_river game_state creet =
  if creet.state = Healthy && creet.y <= float_of_int river_height
  then infect_creet game_state creet

let%client random_rotation creet =
  if Random.float 1.0 < direction_change_probability
  then (
    let angle = Random.float (2. *. Float.pi) in
    let speed = sqrt ((creet.dx *. creet.dx) +. (creet.dy *. creet.dy)) in
    creet.dx <- cos angle *. speed;
    creet.dy <- sin angle *. speed)

let%client remove_creet game_state creet =
  creet.is_dead <- true;
  creet.dom##.classList##add (Js.string "explode");
  Drag.dettach game_state.drag_controller creet;
  let on_end _ev =
    (match Js.Opt.to_option creet.dom##.parentNode with
    | Some parent -> Dom.removeChild parent creet.dom
    | None -> ());
    game_state.creets <- List.filter (fun c -> c != creet) game_state.creets;
    Js._false
  in
  ignore
    (Dom_html.addEventListener creet.dom Dom_html.Event.animationend
       (Dom_html.handler on_end) Js._false)

let%client creet_callbacks =
  { Drag.on_start = (fun c _ _ -> c.available <- false)
  ; on_move =
      (fun c x y ->
        c.x <- x;
        c.y <- y;
        c.dom##.style##.left := Js.string (Printf.sprintf "%fpx" x);
        c.dom##.style##.top := Js.string (Printf.sprintf "%fpx" y))
  ; on_end =
      (fun c y ->
        c.available <- true;
        if y +. c.r_size >= hospital_line then change_class_state Healthy c)
  ; get_pos = (fun c -> c.x, c.y)
  ; get_dom = (fun c -> c.dom)
  ; get_listener = (fun c -> c.listener)
  ; set_listener = (fun c listener -> c.listener <- listener) }

let%client check_alive game_state creet =
  if creet.state = Infected || creet.state = Berserk || creet.state = Mean
  then
    let dt = game_state.timer -. creet.infected_time in
    if dt > !time_to_die && Random.float 1.0 < death_random_factor
    then remove_creet game_state creet

let%client rec generate_unique_id game_state =
  let id = "creet" ^ string_of_int (Random.int 1_000_000) in
  if List.exists (fun c -> c.id = id) game_state.creets
  then generate_unique_id game_state
  else id

let%client create_creet id x y creet_state =
  let state_class =
    match creet_state with
    | Healthy -> "healthy"
    | Infected -> "infected"
    | Berserk -> "berserk"
    | Mean -> "mean"
  in
  let px = string_of_int (!creet_base_radius * 2) ^ "px" in
  let mk_eye side_class =
    let pupil_node =
      div
        ~a:
          [ a_class ["cell-pupil"]
          ; a_style "position:absolute;background:#111;border-radius:50%" ]
        []
    in
    let eye_node =
      div
        ~a:
          [ a_class ["cell-eye"; side_class]
          ; a_style "position:absolute;border-radius:50%" ]
        [pupil_node]
    in
    let eye_dom = To_dom.of_div eye_node in
    let pupil_dom = To_dom.of_div pupil_node in
    eye_node, eye_dom, pupil_dom
  in
  let eye_1_node, eye_1_dom, pupil_1_dom = mk_eye "cell-eye-left" in
  let eye_2_node, eye_2_dom, pupil_2_dom = mk_eye "cell-eye-right" in
  let left_p = 20. +. Random.float 20.
  and top_p = 40. +. Random.float 20.
  and size_p = 20. +. Random.float 30. in
  let core_node =
    div
      ~a:
        [ a_class ["cell-core"]
        ; a_style
            (Printf.sprintf "left:%.1f%%;top:%.1f%%;width:%.1f%%;height:%.1f%%;"
               left_p top_p size_p size_p) ]
      []
  in
  let granule_nodes =
    let one () =
      let px = 20. +. Random.float 60. in
      let py = 20. +. Random.float 60. in
      let delay = Random.float 2.0 in
      div
        ~a:
          [ a_class ["granule"]
          ; a_style
              (Printf.sprintf "left:%.1f%%;top:%.1f%%;animation-delay:%.2fs;" px
                 py delay) ]
        []
    in
    List.init 5 (fun _ -> one ())
  in
  let creet_node =
    div
      ~a:
        [ a_id id
        ; a_class ["cell-sprite"; state_class]
        ; a_style
            ("position:absolute;" ^ "left:" ^ string_of_int x ^ "px;" ^ "top:"
           ^ string_of_int y ^ "px;" ^ "width:" ^ px ^ ";" ^ "height:" ^ px
           ^ ";") ]
      (eye_1_node :: eye_2_node :: core_node :: granule_nodes)
  in
  let creet_dom = To_dom.of_div creet_node in
  let playground = Dom_html.getElementById "game_area" in
  Dom.appendChild playground creet_dom;
  set_scatter_vars (creet_dom :> Dom_html.element Js.t);
  set_scatter_vars (eye_1_dom :> Dom_html.element Js.t);
  set_scatter_vars (eye_2_dom :> Dom_html.element Js.t);
  set_scatter_vars (pupil_1_dom :> Dom_html.element Js.t);
  set_scatter_vars (pupil_2_dom :> Dom_html.element Js.t);
  List.iter
    (fun g -> set_scatter_vars (To_dom.of_div g :> Dom_html.element Js.t))
    granule_nodes;
  creet_dom, eye_1_dom, eye_2_dom, pupil_1_dom, pupil_2_dom

let%client return_to_normal_size creet =
  match creet.state with
  | Healthy ->
      let base = float_of_int !creet_base_radius in
      if creet.r_size > base
      then creet.r_size <- max base (creet.r_size -. r_growing_speed)
      else if creet.r_size < base
      then creet.r_size <- min base (creet.r_size +. shrink_speed);
      let px = string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px" in
      creet.dom##.style##.width := Js.string px;
      creet.dom##.style##.height := Js.string px
  | _ -> ()

let%client update_pupil_position dx dy (pupil1, pupil2) creet =
  let open Js in
  let base_amp = 30.0 in
  let amp =
    match creet.state with
    | Mean -> base_amp *. 0.5
    | Berserk -> 0.
    | _ -> base_amp
  in
  let mag = sqrt ((dx *. dx) +. (dy *. dy)) in
  let norm_dx, norm_dy = if mag = 0. then 0., 0. else dx /. mag, dy /. mag in
  let percent_x = 50. +. (norm_dx *. amp) in
  let percent_y = 50. +. (norm_dy *. amp) in
  let left_str = Printf.sprintf "%.1f%%" percent_x in
  let top_str = Printf.sprintf "%.1f%%" percent_y in
  pupil1##.style##.left := string left_str;
  pupil1##.style##.top := string top_str;
  pupil2##.style##.left := string left_str;
  pupil2##.style##.top := string top_str

let%client rec creet_loop (game_state : game_state) creet =
  let open Lwt in
  if not game_state.is_running
  then Lwt.return_unit
  else (
    if creet.available && not creet.is_dead
    then (
      let speed = sqrt ((creet.dx *. creet.dx) +. (creet.dy *. creet.dy)) in
      let new_speed = speed +. !game_acceleration in
      if speed > 0.0
      then (
        let ratio = new_speed /. speed in
        creet.dx <- creet.dx *. ratio;
        creet.dy <- creet.dy *. ratio);
      random_rotation creet;
      creet.x <- creet.x +. creet.dx;
      creet.y <- creet.y +. creet.dy;
      check_border_collision creet;
      check_river game_state creet;
      creet.dom##.style##.left := Js.string (Printf.sprintf "%fpx" creet.x);
      creet.dom##.style##.top := Js.string (Printf.sprintf "%fpx" creet.y);
      update_pupil_position creet.dx creet.dy
        (creet.pupil_1, creet.pupil_2)
        creet;
      maybe_duplicate_creet game_state creet;
      handle_infected_creet game_state creet;
      return_to_normal_size creet);
    check_alive game_state creet;
    propagate_infection game_state creet;
    Lwt_js.sleep 0.02 >>= fun () -> creet_loop game_state creet)

and maybe_duplicate_creet game_state creet =
  if creet.state = Healthy && Random.float 1.0 < !creet_duplication_chance
  then (
    let id = generate_unique_id game_state in
    let x = creet.x +. float_of_int (Random.int 30 - 15) in
    let y = creet.y +. float_of_int (Random.int 30 - 15) in
    let dx = creet.dx in
    let dy = creet.dy in
    let dom, eye_1, eye_2, pupil_1, pupil_2 =
      create_creet id (int_of_float x) (int_of_float y) creet.state
    in
    let new_creet =
      { id
      ; x
      ; y
      ; dx
      ; dy
      ; state = Healthy
      ; available = true
      ; is_dead = false
      ; r_size = float_of_int !creet_base_radius
      ; infected_time = 0.0
      ; move_listener = None
      ; up_listener = None
      ; dom
      ; eye_1
      ; eye_2
      ; pupil_1
      ; pupil_2
      ; phage_list = []
      ; listener = None }
    in
    Drag.attach game_state.drag_controller new_creet;
    Lwt.async (fun () -> creet_loop game_state new_creet);
    game_state.creets <- new_creet :: game_state.creets)

let%client rec game_master_loop game_state =
  let open Lwt in
  if
    (not game_state.is_running)
    || game_state.creets = []
    || not (List.exists (fun c -> c.state = Healthy) game_state.creets)
  then (
    (match !on_game_end with Some f -> f () | None -> ());
    List.iter
      (fun c -> if not c.is_dead then remove_creet game_state c)
      game_state.creets;
    game_state.is_running <- false;
    Lwt.return_unit)
  else (
    game_state.timer <- game_state.timer +. 0.02;
    Lwt_js.sleep 0.02 >>= fun () -> game_master_loop game_state)

let%client turn_on_light () =
  let open Js_of_ocaml in
  let add_light_class id =
    match Dom_html.getElementById_coerce id Dom_html.CoerceTo.div with
    | Some el -> el##.classList##add (Js.string "light-on")
    | None -> Firebug.console##log (Js.string ("[⚠️] element not found: " ^ id))
  in
  List.iter add_light_class ["lens-border"; "game_title"; "river"; "hospital"]

let%client init_client () =
  Random.self_init ();
  let creets = ref [] in
  let drag_controller =
    match !drag_controller with
    | Some drag_controller -> drag_controller
    | None ->
        let controller = Drag.create creet_callbacks in
        drag_controller := Some controller;
        Drag.attach_global_listeners controller;
        controller
  in
  for i = 0 to !number_of_creet_at_start - 1 do
    let x = Random.int game_area_width - (!creet_base_radius * 2) in
    let y =
      Random.int
        (game_area_height - (river_height * 4) - (!creet_base_radius * 2))
      + river_height
    in
    let angle = Random.float (2. *. Float.pi) in
    let speed =
      let base = !creet_base_speed in
      let range = !creet_random_speed_range in
      max 0. (base +. ((Random.float 2. -. 1.) *. range))
    in
    let dx = cos angle *. speed in
    let dy = sin angle *. speed in
    let infected = false in
    let dom, eye_1, eye_2, pupil_1, pupil_2 =
      create_creet ("creet" ^ string_of_int i) x y Healthy
    in
    let c =
      { id = "creet" ^ string_of_int i
      ; x = float_of_int x
      ; y = float_of_int y
      ; dx
      ; dy
      ; state = (if infected then Infected else Healthy)
      ; available = true
      ; is_dead = false
      ; r_size = float_of_int !creet_base_radius
      ; infected_time = 0.0
      ; move_listener = None
      ; up_listener = None
      ; dom
      ; eye_1
      ; eye_2
      ; pupil_1
      ; pupil_2
      ; phage_list = []
      ; listener = None }
    in
    Drag.attach drag_controller c;
    creets := c :: !creets
  done;
  let game_state =
    {creets = !creets; is_running = true; timer = 0.0; drag_controller}
  in
  turn_on_light ();
  List.iter
    (fun c -> Lwt.async (fun () -> creet_loop game_state c))
    game_state.creets;
  Lwt.async (fun () -> game_master_loop game_state)
