open Eliom_content.Html.F

[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt

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
  ; mutable phage_list : Dom_html.divElement Js.t list }

type game_state =
  {mutable creets : creet list; mutable is_running : bool; mutable timer : float}]

let%shared r_growing_speed = 0.1
let%shared river_height = 80
let%shared hospital_height = 80
let%shared game_area_height = 800
let%shared game_area_width = 800
let%shared playground_height = 640
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
        []
    ; div
        ~a:
          [ a_id "game_over_screen"
          ; a_style
              "display:none; position:absolute; top:0; left:0; width:100%; height:100%; background:rgba(0,0,0,0.7); color:white; font-size:48px;  align-items:center; justify-content:center; z-index:1000; font-family:sans-serif;"
          ]
        [txt "GAME OVER"] ]

let%client check_collision c1 c2 =
  let dx = c1.x -. c2.x in
  let dy = c1.y -. c2.y in
  let dist2 = sqrt ((dx *. dx) +. (dy *. dy)) in
  dist2 < c1.r_size +. c2.r_size

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
  let ph = Dom_html.createDiv Dom_html.document in
  ph##.className := Js.string "phage";
  let creet_radius = creet.r_size in
  let phage_size = 20. in
  let center = creet_radius -. (phage_size /. 2.) in
  let angle = Random.float (2. *. Float.pi) in
  let dist = creet_radius *. 0.8 *. Random.float 1.0 in
  let x = center +. (cos angle *. dist) in
  let y = center +. (sin angle *. dist) in
  ph##.style##.left := Js.string (Printf.sprintf "%fpx" x);
  ph##.style##.top := Js.string (Printf.sprintf "%fpx" y);
  Dom.appendChild creet.dom ph;
  creet.phage_list <- ph :: creet.phage_list;
  set_scatter_vars ph

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
      if creet.state = Infected && Random.float 1.0 < 0.005
      then spawn_phage creet;
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
  let playground = Dom_html.getElementById "game_area" in
  let creet = Dom_html.createDiv Dom_html.document in
  creet##.id := Js.string id;
  let state_class =
    match creet_state with
    | Healthy -> "healthy"
    | Infected -> "infected"
    | Berserk -> "berserk"
    | Mean -> "mean"
  in
  creet##.className := Js.string ("cell-sprite " ^ state_class);
  creet##.style##.position := Js.string "absolute";
  creet##.style##.left := Js.string (Printf.sprintf "%dpx" x);
  creet##.style##.top := Js.string (Printf.sprintf "%dpx" y);
  let px = string_of_int (!creet_base_radius * 2) ^ "px" in
  creet##.style##.width := Js.string px;
  creet##.style##.height := Js.string px;
  set_scatter_vars creet;
  let mk_eye eye_class =
    let eye = Dom_html.createDiv Dom_html.document in
    eye##.className := Js.string ("cell-eye " ^ eye_class);
    eye##.style##.position := Js.string "absolute";
    eye##.style##.borderRadius := Js.string "50%";
    (Js.Unsafe.coerce eye##.style)##.boxShadow := Js.string "0 0 4px 1px #222";
    let pupil = Dom_html.createDiv Dom_html.document in
    pupil##.className := Js.string "cell-pupil";
    pupil##.style##.position := Js.string "absolute";
    pupil##.style##.background := Js.string "#111";
    pupil##.style##.borderRadius := Js.string "50%";
    Dom.appendChild eye pupil;
    eye, pupil
  in
  let eye_1, pupil_1 = mk_eye "cell-eye-left" in
  let eye_2, pupil_2 = mk_eye "cell-eye-right" in
  Dom.appendChild creet eye_1;
  Dom.appendChild creet eye_2;
  set_scatter_vars eye_1;
  set_scatter_vars eye_2;
  set_scatter_vars pupil_1;
  set_scatter_vars pupil_2;
  Dom.appendChild playground creet;
  let core = Dom_html.createDiv Dom_html.document in
  core##.className := Js.string "cell-core";
  let left_p = 20. +. Random.float 20. in
  let top_p = 40. +. Random.float 20. in
  let size_p = 20. +. Random.float 30. in
  core##.style##.left := Js.string (Printf.sprintf "%.1f%%" left_p);
  core##.style##.top := Js.string (Printf.sprintf "%.1f%%" top_p);
  core##.style##.width := Js.string (Printf.sprintf "%.1f%%" size_p);
  core##.style##.height := Js.string (Printf.sprintf "%.1f%%" size_p);
  Dom.appendChild creet core;
  set_scatter_vars core;
  for _ = 1 to 5 do
    let g = Dom_html.createDiv Dom_html.document in
    g##.className := Js.string "granule";
    let px = 20. +. Random.float 60. in
    let py = 20. +. Random.float 60. in
    g##.style##.left := Js.string (Printf.sprintf "%.1f%%" px);
    g##.style##.top := Js.string (Printf.sprintf "%.1f%%" py);
    let delay = Random.float 2.0 in
    g##.style##.animationDelay := Js.string (Printf.sprintf "%.2fs" delay);
    Dom.appendChild creet g;
    set_scatter_vars g
  done;
  creet, eye_1, eye_2, pupil_1, pupil_2

let%client show_game_over () =
  Js_of_ocaml.Firebug.console##log (Js.string "💀 show_game_over called!");
  let elt = Dom_html.getElementById "game_over_screen" in
  elt##.style##.display := Js.string "flex"

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

let%client enable_drag creet =
  let open Js_of_ocaml in
  let doc = Dom_html.document in
  let game_left = 0. in
  let game_top = 0. in
  let game_right = float_of_int game_area_width -. (creet.r_size *. 2.) in
  let game_bottom = float_of_int game_area_height -. (creet.r_size *. 2.) in
  let hospital_start = float_of_int (game_area_height - hospital_height) in
  let on_move ev =
    let x_raw = float_of_int ev##.clientX -. creet.r_size in
    let y_raw = float_of_int ev##.clientY -. creet.r_size in
    let x = max game_left (min game_right x_raw) in
    let y = max game_top (min game_bottom y_raw) in
    creet.x <- x;
    creet.y <- y;
    creet.dom##.style##.left := Js.string (Printf.sprintf "%fpx" x);
    creet.dom##.style##.top := Js.string (Printf.sprintf "%fpx" y);
    Js._false
  in
  let on_up _ev =
    if creet.y +. creet.r_size >= hospital_start
    then change_class_state Healthy creet;
    creet.available <- true;
    Option.iter Dom_html.removeEventListener creet.move_listener;
    Option.iter Dom_html.removeEventListener creet.up_listener;
    creet.move_listener <- None;
    creet.up_listener <- None;
    Js._false
  in
  let on_down _ev =
    creet.available <- false;
    creet.move_listener <-
      Some
        (Dom_html.addEventListener doc Dom_html.Event.mousemove
           (Dom_html.handler on_move) Js._false);
    creet.up_listener <-
      Some
        (Dom_html.addEventListener doc Dom_html.Event.mouseup
           (Dom_html.handler on_up) Js._false);
    Js._false
  in
  ignore
    (Dom_html.addEventListener creet.dom Dom_html.Event.mousedown
       (Dom_html.handler on_down) Js._false)

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
    let dx = (2.0 *. Random.float 1.0) -. 1.0 in
    let dy = (2.0 *. Random.float 1.0) -. 1.0 in
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
      ; phage_list = [] }
    in
    enable_drag new_creet;
    Lwt.async (fun () -> creet_loop game_state new_creet);
    game_state.creets <- new_creet :: game_state.creets)

let%client rec game_master_loop game_state =
  let open Lwt in
  if
    game_state.creets = []
    || not (List.exists (fun c -> c.state = Healthy) game_state.creets)
  then (
    show_game_over ();
    game_state.is_running <- false;
    Lwt.return_unit)
  else (
    game_state.timer <- game_state.timer +. 0.02;
    Lwt_js.sleep 0.02 >>= fun () -> game_master_loop game_state)

let%client init_client () =
  Random.self_init ();
  let creets = ref [] in
  for i = 0 to !number_of_creet_at_start - 1 do
    let x = Random.int game_area_width - (!creet_base_radius * 2) in
    let y =
      Random.int
        (game_area_height - (river_height * 4) - (!creet_base_radius * 2))
      + river_height
    in
    let dx = (2.0 *. Random.float 1.0) -. 1.0 in
    let dy = (2.0 *. Random.float 1.0) -. 1.0 in
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
      ; phage_list = [] }
    in
    enable_drag c;
    creets := c :: !creets
  done;
  let game_state = {creets = !creets; is_running = true; timer = 0.0} in
  List.iter
    (fun c -> Lwt.async (fun () -> creet_loop game_state c))
    game_state.creets;
  Lwt.async (fun () -> game_master_loop game_state)
