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
  ; dom : Dom_html.divElement Js.t }

type game_state =
  {mutable creets : creet list; mutable is_running : bool; mutable timer : float}]

let%shared creet_base_radius = 20
let%shared r_growing_speed = 0.1
let%shared river_height = 50
let%shared hospital_height = 50
let%shared game_area_height = 600
let%shared game_area_width = 600
let%shared playground_height = 500
let%shared mean_color = "violet"
let%shared berserk_color = "orange"
let%shared infected_color = "green"
let%shared healthy_color = "gray"
let%shared game_acceleration = 0.0005
let%shared direction_change_probability = 0.005
let%shared creet_duplication_chance = 0.0001
let%shared number_of_creet_at_start = 5
let%shared time_to_die : float = 15.0
let%shared mean_reduce_factor = 0.85
let%shared mean_speed_acceleration = 0.0005
let%shared shrink_speed = 0.01
let%shared death_random_factor = 0.01

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
          [ a_id "river"
          ; a_style
              ("height:" ^ string_of_int river_height ^ "px; background:blue;")
          ]
        []
    ; div
        ~a:
          [ a_id "playground"
          ; a_style
              ("height:"
              ^ string_of_int playground_height
              ^ "px; background:white; position:relative;") ]
        []
    ; div
        ~a:
          [ a_id "hospital"
          ; a_style
              ("height:" ^ string_of_int hospital_height ^ "px; background:red;")
          ]
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

let%client infect_creet game_state creet =
  let n = Random.int 10 in
  let new_state =
    if n = 0
    then Berserk (* 1/10 *)
    else if n = 1
    then Mean (* 1/10 *)
    else Infected (* 8/10 *)
  in
  creet.state <- new_state;
  creet.infected_time <- game_state.timer;
  (match new_state with
  | Infected ->
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85
  | Mean ->
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85;
      creet.r_size <- float_of_int creet_base_radius *. mean_reduce_factor;
      let px = string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px" in
      creet.dom##.style##.width := Js.string px;
      creet.dom##.style##.height := Js.string px
  | Berserk ->
      creet.dx <- creet.dx *. 0.85;
      creet.dy <- creet.dy *. 0.85
  | Healthy -> ());
  creet.dom##.style##.backgroundColor
  := Js.string
       (match new_state with
       | Berserk -> berserk_color
       | Mean -> mean_color
       | Infected -> infected_color
       | Healthy -> healthy_color)

let%client propagate_infection game_state creet =
  if creet.state = Infected || creet.state = Berserk || creet.state = Mean
  then
    List.iter
      (fun target ->
         if
           target != creet && target.state = Healthy && target.available
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
      if creet.r_size < float_of_int (4 * creet_base_radius)
      then (
        creet.r_size <- creet.r_size +. r_growing_speed;
        creet.dom##.style##.width
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px");
        creet.dom##.style##.height
        := Js.string (string_of_int (int_of_float (creet.r_size *. 2.)) ^ "px"))
  | Mean -> (
      if creet.r_size > float_of_int creet_base_radius *. mean_reduce_factor
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
  Option.iter Dom_html.removeEventListener creet.move_listener;
  Option.iter Dom_html.removeEventListener creet.up_listener;
  creet.move_listener <- None;
  creet.up_listener <- None;
  creet.is_dead <- true;
  (match Js.Opt.to_option creet.dom##.parentNode with
  | Some parent -> Dom.removeChild parent creet.dom
  | None -> ());
  game_state.creets <- List.filter (fun c -> c != creet) game_state.creets

let%client check_alive game_state creet =
  if creet.state = Infected || creet.state = Berserk || creet.state = Mean
  then
    let dt = game_state.timer -. creet.infected_time in
    if dt > time_to_die && Random.float 1.0 < death_random_factor
    then remove_creet game_state creet

let%client rec generate_unique_id game_state =
  let id = "creet" ^ string_of_int (Random.int 1_000_000) in
  if List.exists (fun c -> c.id = id) game_state.creets
  then generate_unique_id game_state
  else id

let%client create_creet id x y infected =
  let playground = Dom_html.getElementById "game_area" in
  let creet = Dom_html.createDiv Dom_html.document in
  creet##.id := Js.string id;
  creet##.className := Js.string "creet";
  creet##.style##.position := Js.string "absolute";
  creet##.style##.left := Js.string (Printf.sprintf "%dpx" x);
  creet##.style##.top := Js.string (Printf.sprintf "%dpx" y);
  creet##.style##.width
  := Js.string (string_of_int (creet_base_radius * 2) ^ "px");
  creet##.style##.height
  := Js.string (string_of_int (creet_base_radius * 2) ^ "px");
  creet##.style##.backgroundColor
  := Js.string (if infected then infected_color else healthy_color);
  Dom.appendChild playground creet;
  creet

let%client show_game_over () =
  Js_of_ocaml.Firebug.console##log (Js.string "💀 show_game_over called!");
  let elt = Dom_html.getElementById "game_over_screen" in
  elt##.style##.display := Js.string "flex"

let%client return_to_normal_size creet =
  match creet.state with
  | Healthy ->
      let base = float_of_int creet_base_radius in
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
    then (
      creet.state <- Healthy;
      creet.dom##.style##.backgroundColor := Js.string healthy_color);
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

let%client rec creet_loop (game_state : game_state) creet =
  let open Lwt in
  if not game_state.is_running
  then Lwt.return_unit
  else (
    if creet.available
    then (
      let speed = sqrt ((creet.dx *. creet.dx) +. (creet.dy *. creet.dy)) in
      let new_speed = speed +. game_acceleration in
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
      maybe_duplicate_creet game_state creet;
      handle_infected_creet game_state creet;
      return_to_normal_size creet);
    check_alive game_state creet;
    propagate_infection game_state creet;
    Lwt_js.sleep 0.02 >>= fun () -> creet_loop game_state creet)

and maybe_duplicate_creet game_state creet =
  if creet.state = Healthy && Random.float 1.0 < creet_duplication_chance
  then (
    let id = generate_unique_id game_state in
    let x = creet.x +. float_of_int (Random.int 30 - 15) in
    let y = creet.y +. float_of_int (Random.int 30 - 15) in
    let dx = (2.0 *. Random.float 1.0) -. 1.0 in
    let dy = (2.0 *. Random.float 1.0) -. 1.0 in
    let dom = create_creet id (int_of_float x) (int_of_float y) false in
    let new_creet =
      { id
      ; x
      ; y
      ; dx
      ; dy
      ; state = Healthy
      ; available = true
      ; is_dead = false
      ; r_size = float_of_int creet_base_radius
      ; infected_time = 0.0
      ; move_listener = None
      ; up_listener = None
      ; dom }
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
  for i = 0 to number_of_creet_at_start - 1 do
    let x = Random.int game_area_width - (creet_base_radius * 2) in
    let y = Random.int game_area_height - (creet_base_radius * 2) in
    let dx = (2.0 *. Random.float 1.0) -. 1.0 in
    let dy = (2.0 *. Random.float 1.0) -. 1.0 in
    let infected = false in
    let dom = create_creet ("creet" ^ string_of_int i) x y infected in
    let c =
      { id = "creet" ^ string_of_int i
      ; x = float_of_int x
      ; y = float_of_int y
      ; dx
      ; dy
      ; state = (if infected then Infected else Healthy)
      ; available = true
      ; is_dead = false
      ; r_size = float_of_int creet_base_radius
      ; infected_time = 0.0
      ; move_listener = None
      ; up_listener = None
      ; dom }
    in
    enable_drag c;
    creets := c :: !creets
  done;
  let game_state = {creets = !creets; is_running = true; timer = 0.0} in
  List.iter
    (fun c -> Lwt.async (fun () -> creet_loop game_state c))
    game_state.creets;
  Lwt.async (fun () -> game_master_loop game_state)
