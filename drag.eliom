[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt

type 'a callbacks =
  { on_start : 'a -> float -> float -> unit
  ; on_move : 'a -> float -> float -> unit
  ; on_end : 'a -> float -> unit
  ; get_pos : 'a -> float * float
  ; get_dom : 'a -> Dom_html.element Js.t
  ; get_listener : 'a -> unit Lwt.t option
  ; set_listener : 'a -> unit Lwt.t option -> unit }

type 'a t =
  { cb : 'a callbacks
  ; mutable current : 'a option
  ; mutable offset : float * float }

let create callbacks = {cb = callbacks; current = None; offset = 0., 0.}

let compute_offset t ev =
  let mouse_x = float_of_int ev##.clientX in
  let mouse_y = float_of_int ev##.clientY in
  let obj_x, obj_y = t.cb.get_pos (Option.get t.current) in
  t.offset <- mouse_x -. obj_x, mouse_y -. obj_y

let handle_move t ev _ =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      let dx, dy = t.offset in
      let x = float_of_int ev##.clientX -. dx in
      let y = float_of_int ev##.clientY -. dy in
      t.cb.on_move elt x y; Lwt.return_unit

let handle_up t ev _ =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      t.current <- None;
      let _, dy = t.offset in
      let y = float_of_int ev##.clientY -. dy in
      t.cb.on_end elt y; Lwt.return_unit

let attach_global_listeners t =
  Lwt.async (fun () ->
    Lwt_js_events.mousemoves Dom_html.document (handle_move t));
  Lwt.async (fun () -> Lwt_js_events.mouseups Dom_html.document (handle_up t))

let start_drag t elt ev =
  Dom.preventDefault ev;
  t.current <- Some elt;
  compute_offset t ev;
  let obj_x, obj_y = t.cb.get_pos elt in
  t.cb.on_start elt obj_x obj_y;
  Lwt.return_unit

let attach t elt =
  let dom = t.cb.get_dom elt in
  let listener =
    Lwt_js_events.mousedowns dom (fun ev _ -> start_drag t elt ev)
  in
  t.cb.set_listener elt (Some listener)

let dettach t elt =
  match t.cb.get_listener elt with
  | None -> ()
  | Some listener -> Lwt.cancel listener]
