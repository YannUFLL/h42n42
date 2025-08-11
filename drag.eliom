[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js

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
  ; mutable offset : float * float
  ; layer : Dom_html.divElement Js.t
  ; original_parent : Dom_html.divElement Js.t
  ; mutable playground_offset : float * float }

let make_drag_layer () : Dom_html.divElement Js.t =
  let layer_node =
    div
      ~a:
        [ a_id "drag-layer"
        ; a_style
            "position:fixed;left:0;top:0;right:0;bottom:0;z-index:999999;pointer-events:none;"
        ]
      []
  in
  let layer_dom = To_dom.of_div layer_node in
  Dom.appendChild Dom_html.document##.body layer_dom;
  layer_dom

let create callbacks original_parent =
  { cb = callbacks
  ; current = None
  ; offset = 0., 0.
  ; layer = make_drag_layer ()
  ; original_parent
  ; playground_offset = 0., 0. }

let compute_offset t ev =
  let dom = t.cb.get_dom (Option.get t.current) in
  let r = dom##getBoundingClientRect in
  let playground_x, playground_y = t.cb.get_pos (Option.get t.current) in
  t.playground_offset <-
    ( float_of_int ev##.clientX -. playground_x
    , float_of_int ev##.clientY -. playground_y );
  t.offset <-
    float_of_int ev##.clientX -. r##.left, float_of_int ev##.clientY -. r##.top

let handle_move t ev _ =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      let x = float_of_int ev##.clientX -. fst t.offset in
      let y = float_of_int ev##.clientY -. snd t.offset in
      t.cb.on_move elt x y; Lwt.return_unit

let handle_up t ev _ =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      Dom.appendChild t.original_parent (t.cb.get_dom elt);
      t.cb.on_move elt
        (float_of_int ev##.clientX -. fst t.playground_offset)
        (float_of_int ev##.clientY -. snd t.playground_offset);
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
  let dom = t.cb.get_dom elt in
  compute_offset t ev;
  Dom.appendChild t.layer dom;
  t.cb.on_move elt
    (float_of_int ev##.clientX -. fst t.offset)
    (float_of_int ev##.clientY -. snd t.offset);
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
