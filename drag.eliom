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

let get_scale (el : #Dom_html.element Js.t) : float * float =
  let rect = el##getBoundingClientRect in
  let ow = float_of_int (Js.Unsafe.coerce el)##.offsetWidth in
  let oh = float_of_int (Js.Unsafe.coerce el)##.offsetHeight in
  let rect_w = Js.Optdef.get rect##.width (fun () -> ow) in
  let rect_h = Js.Optdef.get rect##.height (fun () -> oh) in
  let sx = if ow = 0. then 1. else rect_w /. ow in
  let sy = if oh = 0. then 1. else rect_h /. oh in
  sx, sy

let create callbacks original_parent =
  { cb = callbacks
  ; current = None
  ; offset = 0., 0.
  ; layer = make_drag_layer ()
  ; original_parent
  ; playground_offset = 0., 0. }

let compute_offset t ev =
  let dom = t.cb.get_dom (Option.get t.current) in
  let sx, sy = get_scale t.layer in
  let r = dom##getBoundingClientRect in
  t.offset <-
    ( (float_of_int ev##.clientX -. r##.left) /. sx
    , (float_of_int ev##.clientY -. r##.top) /. sy )

let end_drag t ev : unit Lwt.t =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      let sx, sy = get_scale t.layer in
      let r_pg =
        (t.original_parent :> Dom_html.element Js.t)##getBoundingClientRect
      in
      let mx_pg = (float_of_int ev##.clientX -. r_pg##.left) /. sx in
      let my_pg = (float_of_int ev##.clientY -. r_pg##.top) /. sy in
      Dom.appendChild t.original_parent (t.cb.get_dom elt);
      let x = mx_pg -. fst t.offset in
      let y = my_pg -. snd t.offset in
      t.cb.on_move elt x y;
      t.current <- None;
      t.layer##.style##.pointerEvents := Js.string "none";
      Dom_html.document##.body##.classList##remove (Js.string "is-dragging");
      t.cb.on_end elt y;
      Lwt.return_unit

let handle_move t ev _ =
  match t.current with
  | None -> Lwt.return_unit
  | Some elt ->
      if ev##.button = 1
      then end_drag t ev
      else
        let sx, sy = get_scale t.layer in
        let r_layer =
          (t.layer :> Dom_html.element Js.t)##getBoundingClientRect
        in
        let x =
          ((float_of_int ev##.clientX -. r_layer##.left) /. sx) -. fst t.offset
        in
        let y =
          ((float_of_int ev##.clientY -. r_layer##.top) /. sy) -. snd t.offset
        in
        t.cb.on_move elt x y; Lwt.return_unit

let handle_up t ev _ =
  match t.current with None -> Lwt.return_unit | Some _ -> end_drag t ev

let attach_global_listeners t =
  Lwt.async (fun () ->
    Lwt_js_events.mousemoves Dom_html.document (handle_move t));
  Lwt.async (fun () -> Lwt_js_events.mouseups Dom_html.document (handle_up t))

let start_drag t elt ev =
  Dom.preventDefault ev;
  t.current <- Some elt;
  let dom = t.cb.get_dom elt in
  let sx, sy = get_scale t.layer in
  let obj_x, obj_y = t.cb.get_pos elt in
  t.cb.on_start elt obj_x obj_y;
  ignore dom##.offsetWidth;
  dom##.classList##add (Js.string "dragging");
  let r_elt = dom##getBoundingClientRect in
  let r_layer = (t.layer :> Dom_html.element Js.t)##getBoundingClientRect in
  Dom.appendChild t.layer dom;
  dom##.style##.position := Js.string "absolute";
  dom##.style##.left
  := Js.string (Printf.sprintf "%fpx" ((r_elt##.left -. r_layer##.left) /. sx));
  dom##.style##.top
  := Js.string (Printf.sprintf "%fpx" ((r_elt##.top -. r_layer##.top) /. sy));
  compute_offset t ev;
  t.cb.on_move elt
    (((float_of_int ev##.clientX -. r_layer##.left) /. sx) -. fst t.offset)
    (((float_of_int ev##.clientY -. r_layer##.top) /. sy) -. snd t.offset);
  t.layer##.style##.pointerEvents := Js.string "auto";
  Dom_html.document##.body##.classList##add (Js.string "is-dragging");
  Lwt.return_unit

let abort_current_drag (t : 'a t) (elt : 'a) =
  match t.current with
  | Some e when e == elt ->
      let sx, sy = get_scale t.layer in
      let dom = t.cb.get_dom elt in
      dom##.classList##remove (Js.string "dragging");
      let r = dom##getBoundingClientRect in
      let pr =
        (t.original_parent :> Dom_html.element Js.t)##getBoundingClientRect
      in
      let x = (r##.left -. pr##.left) /. sx in
      let y = (r##.top -. pr##.top) /. sy in
      dom##.style##.left := Js.string (Printf.sprintf "%fpx" x);
      dom##.style##.top := Js.string (Printf.sprintf "%fpx" y);
      Dom.appendChild t.original_parent dom;
      t.current <- None;
      t.layer##.style##.pointerEvents := Js.string "none";
      Dom_html.document##.body##.classList##remove (Js.string "is-dragging")
  | _ -> ()

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
