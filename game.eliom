open Eliom_content.Html.F

let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := Js.string color;
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke

let%server creet_canvas =
  Eliom_content.Html.F.canvas
    ~a:[a_id "creet-canvas"; a_width 1000; a_height 800]
    [txt "Your browser does not support canvas"]

let%client init_client () =
  let canvas = Eliom_content.Html5.To_dom.of_canvas ~%creet_canvas in
  let ctx = canvas##(getContext Dom_html._2d_) in
  ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 12, (10, 10), (200, 100))

let test1 = 42
let%server test2 = 42
let%client test4 = 42
