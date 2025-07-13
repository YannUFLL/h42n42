open Eliom_content.Html.F

let%server game_area =
  div ~a:[a_id "game_area"; a_style "position: relative; width: 1000px; height: 600px;"]
    [ div ~a:[a_id "river"; a_style "height:50px; background:blue;"] []
    ; div ~a:[a_id "playground"; a_style "height:500px; background:white; position:relative;"] []
    ; div ~a:[a_id "hospital"; a_style "height:50px; background:red;"] []
    ]


let%client create_creet id x y =
  let open Js_of_ocaml in
  let playground = Dom_html.getElementById "playground" in
  let creet = Dom_html.createDiv  Dom_html.document in
  creet##.id := Js.string id;
  creet##.className := Js.string "creet";
  creet##.style##.left := Js.string (Printf.sprintf "%dpx" x);
  creet##.style##.top := Js.string (Printf.sprintf "%dpx" y);
  Dom.appendChild playground creet;
  creet

  let%client init_client () =
  let _ = create_creet "creet1" 100 100 in
  ()

let%client print_coucou () = print_int 42
