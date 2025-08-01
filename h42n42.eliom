(* This file was generated by Eliom-distillery. Feel free to use it, modify it,
   and redistribute it as you wish. *)

let%server application_name = "h42n42"
let%client application_name = Eliom_client.get_application_name ()

let%server () =
  Ocsipersist_settings.set_db_file "local/var/data/h42n42/h42n42_db"

(* Create a module for the application. See
   https://ocsigen.org/eliom/manual/clientserver-applications for more
   information. *)
module%shared App = Eliom_registration.App (struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)

(* As the headers (stylesheets, etc) won't change, we ask Eliom not to update
   the <head> of the page when changing page. (This also avoids blinking when
   changing page in iOS). *)
let%client _ = Eliom_client.persist_document_head ()

let%server main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client () = print_endline "Hello"

[%%client
let init_client () =
  let open Js_of_ocaml in
  Lwt.async (fun () -> Main_page.show_tutorial_pages 0);
  let btn_dom : Dom_html.divElement Js.t =
    Dom_html.getElementById "start-button"
  in
  Game.set_on_game_end_callback (fun () -> Main_page.reset_game ());
  ignore
    (Js_of_ocaml.Dom_html.addEventListener btn_dom
       Js_of_ocaml.Dom_html.Event.click
       (Js_of_ocaml.Dom_html.handler (fun _ev ->
          Js_of_ocaml.Firebug.console##log (Js.string "clicked !");
          Main_page.lock_settings_panel true;
          Main_page.update_settings_from_inputs ();
          let btn_dom = Js_of_ocaml.Dom_html.getElementById "start-button" in
          btn_dom##.textContent := Js.some (Js.string "Relaunch simulation");
          Game.init_client ();
          Main_page.timer_running := true;
          let start_time =
            (Js_of_ocaml.Js.Unsafe.eval_string "Date.now()" : float) /. 1000.
          in
          Lwt.async (fun () -> Main_page.show_timer start_time);
          Js._false))
       Js._false);
  Lwt.return_unit]

let%server () =
  App.register ~service:main_service (fun () () ->
    let _ = [%client (init_client () : unit Lwt.t)] in
    Lwt.return (Main_page.main_page ()))
