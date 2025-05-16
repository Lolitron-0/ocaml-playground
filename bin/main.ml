open Ocaml_playground
open Scenes
open Raylib

let width = 1200
let height = 800

module State = struct
  type t = { scene_module : SceneSign.t; scene_data : bytes }
end

let setup () =
  set_config_flags [ ConfigFlags.Msaa_4x_hint ];
  init_window width height "playground";
  SceneCommons.init ();
  set_target_fps 60;
  disable_cursor ();
  FPCamera.set_sensitivity 0.5;
  let (module StartSceneHandler) = (module GraveyardScene : SceneSign.S) in
  let scene_data = StartSceneHandler.load () in
  let state =
    State.
      {
        scene_module = (module StartSceneHandler);
        scene_data = Obj.magic scene_data;
      }
  in
  state

let rec loop (scene : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let (module SceneHandler) = scene.scene_module in
      let scene_data, new_scene_opt =
        SceneHandler.update @@ Obj.magic scene.scene_data
      in
      SceneHandler.draw scene_data;
      let scene_module, scene_data =
        match new_scene_opt with
        | None -> (scene.scene_module, scene_data)
        | Some new_scene_id ->
            let (module Handler) = SceneConverter.to_module new_scene_id in
            let scene_data = Handler.load () in
            ((module Handler), Obj.magic scene_data)
      in
      loop { scene_module; scene_data = Obj.magic scene_data }

let () = setup () |> loop
