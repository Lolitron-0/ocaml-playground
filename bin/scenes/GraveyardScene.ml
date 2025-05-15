open Raylib
open Ocaml_playground

module Impl : SceneSign.S = struct
  type t = { common : SceneCommons.t }

  let load () =
    let player =
      Player.create (Vector3.create (-50.) 0. 0.) LookDirection.XPlus
    in
    let pillar_obj =
      Object.create_pro "resources/models/house1/scene.gltf"
        (Vector3.create 0. 0. 0.) true
      |> Object.set_transform (Matrix.scale 200. 200. 200.)
    in
    let objects = [ pillar_obj ] in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 0. (-1.) (-0.3))
           1.0 Color.white
    in
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in
    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path;
    }

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.black;
        DebugUtils.draw_axis ();
        SceneCommons.draw scene.common)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        Player.draw_2d scene.common.player)
      scene.common

  let update scene =
    let common = SceneCommons.update scene.common in
    ({ common }, None)
end

include Impl
