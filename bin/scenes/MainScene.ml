open Raylib
open Ocaml_playground

module Impl : SceneSign.S = struct
  type t = { common : SceneCommons.t }

  let load () =
    let player =
      Player.create (Vector3.create (-50.) 0. 0.) LookDirection.XPlus
    in
    let pillar_obj =
      Object.create "resources/models/doric_pillar.glb"
        (Vector3.create 0. 0. 50.)
    in
    let pillars =
      List.init 10 (fun i ->
          [
            Object.set_position
              (Vector3.create (float i *. 50.) 0. 50.)
              pillar_obj;
            Object.set_position
              (Vector3.create (float i *. 50.) 0. (-50.))
              pillar_obj;
          ])
      |> List.flatten
    in
    Object.set_transform (Utils.xzy_to_xyz_transform 1.) pillar_obj;
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 1. (-1.) (-1.))
           Color.white
    in
    let objects = pillars in
    let postprocess_shader_path = "resources/shaders/bloom.fs" in
    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path;
    }

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.raywhite;
        (* draw_grid 1000 5.0; *)
        (* DebugUtils.draw_axis (); *)
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
