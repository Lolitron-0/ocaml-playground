open Raylib
open Ocaml_playground

module Impl : SceneSign.S = struct
  type t = {
    common : SceneCommons.t;
    picture_tex : Texture2D.t;
    picture_pos : Vector3.t;
  }

  let load () =
    let player =
      Player.create (Vector3.create (-50.) 0. 0.) LookDirection.XPlus
    in
    let pillar_obj =
      Object.create "resources/models/doric_pillar.glb"
        (Vector3.create 0. 0. 0.)
      |> Object.set_transform (Utils.xzy_to_xyz_transform 1.)
    in
    let pillars =
      List.init 20 (fun i ->
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
    let floor =
      Object.create "resources/models/marble_floor.glb" (Vector3.zero ())
      |> Object.set_transform @@ Matrix.scale 1000. 1000. 1000.
      |> Object.set_position (Vector3.create 0. (-0.1) 0.)
    in
    let picture_tex = load_texture "resources/textures/picture1.png" in
    let picture_pos = Vector3.create 450. 20. 0. in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 1. (-1.) (-1.))
           Color.white
      |> LightingSystem.add_point_light picture_pos Color.orange
    in
    let objects = pillars @ [ floor ] in
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in

    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path;
      picture_tex;
      picture_pos;
    }

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.raywhite;
        DebugUtils.draw_axis ();
        SceneCommons.draw scene.common;
        let camera = Player.get_view scene.common.player in
        draw_billboard camera scene.picture_tex scene.picture_pos 10.
          Color.white)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        Player.draw_2d scene.common.player)
      scene.common

  let update scene =
    let common = SceneCommons.update scene.common in
    let picture_pos_y = (sin @@ get_time () *. 2.0) *. 0.04 in
    Vector3.set_y scene.picture_pos
      (Vector3.y scene.picture_pos +. picture_pos_y);
    ({ scene with common }, None)
end

include Impl
