open Raylib
open Ocaml_playground

module Impl : SceneSign.S = struct
  type t = { common : SceneCommons.t; sky : Object.t }

  let load () =
    let player =
      Player.create (Vector3.create (-30.) 0. 0.) LookDirection.XPlus
    in
    let statue_obj =
      Object.create_no_collision "resources/models/statue_silence/scene.gltf"
        (Vector3.create 60. 20. 0.)
      |> Object.set_transform
         @@ Matrix.multiply
              (Matrix.rotate_y @@ Utils.deg_to_rad (-90.))
              (Matrix.scale 20. 20. 20.)
    in
    let sky =
      Object.create_pro "resources/models/marble_sphere_black.glb"
        (Vector3.zero ()) true true
      |> Object.set_transform @@ Matrix.scale 300. 300. 300.
      |> Object.set_position (Vector3.create 0. 0. 0.)
    in
    let barriers =
      List.init 6 (fun i ->
          Object.create
            ("resources/models/barriers/barrier" ^ string_of_int i ^ ".glb")
            (Vector3.create 0. 0. 0.))
    in
    let objects = [ statue_obj ] @ barriers in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 0.3 (-1.) 0.7)
           1.0 Color.white
    in
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in
    let ambient_music_path = "resources/audio/music/Ekkehard Ehlers - Earlier_Fruher.mp3" in
    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path ambient_music_path;
      sky;
    }

  let rotate_sky (scene : t) =
    let trig_coeff = 0.0004 in
    let mouse_coeff = 0.001 in
    let mouse_delta = get_mouse_delta () in
    let dx, dy = (Vector2.x mouse_delta, Vector2.y mouse_delta) in
    let t = get_time () in
    let sin, cos = (sin t, cos t) in
    let const_rot = 0.001 in
    let sky =
      Object.apply_transform
        (Matrix.multiply
           (Matrix.rotate_xyz
              (Vector3.create
                 ((trig_coeff *. sin) +. const_rot)
                 ((trig_coeff *. cos) +. const_rot)
                 ((trig_coeff *. sin *. cos) +. const_rot)))
           (Matrix.rotate_xyz
           @@ Vector3.create (dy *. mouse_coeff) (dx *. mouse_coeff)
                ((dx +. dy) /. 2. *. mouse_coeff)))
        scene.sky
    in
    sky

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.black;
        DebugUtils.draw_axis ();
        Object.draw scene.sky;
        SceneCommons.draw scene.common)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        Player.draw_2d scene.common.player)
      scene.common

  let update scene =
    let common = SceneCommons.update scene.common in
    let sky = rotate_sky scene in
    ({ common; sky }, None)
end

include Impl
