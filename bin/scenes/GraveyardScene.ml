open Raylib
open Ocaml_playground

let randomize_frame_counter = ref 0

module Impl : SceneSign.S = struct
  type t = {
    common : SceneCommons.t;
    sky : Object.t;
    flying_stuff : Object.t list;
  }

  let randomize (obj : Object.t) =
    let float_in_range min max = Random.float (max -. min) +. min in
    let x = float_in_range 0. 50. in
    let y = float_in_range 0. 30. in
    let z = float_in_range (-50.) 50. in
    let angle = Utils.deg_to_rad @@ float_in_range 0. 360. in
    let new_obj =
      obj
      |> Object.set_position (Vector3.create x y z)
      |> Object.apply_transform
           (Matrix.rotate_xyz (Vector3.create angle angle angle))
    in
    new_obj

  let load () =
    let player =
      Player.create "resources/audio/sounds/steps_barefeet"
        (Vector3.create (-30.) 0. 0.)
        LookDirection.XPlus
    in
    let statue_obj =
      Object.create "resources/models/graveyard/statue_silence.glb"
        (Vector3.create 60. 10. 0.)
      |> Object.apply_transform @@ Matrix.scale 20. 20. 20.
      |> Object.apply_transform @@ Matrix.rotate_y @@ Utils.deg_to_rad (-90.)
    in
    let floor =
      Object.create_pro "resources/models/marble_floor_black.glb"
        (Vector3.zero ()) true false
      |> Object.set_transform @@ Matrix.scale 100. 100. 100.
    in
    let g_model_name name = "resources/models/graveyard/" ^ name ^ ".glb" in
    let apply_std scale =
      Object.set_transform @@ Utils.xzy_to_xyz_transform scale
    in
    let rot_y ang =
      Object.apply_transform @@ Matrix.rotate_y @@ Utils.deg_to_rad ang
    in
    let std_rot_y ang scale = fun o -> o |> apply_std scale |> rot_y ang in
    let scale_rot_y ang scale =
     fun o ->
      o |> Object.apply_transform @@ Matrix.scale scale scale scale |> rot_y ang
    in
    let rot_move_up ang dist =
     fun o ->
      o |> rot_y ang |> Object.apply_transform @@ Matrix.translate 0. dist 0.
    in
    let std_rot_move_up scale ang dist =
     fun o -> o |> apply_std scale |> rot_move_up ang dist
    in
    let graveyard_opts =
      [
        (g_model_name "gravestone1", apply_std 1.5);
        (g_model_name "gravestone1_alt", apply_std 1.5);
        (g_model_name "gravestone2", scale_rot_y (-120.) 0.5);
        (g_model_name "gravestone3", scale_rot_y (-90.) 2.);
        (g_model_name "graveplate", std_rot_y 180. 2.);
        (g_model_name "cross1", std_rot_y 180. 2.);
        (g_model_name "cross2", rot_move_up (-90.) 0.7);
        (g_model_name "cross3", rot_move_up (-90.) 0.7);
        (g_model_name "big_tomb1", std_rot_y 90. 2.);
        ( g_model_name "big_tomb2",
          fun o -> o |> scale_rot_y (-90.) 2. |> rot_move_up 0. 0.7 );
        (g_model_name "angel_statue2", std_rot_move_up 2. (-100.) 3.0);
      ]
    in
    print_int @@ List.length graveyard_opts;
    let graveyard =
      [
        Object.create (g_model_name "angel_statue2") (Vector3.create 0. 0. 0.)
        |> std_rot_move_up 2. (-100.) 3.0;
      ]
    in
    let objects = [ statue_obj; floor ] @ graveyard in
    let sky =
      Object.create_pro "resources/models/marble_sphere_black.glb"
        (Vector3.zero ()) true true
      |> Object.set_transform @@ Matrix.scale 300. 300. 300.
      |> Object.set_position (Vector3.create 0. 0. 0.)
    in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 0.3 (-1.) 0.7)
           0.5
           (Color.create 180 180 230 255)
    in
    let barriers =
      List.init 6 (fun i ->
          Object.create_pro
            ("resources/models/barriers/barrier" ^ string_of_int i ^ ".glb")
            (Vector3.create 0. (-100.) 0.)
            false false
          |> Object.set_transform @@ Matrix.scale 3. 3. 3.
          |> randomize)
    in
    let flying_stuff = barriers in
    List.iter
      (Object.apply_shader @@ LightingSystem.shader lighting)
      flying_stuff;
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in
    let ambient_music_path =
      "resources/audio/music/Joseph Suchy - Soan-Ne.mp3"
    in
    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path
          ambient_music_path;
      sky;
      flying_stuff;
    }

  let unload (scene : t) =
    SceneCommons.destroy scene.common;
    Object.destroy scene.sky;
    List.iter Object.destroy scene.flying_stuff

  let rotate_sky (sky : Object.t) =
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
        sky
    in
    sky

  let randomize_flying_stuff (stuff : Object.t list) =
    let randomize_frame_counter_max = 90 in
    let const_rot = 0.003 in
    randomize_frame_counter := !randomize_frame_counter + 1;
    let do_randomize =
      !randomize_frame_counter >= randomize_frame_counter_max
    in
    if do_randomize then randomize_frame_counter := 0;
    List.map
      (fun obj ->
        if do_randomize then (
          randomize_frame_counter := 0;
          randomize obj)
        else
          obj
          |> Object.apply_transform
               (Matrix.rotate_xyz
               @@ Vector3.create const_rot const_rot const_rot))
      stuff

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.black;
        DebugUtils.draw_axis ();
        List.iter Object.draw scene.flying_stuff;
        Object.draw scene.sky;
        SceneCommons.draw scene.common)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        Player.draw_2d scene.common.player)
      scene.common

  let update scene =
    let sky = rotate_sky scene.sky in
    let flying_stuff = randomize_flying_stuff scene.flying_stuff in
    let common = scene.common |> SceneCommons.update in
    ({ common; sky; flying_stuff }, None)
end

include Impl
