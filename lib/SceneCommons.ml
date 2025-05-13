open Raylib

type t = {
  objects : Object.t list;
  lighting : LightingSystem.t;
  player : Player.t;
  postprocess_shader : Shader.t;
}

let (render_texture : RenderTexture.t option ref) = ref None

let prevent_player_collision objects player =
  let player = player in
  if List.exists (fun obj -> Object.collides_with player obj) objects then
    Player.undo_movement player
  else player

let create objects lighting player postprocess_shader_path =
  let shader = LightingSystem.shader lighting in
  List.iter (fun obj -> Object.apply_shader shader obj) objects;
  let postprocess_shader = load_shader "" postprocess_shader_path in
  let render_width_loc = get_shader_location postprocess_shader "renderWidth" in
  let render_height_loc =
    get_shader_location postprocess_shader "renderHeight"
  in
  let render_width_ptr =
    to_voidp @@ Ctypes.allocate Ctypes.float (float @@ get_screen_width ())
  in
  let render_height_ptr =
    to_voidp @@ Ctypes.allocate Ctypes.float (float @@ get_screen_height ())
  in
  set_shader_value postprocess_shader render_width_loc render_width_ptr
    ShaderUniformDataType.Float;
  set_shader_value postprocess_shader render_height_loc render_height_ptr
    ShaderUniformDataType.Float;
  { objects; lighting; player; postprocess_shader }

let destroy (data : t) =
  List.iter Object.destroy data.objects;
  unload_shader data.postprocess_shader

let init () =
  render_texture :=
    Some (load_render_texture (get_screen_width ()) (get_screen_height ()))

let get_render_texture () =
  match !render_texture with
  | None -> failwith "render texture not initialized"
  | Some rt -> rt

let draw (scene : t) =
  let lighting = scene.lighting in
  let player = scene.player in

  LightingSystem.begin_system lighting;
  LightingSystem.update_shader lighting (Player.get_view player);
  List.iter (fun o -> Object.draw o) scene.objects;
  LightingSystem.end_system ()

let render_to_texture (draw_f : unit -> unit) scene =
  begin_texture_mode (get_render_texture ());
  let native_camera = Player.get_view scene.player in
  begin_mode_3d native_camera;
  draw_f ();
  end_mode_3d ();
  end_texture_mode ()

let render_to_screen (draw_overlay_f : unit -> unit) scene =
  begin_drawing ();
  clear_background Color.raywhite;

  begin_shader_mode scene.postprocess_shader;
  let texture2d = RenderTexture.texture @@ get_render_texture () in
  draw_texture_rec texture2d
    (Rectangle.create 0. 0.
       (float @@ Texture2D.width texture2d)
       (float @@ -Texture2D.height texture2d))
    (Vector2.zero ()) Color.white;

  end_shader_mode ();
  draw_overlay_f ();
  end_drawing ()

let update (scene : t) =
  let player =
    Player.update scene.player |> prevent_player_collision scene.objects
  in
  { scene with player }
