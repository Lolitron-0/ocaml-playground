open Raylib

let player_height = 20.
let player_width = 10.

type t = {
  old_position : Vector3.t;
  position : Vector3.t;
  fpcamera : FPCamera.t;
  speed : float;
  bbox : BoundingBox.t;
}

let get_view player = FPCamera.get_native_camera player.fpcamera
let fpcamera player = player.fpcamera
let bbox player = player.bbox

let calc_bbox position =
  let bot =
    Vector3.subtract position (Vector3.create player_width 0. player_width)
  in
  let top =
    Vector3.add position
      (Vector3.create player_width player_height player_width)
  in
  BoundingBox.create bot top

let create initial_position look_dir =
  let camera =
    FPCamera.create
      (Vector3.add initial_position (Vector3.create 0. player_height 0.))
      look_dir
  in
  let bbox = calc_bbox initial_position in
  {
    position = initial_position;
    old_position = initial_position;
    fpcamera = camera;
    speed = 1.;
    bbox;
  }

let draw_2d (player : t) =
  let fpcamera = player.fpcamera in
  let camera = FPCamera.get_native_camera fpcamera in
  FPCamera.draw_2d fpcamera;
  let target =
    Vector3.add (FPCamera.position fpcamera) (FPCamera.forward_norm fpcamera)
  in
  let target_screen_coords = get_world_to_screen target camera in
  let screen_image = load_image_from_screen () in
  image_color_invert @@ addr screen_image;
  let under_cursor_color_inv =
    get_image_color screen_image
      (Int.of_float @@ Vector2.x target_screen_coords)
      (Int.of_float @@ Vector2.y target_screen_coords)
  in
  draw_circle_v target_screen_coords 2. under_cursor_color_inv;
  unload_image screen_image

let update (player : t) =
  let forward_xz =
    Vector3.multiply
      (FPCamera.forward_norm player.fpcamera)
      (Vector3.create 1. 0. 1.)
  in
  let right = Vector3.cross_product forward_xz (Vector3.create 0. 1. 0.) in
  let vel_forward =
    if is_key_down Key.W then Vector3.scale forward_xz player.speed
    else Vector3.zero ()
  in
  let vel_backward =
    if is_key_down Key.S then Vector3.scale forward_xz (-.player.speed)
    else Vector3.zero ()
  in
  let vel_right =
    if is_key_down Key.D then Vector3.scale right player.speed
    else Vector3.zero ()
  in
  let vel_left =
    if is_key_down Key.A then Vector3.scale right (-.player.speed)
    else Vector3.zero ()
  in
  let velocity =
    Vector3.zero () |> Vector3.add vel_forward |> Vector3.add vel_backward
    |> Vector3.add vel_right |> Vector3.add vel_left
  in
  let velocity =
    if is_key_down Key.Left_shift then Vector3.scale velocity 5. else velocity
  in
  Vector3.set_y velocity 0.;
  let old_position = player.position in
  let position = Vector3.add player.position velocity in
  let bbox = calc_bbox position in

  FPCamera.add_position player.fpcamera velocity;
  FPCamera.update player.fpcamera;
  { player with position; bbox; old_position }

let undo_movement player =
  let delta = Vector3.subtract player.old_position player.position in
  FPCamera.add_position player.fpcamera delta;
  { player with position = player.old_position }
