open Raylib

module Settings = struct
  type t = {
     sensitivity : float;
    sensitivity_coefficient : float;
    pitch_clamp_angle_range : float;
  }
end

let settings =
  ref
    {
      Settings.sensitivity = 0.5;
      sensitivity_coefficient = 0.01;
      pitch_clamp_angle_range = 45.;
    }

type t = { camera : Camera3D.t }

let target_from_look_dir pos look_dir =
  let target =
    Vector3.add pos
      (match look_dir with
      | LookDirection.XPlus -> Vector3.create 1. 0. 0.
      | LookDirection.XMinus -> Vector3.create (-1.) 0. 0.
      | LookDirection.ZPlus -> Vector3.create 0. 0. 1.
      | LookDirection.ZMinus -> Vector3.create 0. 0. (-1.))
  in
  target

let create position look_dir =
  {
    camera =
      Camera3D.create position
        (target_from_look_dir position look_dir)
        (Vector3.create 0. 1. 0.) 70. CameraProjection.Perspective;
  }

let set_sensitivity sensitivity = settings := { !settings with sensitivity }
let get_native_camera fpcam = fpcam.camera
let position fpcam = Camera3D.position fpcam.camera

let add_position fpcam pos_delta =
  let camera = fpcam.camera in
  Camera3D.set_position camera @@ Vector3.add (position fpcam) pos_delta;
  Camera3D.set_target camera @@ Vector3.add (Camera3D.target camera) pos_delta

let get_view_vec camera =
  Vector3.subtract (Camera3D.target camera) (Camera3D.position camera)

let get_forward_norm camera = Vector3.normalize @@ get_view_vec camera
let forward_norm fpcam = get_forward_norm fpcam.camera

let get_right camera =
  Vector3.cross_product (Camera3D.up camera) (get_forward_norm camera)

let rot_yaw fpcam angle =
  let camera = fpcam.camera in
  let view_vec = get_view_vec camera in
  let new_view_vec =
    Vector3.rotate_by_quaternion view_vec
      (Vector4.from_axis_angle (Camera3D.up camera) angle)
  in
  Camera3D.set_target camera
    (Vector3.add (Camera3D.position camera) new_view_vec)

let angle_will_fit camera angle_delta =
  let angle_up =
    acos @@ Vector3.dot_product (get_forward_norm camera) (Camera3D.up camera)
  in
  let deg_to_rad = Float.pi /. 180. in
  let clamp_high = deg_to_rad *. (90. +. (!settings).pitch_clamp_angle_range) in
  let clamp_low = deg_to_rad *. (90. -.  (!settings).pitch_clamp_angle_range) in
  angle_up +. angle_delta <= clamp_high && angle_up +. angle_delta >= clamp_low

let rot_pitch fpcam angle =
  match angle_will_fit fpcam.camera angle with
  | false -> ()
  | true ->
      let camera = fpcam.camera in
      let view_vec = get_view_vec camera in
      let right = get_right camera in
      let new_view_vec =
        Vector3.rotate_by_quaternion view_vec
          (Vector4.from_axis_angle right angle)
      in
      Camera3D.set_target camera
        (Vector3.add (Camera3D.position camera) new_view_vec)

let update (fpcam : t) =
  let mouse_delta = get_mouse_delta () in
  let sens = (!settings).sensitivity in
  rot_yaw fpcam
    (Vector2.x mouse_delta *. -1. *. sens
   *. (!settings).sensitivity_coefficient);
  rot_pitch fpcam
    (Vector2.y mouse_delta *. sens *. (!settings).sensitivity_coefficient)

let draw_2d (fpcam : t) =
  let camera = fpcam.camera in
  let target = Camera3D.target camera in
  draw_text
    (Printf.sprintf "pos: %s; target: %s"
       (Utils.vec3_to_string @@ position fpcam)
       (Utils.vec3_to_string target))
    10 10 10 Color.red
(* let target_screen_coords = get_world_to_screen target camera in *)
(* let screen_image = load_image_from_screen () in *)
(* image_color_invert @@ addr screen_image; *)
(* let under_cursor_color = *)
(*   get_image_color screen_image *)
(*     (Int.of_float @@ Vector2.x target_screen_coords) *)
(*     (Int.of_float @@ Vector2.y target_screen_coords) *)
(* in *)
(* draw_text *)
(*   (Printf.sprintf "%i %i %i" *)
(*      (Color.r under_cursor_color) *)
(*      (Color.g under_cursor_color) *)
(*      (Color.b under_cursor_color)) *)
(*   10 20 10 Color.white; *)
(* draw_circle_v target_screen_coords 1. under_cursor_color; *)
(* draw_sphere target 0.1 Color.red *)
