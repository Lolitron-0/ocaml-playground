open Raylib

type t = {
  model : Model.t;
  position : Vector3.t;
  bbox : BoundingBox.t;
  no_collide : bool;
  no_light : bool;
}

let shift_bbox delta bbox =
  let min = BoundingBox.min bbox in
  let max = BoundingBox.max bbox in
  BoundingBox.create (Vector3.add min delta) (Vector3.add max delta)

let apply_shader shader obj =
  if obj.no_light then ()
  else
    CArray.iter
      (fun mat -> Material.set_shader mat shader)
      (Model.materials obj.model)

let create_pro path_to_model position no_collide no_light =
  let model = load_model path_to_model in
  let bbox = get_model_bounding_box model |> shift_bbox position in
  { model; position; bbox; no_collide; no_light }

let create path_to_model position =
  create_pro  path_to_model position false false

let create_no_collision path_to_model position =
  create_pro  path_to_model position true false

let destroy obj = 
  unload_model obj.model

let set_transform transform obj =
  Model.set_transform obj.model transform;
  let bbox = get_model_bounding_box obj.model |> shift_bbox obj.position in
  { obj with bbox }

let apply_transform transform obj =
  let new_mat = Matrix.multiply (Model.transform obj.model) transform in
  set_transform new_mat obj

let set_position position obj =
  let delta = Vector3.subtract position obj.position in
  let bbox = shift_bbox delta obj.bbox in
  { obj with position; bbox }

let collides_with player obj =
  match obj.no_collide with
  | true -> false
  | false -> check_collision_boxes obj.bbox (Player.bbox player)

let draw obj = 
  draw_model obj.model obj.position 1. Color.white
