open Raylib

type t = { model : Model.t; position : Vector3.t; bbox : BoundingBox.t }

let apply_shader shader obj =
  CArray.iter
    (fun mat -> Material.set_shader mat shader)
    (Model.materials obj.model)

let create path_to_model position =
  let model = load_model path_to_model in
  let bbox = get_model_bounding_box model in
  { model; position; bbox }

let destroy obj = 
  unload_model obj.model

let set_transform transform obj = Model.set_transform obj.model transform
let set_position position obj = { obj with position }

let collides_with player obj =
  check_collision_boxes obj.bbox (Player.bbox player)

let draw obj =
  draw_model obj.model obj.position 1. Color.white
