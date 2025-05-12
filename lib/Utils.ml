open Raylib

let vec3_to_string v =
  Printf.sprintf "(%f, %f, %f)" (Vector3.x v) (Vector3.y v) (Vector3.z v)

let vec2_to_string v = Printf.sprintf "(%f, %f)" (Vector2.x v) (Vector2.y v)

  let xzy_to_xyz_transform scale =
    let deg_to_rad = Float.pi /. 180. in
    Matrix.multiply
      (Matrix.rotate_x @@ (deg_to_rad *. -90.))
      (Matrix.scale scale scale scale)

