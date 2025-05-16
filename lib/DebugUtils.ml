open Raylib

let draw_axis () =
  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 1. 0. 0.)) Color.red;

  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 0. 1. 0.)) Color.blue;

  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 0. 0. 1.)) Color.green
