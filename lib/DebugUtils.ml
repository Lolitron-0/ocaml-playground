open Raylib
open Core

let (watches : (unit -> string) list ref) = ref []
let add_watch f = watches := f :: !watches

let update () =
  let strings = List.map !watches ~f:(fun w -> w ()) in
  for i = 0 to List.length strings - 1 do
    let font_size = 10 in
    let offset_y = i * font_size in
    let offset_x =
      strings
      |> List.map ~f:(fun s -> measure_text s font_size)
      |> List.max_elt ~compare |> Option.value_exn
    in
    draw_text (List.nth_exn strings i)
      (get_screen_width () - offset_x)
      offset_y font_size Color.gray
  done

let draw_axis () =
  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 1. 0. 0.)) Color.red;

  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 0. 1. 0.)) Color.blue;

  draw_ray (Ray.create (Vector3.zero ()) (Vector3.create 0. 0. 1.)) Color.green
