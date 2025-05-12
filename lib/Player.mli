type t

val create : Raylib.Vector3.t -> LookDirection.t -> t
val fpcamera : t -> FPCamera.t
val bbox : t -> Raylib.BoundingBox.t
val get_view : t -> Raylib.Camera3D.t
val draw_2d : t -> unit
val update : t -> t
val undo_movement : t -> t
