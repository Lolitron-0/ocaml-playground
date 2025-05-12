type t

module Settings : sig
  type t 
end

val settings : Settings.t ref

val create : Raylib.Vector3.t -> LookDirection.t -> t
val set_sensitivity : float -> unit
val add_position : t -> Raylib.Vector3.t -> unit
val get_native_camera : t -> Raylib.Camera3D.t
val position : t -> Raylib.Vector3.t
val forward_norm : t -> Raylib.Vector3.t
val draw_2d : t -> unit
val update : t -> unit
