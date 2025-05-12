type t = {
  load : unit -> t;
  draw : t -> unit;
  update : t -> t;
 }
