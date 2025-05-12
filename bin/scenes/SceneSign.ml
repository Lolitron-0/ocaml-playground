open Ocaml_playground

module type S = Scene.S with type id := SceneEnumerator.t

type t = (module S)
