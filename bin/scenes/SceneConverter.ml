let to_module (id : SceneEnumerator.t) =
  match id with
  | MainScene -> (module MainScene : SceneSign.S)
  | GraveyardScene -> (module GraveyardScene : SceneSign.S)
