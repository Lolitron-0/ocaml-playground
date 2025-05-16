(lang dune 3.18)

(name ocaml-playground)

(generate_opam_files true)

(source
 (github username/reponame))

(license LICENSE)

(package
 (name ocaml-playground)
 (synopsis "OCaml playground")
 (depends ocaml
          raylib
          raygui
          (ocaml-lsp-server :dev)
          (ocamlformat :dev))
 (tags
  ("game" "3d" "raylib")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
