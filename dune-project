(lang dune 3.17)

(name towhowcaml)

(package
 (name towhowcaml)
 (allow_empty)
 (depends
  ocaml
  (dune :with-dev-setup)
  utop
  core
  sedlex
  menhir
  yojson
  ppx_yojson_conv
  (qcheck-core :with-test)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
