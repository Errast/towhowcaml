(lang dune 3.13)
(using ctypes 0.3)

(name towhowcaml)

(generate_opam_files)

(package
 (name towhowcaml)
 (depends ocaml dune utop ppx_yojson_conv ppx_sexp_conv sexplib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
