(lang dune 3.13)

(using ctypes 0.3)

(name towhowcaml)

(generate_opam_files)

(map_workspace_root false)

(package
 (name towhowcaml)
 (depends ocaml dune utop ppx_yojson_conv core))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
