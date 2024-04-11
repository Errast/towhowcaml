(lang dune 3.13)

(name towhowcaml)

(generate_opam_files)

(map_workspace_root false)

(package
 (name radatnet)
 (depends ocaml dune utop ppx_yojson_conv core))

(package
 (name mir)
 (depends ocaml dune utop core))

(package
 (name towhowcaml)
 (depends ocaml dune utop core))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
