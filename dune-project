(lang dune 3.13)

(name towhowcaml)

(generate_opam_files)

(package
 (name towhowcaml)
 (depends ocaml dune utop core sedlex menhir))

(using menhir 3.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
