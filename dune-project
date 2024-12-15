(lang dune 3.12)

(name compiler)

(generate_opam_files true)

(source
 (github as2397/ocamlCompiler))

(authors "As2397")

(maintainers "As2397")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name compiler)
 (synopsis "A compiler built using OCaml")
 (description "An even longer description of a compiler build using OCaml")
 (depends ocaml dune)
 (tags
  (compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
