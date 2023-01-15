(lang dune 3.4)

(name tipe)

(generate_opam_files true)

(source
 (github theovidal/tipe))

(authors "Théo Vidal")

(maintainers "Théo Vidal")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tipe)
 (synopsis "School project for CPGE exams")
 (description "Tracing of paths for reconnaissance of an earthquake zone by drone")
 (depends ocaml dune owl-plplot)
 (tags
  (tipe drone "geometric bisection" graph partitioning)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
