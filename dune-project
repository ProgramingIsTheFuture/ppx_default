(lang dune 3.10)

(name ppx_default)

(generate_opam_files true)

(authors "Francisco Santos")
(maintainers "franciscosantosfcp@hotmail.com")
(homepage "https://github.com/ProgramingIsTheFuture/ppx_default")
(bug_reports "https://github.com/ProgramingIsTheFuture/ppx_default/issues")
(source (github ProgramingIsTheFuture/ppx_default))
(license MIT)

(package
 (name ppx_default)
 (synopsis "Generate default values for your types")
 (depends
  (ocaml (>= 4.14))
  (ppxlib (>= 0.25))
  ppx_deriving))
