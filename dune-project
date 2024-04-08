(lang dune 3.11)

(name typelang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name typelang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml 
    (>= 5.1.1)) 
  dune
  (llvm
    (>= 16.0.6))
  (conf-llvm
    (>= 16)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
