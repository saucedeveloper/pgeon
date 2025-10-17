(lang dune 3.19)

(name pgeon)

(using menhir 3.0)

(authors "Romain Sidhoum <romain.sidhoum@umontpellier.fr>")

(package
 (name pgeon)
 (depends (ocaml (>= 5.1)) (menhir (>= 20201201)))
 )

(map_workspace_root false)
