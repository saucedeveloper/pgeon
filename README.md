# Pgeon

Pgeon (Prover GEneratiON) is a generic tableau prover for first order logic.

## Getting Started

To build Pgeon, install:
- OCaml (version >= 5.1)
- The [Dune OCaml build system](https://github.com/ocaml/dune/) (version >= 3.19)

Opam (https://opam.ocaml.org/) is the recommended way to install OCaml and the required packages.
```bash
git clone https://gite.lirmm.fr/rsidhoum/pgeon/
cd pgeon
opam switch create . 5.1.1
eval $(opam env)
opam install dune
```

## Usage

Build and run Pgeon with Dune:
```bash
eval $(opam env) # (only needed in a fresh shell)
dune exec pgeon -- examples/LK/fo/lk_fo.txt examples/LK/fo/drinker.txt
```

The first argument is a `.pgeon`-style logic specification and the second is a problem instance containing formulas to prove.

