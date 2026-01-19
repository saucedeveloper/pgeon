# Pgeon

Pgeon (Prover GEneratiON) is a generic tableau prover for first order logic.

## Getting Started

To build Pgeon, install:
- OCaml (version >= 5.1)
- The [Menhir parser generator](https://opam.ocaml.org/packages/menhir/) (version >= 20201201)
- The [Dune OCaml build system](https://github.com/ocaml/dune/) (version >= 3.19)

Opam (https://opam.ocaml.org/) is the recommended way to install OCaml and the required packages.
```bash
opam switch create . 5.1.1
eval $(opam env)
opam install dune menhir
```

## Usage

Build and run Pgeon with Dune:
```bash
eval $(opam env) # (only needed in a fresh shell)
dune exec pgeon -- examples/LK/logic.txt examples/LK/problem.txt
```

The first argument is a `.pgeon`-style logic specification and the second is a problem instance containing formulas to prove.
Some examples in examples/ directory are provided.

