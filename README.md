# Pgeon

Pgeon (Prover GEneratiON) is a generic tableau prover for first order logic.

## Getting Started

```bash
opam switch create . 5.1.1    # or another OCaml version supported by dune
opam install --deps-only .    # installs dune, menhir, menhirLib, etc.
dune build                    # builds the library and CLI
```

## Usage

```bash
dune exec pgeon -- logic.txt problem.txt
```

The first argument is a `.pgeon`-style logic specification and the second is a
problem instance containing formulas to prove. Sample fixtures `logic.txt` and
`problem.txt` are provided.

