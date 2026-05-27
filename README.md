# Pgeon

Pgeon (Prover GEneratiON) is a generic tableau prover for first order logic.

## Getting Started

To build Pgeon, install:
- OCaml (version >= 5.1)
- The [Dune OCaml build system](https://github.com/ocaml/dune/) (version >= 3.19)

Opam (https://opam.ocaml.org/) is the recommended way to install OCaml and the required packages.
```bash
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
Some examples in examples/ directory are provided.

`examples/LK/fo/agatha.txt` is included as an exploratory Dreadbury Mansion
benchmark translated from TPTP `PUZ001-1`; it is not part of the fast example
suite yet because the current search strategy needs stronger branch caching or
subsumption for that CNF-heavy problem.

## Strategy operators

Strategies combine rule calls:

- `a || b` is left-biased choice. It tries `a`; `b` is used only when `a`
  produces no result.
- `a &| b` interleaves both alternatives fairly.
- `a ; b` runs `b` after each result of `a`.
- `a & b` runs `b` after `a` with fair diagonalization.
- `a*` repeats `a` and includes the zero-step result.
- `rule!` applies a supported rule eagerly and returns one result. It fails
  when no premise is applicable. Bang is intentionally unsupported for
  invertible rules (`==>`).

Use `rule!` only for rules whose eager saturation is terminating and useful.
Generative gamma rules such as `forall x. P --> P[fresh]` should usually stay
unbanged; otherwise the same quantified formula can keep producing fresh
instances.

For first-order LK examples, a useful pattern is iterative deepening over gamma
steps followed by propositional saturation:

```text
strategy gamma : rForall || rNotExists
strategy sat : (closure || alpha || delta || beta)*

main strategy prove : gamma* & sat
```

## Tree-rule constraints

Tree rules can constrain captured branch remainders in a `where` block:

```text
tree rule S4 : (not(box(P)); ...B) | ...T --> (not(P); ...B) | ...T
where {
  ...B : box(_)
}
```

The clause `...B : pattern` means every formula captured by `...B` must match
`pattern`. The variable `_` is treated as a wildcard in this constraint.
