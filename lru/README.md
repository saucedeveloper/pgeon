# Lru

A small LRU cache implementation for OCaml values.
Recency is tracked by a triangular bit matrix.

## Features

- Generic key/value cache.
- No external dependencies beyond OCaml and C compiler.

## Build

In your dune file add lru as a library dependency
```dune
(executable
 (name main)
 (libraries lru)
 (modes native byte)
)
```
## Usage
```ocaml
let () =
  (* Create a cache of capacity 3 *)
  let cache = Lru.create 3 in

  (* insert some bindings *)
  let _ = Lru.put cache "apple" 1 in
  let _ = Lru.put cache "banana" 2 in
  let _ = Lru.put cache "pear" 3 in

  (* access to bump recency *)
  let _ = Lru.get cache "apple" in

  (* insert a fourth binding -> evicts the LRU ("banana") *)
  let _ = Lru.put cache "cherry" 4 in

  (* test membership *)
  List.iter
    (fun key ->
      match Lru.get cache key with
      | Some v -> Printf.printf "%s -> %d\n" key v
      | None -> Printf.printf "%s <deleted>\n" key)
    [ "apple"; "banana"; "pear"; "cherry" ]
```

```bash
apple -> 1
banana <deleted>
pear -> 3
cherry -> 4
```

