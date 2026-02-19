type t =
  | Skip
  | Fail
  | Rule of int
  | Bang of int
  | Call of string
  | AndThen of t * t
  | OrElse of t * t
  | Repeat of t
