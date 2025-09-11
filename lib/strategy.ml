type t =
  | Skip
  | Fail
  | Rule of int
  | AndThen of t * t
  | OrElse of t * t
  | Repeat of t
