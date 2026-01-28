type limit_bound =
  | LimitConst of int
  | LimitDepth

type t =
  | Skip
  | Fail
  | Rule of int
  | Call of string
  | AndThen of t * t
  | OrElse of t * t
  | Repeat of t
  | Limit of limit_bound * t
  | Depth of t
  | DepthIter of int * t
  | PopLimit
  | PopDepth
