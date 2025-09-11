type name = int
type t = Bvar of int | Fvar of name | App of name * t list | Bind of name * t

let rec equal a b =
  match (a, b) with
  | Bvar i, Bvar j -> i = j
  | Fvar i, Fvar j -> i = j
  | App (fn, args), App (fn', args') ->
      fn = fn' && List.length args = List.length args'
      && List.for_all2 equal args args'
  | Bind (name, t), Bind (name', t') -> name = name' && equal t t'
  | _ -> false

let rec occurs n = function
  | Bvar _ -> false
  | Fvar n' -> n = n'
  | App (_, p) -> List.fold_left (fun acc t -> acc || occurs n t) false p
  | Bind (_, t) -> occurs n t

let var_open u t =
  let rec var_open k u = function
    | Bvar i as t -> if i = k then u else t
    | Fvar _ as t -> t
    | App (name, tl) -> App (name, List.map (var_open k u) tl)
    | Bind (name, t) -> Bind (name, var_open (k + 1) u t)
  in
  snd (List.fold_left (fun (i, t) u -> (i + 1, var_open i u t)) (0, t) u)

let rec substitute subs = function
  | Bvar i -> Bvar i
  | Fvar x -> ( match List.assoc_opt x subs with None -> Fvar x | Some u -> u)
  | App (name, tl) -> App (name, List.map (substitute subs) tl)
  | Bind (name, t) -> Bind (name, substitute subs t)

let p_match t t' =
  let exception UnifyFailure in
  let rec p_match dt t t' =
    match (t, t') with
    | [], [] -> dt
    | t :: tl, t' :: tl' when t = t' -> p_match dt tl tl'
    | App (f, p) :: tl, App (f', p') :: tl' ->
        if f = f' && List.length p = List.length p' then
          p_match dt (p @ tl) (p' @ tl')
        else raise UnifyFailure
    | Fvar n :: tl, t :: tl' ->
        if occurs n t then raise UnifyFailure
        else
          let dt =
            match List.assoc_opt n dt with
            | Some existing ->
                if equal existing t then dt else raise UnifyFailure
            | None ->
                (n, t)
                :: List.map
                     (fun (n', t') -> (n', substitute [ (n, t) ] t'))
                     dt
          in
          p_match dt tl tl'
    | Bind (b, t) :: tl, Bind (b', t') :: tl' ->
        if b = b' then p_match dt (t :: tl) (t' :: tl') else raise UnifyFailure
    | [], _ | _, [] | Bvar _ :: _, _ | App _ :: _, _ | Bind _ :: _, _ ->
        raise UnifyFailure
  in
  try Some (p_match [] t t') with UnifyFailure -> None

let memo = Lru.create 512

let memo_match t t' =
  match Lru.get memo (t, t') with
  | Some sigma -> sigma
  | None ->
      let sigma = p_match t t' in
      Lru.put memo (t, t') sigma;
      sigma

let p_match = memo_match
