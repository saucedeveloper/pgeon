module Mat = struct
  type t

  external create : int -> t = "caml_mat_create"
  external access : t -> int -> unit = "caml_mat_access"
  external get_lru : t -> int = "caml_mat_get_lru"
end

type ('k, 'v) t = {
  capacity : int;
  mat : Mat.t;
  index_of_key : ('k, int) Hashtbl.t;
  keys : 'k array;
  values : 'v array;
  mutable size : int;
}

let create capacity =
  if capacity <= 0 then invalid_arg "Lru.create";
  {
    capacity;
    mat = Mat.create capacity;
    index_of_key = Hashtbl.create capacity;
    keys = Array.make capacity (Obj.magic ());
    values = Array.make capacity (Obj.magic ());
    size = 0;
  }

let size t = t.size

let get t key =
  match Hashtbl.find_opt t.index_of_key key with
  | None -> None
  | Some idx ->
      (* update recency *)
      Mat.access t.mat idx;
      Some t.values.(idx)

let put t key value =
  match Hashtbl.find_opt t.index_of_key key with
  | Some idx ->
      (* overwrite existing and bump recency *)
      t.values.(idx) <- value;
      Mat.access t.mat idx
  | None ->
      if t.size < t.capacity then (
        (* still space: use next slot = size *)
        let idx = t.size in
        t.size <- t.size + 1;
        t.keys.(idx) <- key;
        t.values.(idx) <- value;
        Hashtbl.add t.index_of_key key idx;
        Mat.access t.mat idx)
      else
        (* evict LRU *)
        let idx = Mat.get_lru t.mat in
        let old = t.keys.(idx) in
        Hashtbl.remove t.index_of_key old;
        t.keys.(idx) <- key;
        t.values.(idx) <- value;
        Hashtbl.add t.index_of_key key idx;
        Mat.access t.mat idx
