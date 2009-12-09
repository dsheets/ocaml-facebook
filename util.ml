open Lwt

module Assoc(M : Map.S) =
struct
  let to_string ?(sep=",") assoc =
    String.concat sep (List.map (fun (k,v) -> Printf.sprintf "%s=%s" k v) assoc)

  let into_map map assoc =
    List.fold_left (fun m (k, v) -> M.add k v m) map assoc

  let rec findo keys assoc =
    match keys with [] -> None
      | k::ks -> try Some (List.assoc k assoc)
	with Not_found -> findo ks assoc
end

module List =
struct
  let front lst n =
    let rec acc prev next n = match n, next with
      | 0, _ | _, [] -> List.rev prev
      | x, n::ns -> acc (n::prev) ns (x - 1)
    in acc [] lst n
end

module String =
struct
  let begins pref str =
    let pos = min (String.length pref) (String.length str) in
      pref = (String.sub str 0 pos)
  let from str pos =
    try
      String.sub str pos ((String.length str) - pos)
    with Invalid_argument m -> raise (Invalid_argument ("Util.String.from: "^m))
end

module Map(M : Map.S) =
struct
  let rec findo keys m =
    match keys with [] -> None
      | k::ks -> try Some (M.find k m)
	with Not_found -> findo ks m

  let merge a b = M.fold M.add b a
end

module Function =
struct
  let ($) x y z = x (y z)
end

module Option =
struct
  let (>>=) opt f = match opt with Some x -> f x | None -> None
  let return x = Some x
end

module Database =
struct
  let attach ?(rm=true) fn name =
    if Sys.file_exists name && rm then Sys.remove name;
    try_lwt
      return (fn name)
    with
	exn -> Printexc.print_backtrace stdout; fail exn
end
