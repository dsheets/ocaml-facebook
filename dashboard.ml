(*
  ocaml-facebook - Facebook Platform client API in OCaml

  Copyright (C) <2010> David Sheets <sheets@alum.mit.edu>
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Lwt
open Api
module J = Json_type.Browse
  
let call user = Namespace.call_method "dashboard" user
  
let tuple_list kfn vfn json = List.fold_left
  (fun al -> function
     | k::v::[] -> (kfn k, vfn v)::al
     | _ -> al)
  []
  (J.list J.array json)
  
let uid_of_json json = Int64.of_string (J.string json)
  
let get_count user = (call user "getCount" [("uid", `B user.uid)]) >|= J.int
  
let multi_get_count = function
  | [] -> return []
  | (ux::_) as ul -> let uids = `List (List.map (fun u -> `B u.uid) ul) in
      (call ux "multiGetCount" [("uids", uids)])
      >|= (tuple_list uid_of_json J.int)
	
let set_count user i =
  (call user "setCount" [("uid", `B user.uid); ("count", `D i)]) >|= J.bool
  
let multi_set_count = function
  | [] -> return []
  | (ux::_) as ul -> let uids = `List (List.map (fun u -> `B u.uid) ul) in
      (* yes, this method uses 'ids' instead of 'uids' *)
      (call ux "multiSetCount" [("ids", uids)])
      >|= (tuple_list uid_of_json J.bool)
	
let increment_count user =
  (call user "incrementCount" [("uid", `B user.uid)]) >|= J.bool
  
let multi_increment_count = function
  | [] -> return []
  | (ux::_) as ul -> let uids = `List (List.map (fun u -> `B u.uid) ul) in
      (call ux "multiIncrementCount" [("uids", uids)])
      >|= (tuple_list uid_of_json J.bool)
	
let decrement_count user =
  (call user "decrementCount" [("uid", `B user.uid)]) >|= J.bool
  
let multi_decrement_count = function
  | [] -> return []
  | (ux::_) as ul -> let uids = `List (List.map (fun u -> `B u.uid) ul) in
      (call ux "multiDecrementCount" [("uids", uids)])
      >|= (tuple_list uid_of_json J.bool)
