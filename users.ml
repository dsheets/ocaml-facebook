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
  
let call user = Namespace.call_method "users" user
  
let get_info user users fields = call user "getInfo"
  [("uids", `List (List.map (fun u -> `B u.uid) users));
   ("fields", `List fields)]
  
let get_standard_info user users fields = call user "getStandardInfo"
  [("uids", `List (List.map (fun u -> `B u.uid) users));
   ("fields", `List fields)]
  
(* TODO: Really? Mutate the world for a boolean API call?! *)
let is_app_user user =
  lwt b = (call user "isAppUser" [("uid", `B user.uid)]) >|= J.bool
  in save_user {user with added=b}; return b
