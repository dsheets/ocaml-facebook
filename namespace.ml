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

open Printf
open Lwt
open Cohttp
open Api

module StrMap = Map.Make(String)
module AStrMap = Util.Assoc(StrMap)
module MStrMap = Util.Map(StrMap)
module J = Json_type.Browse

type fb_data = [ `B of int64
               | `D of int
	       | `F of float
	       | `S of string
	       | `List of fb_data list
	       ]

let facebook_http  =  "http://api.facebook.com/restserver.php"
let facebook_https = "https://api.facebook.com/restserver.php"

let rec coerce = function
  | `B l -> Int64.to_string l
  | `D i -> string_of_int i
  | `F f -> string_of_float f
  | `S s -> s
  | `List v -> Json_io.string_of_json (json_of_list v)
and json_coerce = function
  | `B l -> json_of_string (Int64.to_string l)
  | `D i -> json_of_int i
  | `F f -> json_of_float f
  | `S s -> json_of_string s
  | `List v -> json_of_list v
and json_of_int i = Json_type.Int i
and json_of_float f = Json_type.Float f
and json_of_string s = Json_type.String s
and json_of_list l = Json_type.Array (List.map json_coerce l)
  
let bind_value fn (k, v) = (k, fn v)

let std_params user meth params =
  let get = [("method", meth);
             ("api_key", user.user_app.app_key);
             ("v", "1.0")] in
  let get = match user.session with None -> get
    | Some (k, _) -> ("session_key", k) :: get in
  let post = [("call_id", string_of_int user.user_app.seq);
              ("format", "JSON")] @ params in
  let pmap = AStrMap.into_map StrMap.empty (get @ post) in
  let () = user.user_app.seq <- user.user_app.seq + 1 in
    (get, ("sig", generate_sig user.user_app pmap) :: post)

let url_encode = Netencoding.Url.mk_url_encoded_parameters
  
let attempt_post headers body url =
  let rec post exnlist = function
    | 0 -> fail (Facebook_error ("Cannot connect to Facebook", exnlist))
    | n -> begin try_lwt Http_user_agent.post ~headers ~body url with
        | e -> post (e::exnlist) (n - 1)
      end
  in post [] 3

let facebook_error meth user params jo =
  let code = J.int (List.assoc "error_code" jo) in
  let msg = J.string (List.assoc "error_msg" jo) in
    fail (Facebook_error (sprintf "Error %d in %s : %s" code meth msg, []))
       
let call_method ns user name params =
  let params = List.map (bind_value coerce) params in
  let meth = "facebook." ^ ns ^ "." ^ name in
  let get, post = std_params user meth params in
  let qs = url_encode get in
  let headers = [("User-Agent", "ocaml-facebook/0.1")] in
  let body = url_encode post in
  let url = facebook_http ^ "?" ^ qs in
  lwt _, jss = attempt_post headers body url in
  let json = Json_io.json_of_string ~recursive:true ~big_int_mode:true jss in
  let fberr = try let jo = J.objekt json in if List.mem_assoc "error_response" jo
    then Some jo else None
  with Json_type.Json_error _ -> None
  in match fberr with Some jo -> facebook_error meth user params jo
    | None -> return json
