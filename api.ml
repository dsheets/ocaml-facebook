(*
  ocaml-facebook - Facebook Platform client API in OCaml

  Copyright (C) <2009-2010> David Sheets <sheets@alum.mit.edu>

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

let ($) = Util.Function.($)

module StrMap = Map.Make(String)
module AStrMap = Util.Assoc(StrMap)
module MStrMap = Util.Map(StrMap)

exception Facebook_error of string * exn list
exception Facebook_timeout
exception Facebook_bad_sig

type application =
    { app_cookie_prefix : string option;
      app_key : string;
      app_sec : string;
      app_domain : string;
      timeout : float option;
      mutable seq : int
    }
and user =
    { uid : int64;
      user_app : application;
      time : float;
      session : (string * float) option;
      added : bool;
      friends : user list;
      props : (string * string) list
    }

type user_table = (int64, user) Hashtbl.t

let users : user_table = Hashtbl.create 10

let application ?(timeout=None) ?cpref key sec domain =
  return { app_cookie_prefix = cpref; app_key = key; app_sec = sec;
	   app_domain = domain; timeout = timeout; seq = 0 }
  
let generate_sig { app_sec = sec } pmap =
  let meat = StrMap.fold (fun k v p -> sprintf "%s%s=%s" p k v) pmap "" in
    Digest.to_hex (Digest.string (meat ^ sec))

(* validate an assoc of params against a facebook signature *)      
let validate_params app ?(ns="fb_sig") params =
  let pref  = ns ^ "_" in
  let prefl = String.length pref in
  let is_ns_param (k, _) = Util.String.begins pref k in
  let fbparams, others = List.partition is_ns_param params in
  let fbparams = List.map (fun (k, v) -> (Util.String.from k prefl, v)) fbparams in
  let fbm = AStrMap.into_map StrMap.empty fbparams in
  let check_timeout fbm =
    let fbtime = MStrMap.findo ["time"] fbm in
    let now = Unix.time ()
    in (match MStrMap.findo ["expires"] fbm with
	  | None -> true
	  | Some expiry when (float_of_string expiry) -. now > 0. -> true
	  | _ -> false)
       || (match app.timeout, fbtime with
	     | Some tm, Some time when (now -. (float_of_string time)) > tm -> false
	     | Some tm, None -> false
	     | _, _ -> true)
  in
  let check_sig pmap sign = sign = (generate_sig app pmap) in
    match AStrMap.findo [ns] others with
      | Some v ->
	  if check_timeout fbm then
	    if check_sig fbm v then
	      return fbm
	    else fail Facebook_bad_sig
	  else fail Facebook_timeout
      | None ->
	  return StrMap.empty

let string_of_time f = Int64.to_string (Int64.of_float f)

let get_uido app fbm =
  let (>>=) = Util.Option.(>>=) in
  let return = Util.Option.return in
    (MStrMap.findo ["user"; "profile_user"; "canvas_user"] fbm)
    >>= (return $ Int64.of_string)

let load_user app uid =
  try Hashtbl.find users uid
  with Not_found -> { uid=uid; user_app=app; session=None;
		      friends=[]; added=false; props=[]; time=0. }

let save_user user = Hashtbl.replace users user.uid user

let get_user app fbm =
  let module M = MStrMap in
  let module O = Util.Option in
  let (>>=) = O.(>>=) in
  let return = O.return in
  let time = (M.findo ["time"] fbm) >>= (return $ float_of_string) in
  let added = (M.findo ["added"] fbm) >>= (return $ ((=) 1) $ int_of_string) in
    match get_uido app fbm with
      | Some uid ->
	  let session = M.findo ["session_key"; "profile_session_key"] fbm in
	  let expiry = (M.findo ["expires"] fbm) >>= (return $ float_of_string) in
	  let user = load_user app uid in
          let user = { user with
			 session = session >>= (fun s ->
						  expiry
						  >>= (return $ fun e -> (s, e)));
			 added = begin match added with
			   | Some b -> b
			   | None   -> user.added end;
			 time =  begin match time with
			   | Some t -> t
			   | None   -> user.time end
		     } in
	  let () = save_user user in
	    Lwt.return (Some user)
      | None -> Lwt.return None
	    
let user app req =
  let post = Http_request.params_post req in
  lwt fbp = validate_params app post in
    if StrMap.is_empty fbp then
      lwt fbg = validate_params app (Http_request.params_get req) in
      (* Ignore GET params if the time is old (user nav) *)
      lwt fbg = match MStrMap.findo ["time"] fbg with
	| Some time ->
	    begin match get_uido app fbg with
	      | Some uid -> if (load_user app uid).time > (float_of_string time)
		then return StrMap.empty
		else return fbg
	      | None -> return fbg
	    end
	| None -> return fbg in
      lwt fbp = validate_params app ~ns:"fb_post_sig" post in
      let fbm = MStrMap.merge fbg fbp in
	if StrMap.is_empty fbm then
	  let cookies = Http_cookie.extract req in
	  lwt fbc = validate_params app ~ns:app.app_key cookies
          in match app.app_cookie_prefix with
	    | None -> get_user app fbc
	    | Some p -> if StrMap.is_empty fbc
	      then (validate_params app ~ns:(p ^ "_" ^ app.app_key) cookies)
		>>= (get_user app)
	      else get_user app fbc
        else
          get_user app fbm
    else
      get_user app fbp

let uid user = user.uid

let gen_cookies path user =
  let make = Http_cookie.make in
  let serialize = Http_cookie.serialize in
  let app = user.user_app in
  let domain = app.app_domain in
  let cookies = [("user", Int64.to_string user.uid);
		 ("time", string_of_time (Unix.time ()))] in
  let cm = AStrMap.into_map StrMap.empty cookies in
  let prefix = match app.app_cookie_prefix with
    | None -> app.app_key
    | Some p -> p ^ "_" ^ app.app_key in
  let cookies = List.map (fun (n,v) -> (prefix ^ "_" ^ n, v)) cookies in
  let cookies = (prefix, generate_sig app cm) :: cookies in
    match user.session, app.timeout with
      | Some (_, expiry), _ ->
	  List.map
	    (fun (n,v) ->
	       serialize (make ~expiry:(`Until expiry) ~path ~domain n v))
	    cookies
      | None, Some tm ->
	  List.map
	    (fun (n,v) ->
	       serialize (make ~expiry:(`Age [`Second (truncate tm)])
			    ~path ~domain n v))
	    cookies
      | None, None ->
	  List.map
	    (fun (n,v) ->
	       serialize (make ~path ~domain n v))
	    cookies
