open Printf
open Lwt
open Cohttp

let ($) = Util.Function.($)

module StrMap = Map.Make(String)
module AStrMap = Util.Assoc(StrMap)
module MStrMap = Util.Map(StrMap)

exception Timeout
exception Bad_sig

type application =
    { app_db : string;
      app_key : string;
      app_sec : string;
      app_domain : string;
      timeout : float option;
      mutable seq : int
    }
and user =
    { uid : int64;
      user_app : application;
      session : (string * int32) option;
      added : bool;
      friends : user list;
      props : (string * string) list
    }
  with orm(
    unique: user<uid>, application<app_key>;
    debug: all;
    dot: "facebook.dot"
  )

let facebook_http  =  "http://api.facebook.com/restserver.php"
let facebook_https = "https://api.facebook.com/restserver.php"

let application ?(timeout=None) dbn key sec domain =
  lwt db = Util.Database.attach ~rm:false application_init dbn in
  let save a = let () = application_save db a in a in
    match application_get ~app_key:(`Eq key) db with
      | [] -> return (save { app_db = dbn; app_key = key; app_sec = sec;
			     app_domain = domain; timeout = timeout; seq = 0 })
      | a::_ -> return (save { a with app_db = dbn })
  
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
    let now = Unix.time () in
      match app.timeout, fbtime with
	| Some tm, Some time when (now -. (float_of_string time)) > tm -> false
	| Some tm, None -> false
	| _, _ -> true
  in
  let check_sig pmap sign = sign = (generate_sig app pmap) in
    match AStrMap.findo [ns] others with
      | Some v ->
	  if check_timeout fbm then
	    if check_sig fbm v then
	      return fbm
	    else fail Bad_sig
	  else fail Timeout
      | None ->
	  return StrMap.empty

let load_user app db uid =
  match user_get ~uid:(`Eq uid) db with
    | [] -> { uid = uid;
	      user_app = app;
	      session = None;
	      friends = [];
	      added = false;
	      props = []
	    }
    | u::_ -> u

let get_user app fbm =
  let module M = MStrMap in
  let module O = Util.Option in
  let (>>=) = O.(>>=) in
  let return = O.return in
  let added = (M.findo ["added"] fbm) >>= (return $ ((=) 1) $ int_of_string) in
    match (M.findo ["user"; "profile_user"; "canvas_user"] fbm)
      >>= (return $ Int64.of_string) with
	| Some uid ->
	    let session = M.findo ["session_key"; "profile_session_key"] fbm in
	    let expiry = (M.findo ["expires"] fbm) >>= (return $ Int32.of_string) in
	    lwt db = Util.Database.attach ~rm:false user_init app.app_db in
            let user = { load_user app db uid with
			   session = session >>= (fun s ->
						    expiry
						    >>= (return $ fun e -> (s, e)));
			   added = match added with None -> false | Some b -> b
		       } in
	    let save u = let () = user_save db u in u in
	      Lwt.return (Some (save user))
        | None -> Lwt.return None
	    
(* Does not look at cookies *)
let user app req =
  let post = Http_request.params_post req in
  lwt fbp = validate_params app post in
    if StrMap.is_empty fbp then
      lwt fbg = validate_params app (Http_request.params_get  req) in
      lwt fbp = validate_params app ~ns:"fb_post_sig" post in
      let fbm = MStrMap.merge fbg fbp in
        get_user app fbm
    else
      get_user app fbp

let gen_cookie user =
  let app = user.user_app in
  let cookie = [("user", Int64.to_string user.uid)] in
  let cookie = match user.session with
    | Some (sess_key, expiry) ->
	("session_key", sess_key) :: ("expires", Int32.to_string expiry) :: cookie
    | None -> cookie in
  let cm = AStrMap.into_map StrMap.empty cookie in
  let cookie = List.map (fun (n,v) -> (app.app_key ^ "_" ^ n, v)) cookie in
  let cookie = match user.session with
    | Some (_, expiry) -> ("expires", Int32.to_string expiry) :: cookie
    | None -> cookie in
  let cookie = [(app.app_key, generate_sig app cm);
		("domain", app.app_domain)] @ cookie in
    String.concat "; " (List.map (fun (n,v) -> n ^ "=" ^ v) cookie)

(* QUERIES *)

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

module Namespace =
struct
  let rec coerce = function
    | `D i -> string_of_int i
    | `S s -> s
    | `List v -> Json_io.string_of_json (json_of_list v)
  and json_coerce = function
    | `D i -> json_of_int i
    | `S s -> json_of_string s
    | `List v -> json_of_list v
  and json_of_int i = Json_type.Int i
  and json_of_string s = Json_type.String s
  and json_of_list l = Json_type.Array (List.map json_coerce l)

  let bind_value fn (k, v) = (k, fn v)

  let call_method ns user name params =
    let params = List.map (bind_value coerce) params in
    let get, post = std_params user ("facebook." ^ ns ^ "." ^ name) params in
    let qs = url_encode get in
    let headers = [("User-Agent", "ocaml-facebook/0.1")] in
    let body = url_encode post in
    let url = facebook_http ^ "?" ^ qs in
    lwt _, jsons = Http_user_agent.post ~headers ~body url in
      return (Json_io.json_of_string jsons)
end

module Users =
struct
  include Namespace
  let call user = call_method "users" user
    
  let loadr json = return json

  let get_info user uids fields =
    (call user "getInfo" [("uids", `List uids);
			  ("fields", `List fields)]) >>= loadr

  let get_standard_info user uids fields =
    (call user "getStandardInfo" [("uids", `List uids);
				  ("fields", `List fields)]) >>= loadr
end

module Fql =
struct
  include Namespace
  let call user = call_method "fql" user

  let query user q = call user "query" [("query", `S q)]

end
