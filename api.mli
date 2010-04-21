exception Facebook_error of string * exn list
exception Facebook_timeout
exception Facebook_bad_sig

type application = {
  app_cookie_prefix : string option;
  app_key : string;
  app_sec : string;
  app_domain : string;
  timeout : float option;
  mutable seq : int;
}
and user = {
  uid : int64;
  user_app : application;
  time : float;
  session : (string * float) option;
  added : bool;
  friends : user list;
  props : (string * string) list;
}

val application :
  ?timeout:float option ->
  ?cpref:string -> string -> string -> string -> application Lwt.t

val generate_sig : application -> string Map.Make(String).t -> string
val validate_params :
  application ->
  ?ns:string -> (string * string) list -> string Map.Make(String).t Lwt.t

val load_user : application -> int64 -> user
val save_user : user -> unit
val user : application -> Cohttp.Http_request.request -> user option Lwt.t
val uid : user -> int64
val gen_cookies : string -> user -> (string * string) list
