val get_info :
  Api.user -> Api.user list -> Namespace.fb_data list -> Json_type.t Lwt.t
val get_standard_info :
  Api.user -> Api.user list -> Namespace.fb_data list -> Json_type.t Lwt.t
val is_app_user : Api.user -> bool Lwt.t
