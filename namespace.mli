type fb_data =
    [ `B of int64
    | `D of int
    | `F of float
    | `List of fb_data list
    | `S of string ]

val call_method : string -> Api.user -> string -> (string * fb_data) list -> Json_type.t Lwt.t
