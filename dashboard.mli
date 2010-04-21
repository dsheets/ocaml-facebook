val get_count : Api.user -> int Lwt.t
val multi_get_count : Api.user list -> (int64 * int) list Lwt.t
val set_count : Api.user -> int -> bool Lwt.t
val multi_set_count : Api.user list -> (int64 * bool) list Lwt.t
val increment_count : Api.user -> bool Lwt.t
val multi_increment_count : Api.user list -> (int64 * bool) list Lwt.t
val decrement_count : Api.user -> bool Lwt.t
val multi_decrement_count : Api.user list -> (int64 * bool) list Lwt.t
