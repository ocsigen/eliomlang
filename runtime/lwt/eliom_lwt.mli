
(** Implementation of {!Eliom_runtime.Request_data.set_functions}
    using lwt "thread storage".

    This module is aimed at people who want to implement the interface between
    a given webserver library and eliom. If you simply want to use eliom,
    this module is not for you.
*)

val handle_request :
  debug:bool ->
  (unit -> 'a Lwt.t) -> (Eliom_serial.request_data * 'a) Lwt.t
(** [handle_request ~debug f] calls [f] with a fresh request data.
    When f is done, it returns both the result and the data.

    This should be called when starting the handling of a request.
    All code that could lead to the execution of client fragments
    should be inside [f].
*)

val detach_from_request :
  (unit -> 'a) -> 'a
(** [detach_from_request f] launches f with no request data information.
    This should only be used to launch an long-running asynchronous
    functions which is not related to the handling of the current request,
    in order to avoid memory leaks.

    [f] should not lead to the execution of client fragments
    (or terrible things will happen, including client process failure).
*)
