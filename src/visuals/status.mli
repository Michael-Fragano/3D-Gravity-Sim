type input_state =
  | Idle
  | Pressed
  | Held
  | Released
  | Unmonitored


type t

val default : unit -> t
(** [default ()] returns a status with the default values*)

val update_input : bool -> input_state -> input_state
(** [update_input is_down curr] outputs the new input state based on if
    [is_down ()] returns true or false*)

val update_mouse : input_state -> input_state
(** [update_mouse input] changes the mouse state based on if the button
    is down or up*)

val keys_down : unit -> char list
(** returns a list of keys which are down this frame*)

val poll_input : t -> t
(** updates the key states based on which keys are down*)

val pause : t -> t
(** [pause status] sets the paused flag for the status*)

val play : t -> t
(** [play status] unsets the paused flag*)

val toggle_pause : t -> t
(** [toggle_pause status] toggles the paused flag*)


val update_speed : bool -> t -> t
(** [update_speed f status] doubles the speed of the system playback if
    [f] is true, and halves it otherwise *)

val update_body_num : Gravity.system -> t -> t
(** [update_body_num system status] returns a new status with the number
    of bodies updated to the current number of bodies in [system]*)

val mouse_state : t -> input_state
(** [mouse_state status] returns the current state of the mouse*)


val key_state : char -> t -> input_state
(** [key_state c status] returns the current state of the key [c]*)

val bind_mouse : input_state -> (t -> t) -> t -> t
(** [bind_mouse state f status] returns [f status] if [state] matches
    [mouse_state status]. Otherwise returns [status] *)

val bind_key : char -> input_state -> (t -> t) -> t -> t
(** [bind_mouse c state f status] returns [f status] if [state] matches
    [key_state c status]. Otherwise returns [status] *)

val is_paused : t -> bool
(** [is_paused status] returns true if the system is paused*)

val speed : t -> float
(** [speed status] returns the current speed of the system*)