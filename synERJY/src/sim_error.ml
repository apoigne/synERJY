(* sim_error.ml *)

exception SimError of string ;;
exception SimParse of int * string;;

(*--------------------------------------------------------------------------*)

let xtrap (s: string) = () ;;  (* to be traced for debugging purposes *)

(* --------------------------------------------------------------------------
error codes
-------------------------------------------------------------------------- *)

type tsim_error =
	| Target_binary_changed
	| Cannot_start_sim_target of exn
	| Cannot_read_from_target of string
	| Cannot_write_to_target of string
	| Target of int * string
	| Exception of string * exn
	| Violation of string

let string_of_tsim_error = function
	| Target_binary_changed -> "[[SimTarget]] target binary file changed"
	| Cannot_start_sim_target e ->
		 "[[SimTarget]] cannot start simulation file / "
     ^ (Printexc.to_string e)
	| Cannot_read_from_target txt ->
		 "cannot read from simulation process: "^txt
	| Cannot_write_to_target txt ->
		 "cannot write to simulation process: "^txt
	| Target (lnr, msg) ->
		 "syntax error in target response: "^ (string_of_int lnr) ^ ": " ^ msg
	| Exception (msg, e) ->
		 "unexpected exception:\n"^ msg ^"\n"^ (Printexc.to_string e)
	| Violation msg ->
		 "internal error: fatal violation"^ (if msg = "" then "" else ": "^msg)

let err e =
	raise (SimError ("[[SimErrorIntern]] "^string_of_tsim_error e))
let msg text =
	raise (SimError text)
let intern text =
	raise (SimError ("[[SimErrorIntern]] "^text))
