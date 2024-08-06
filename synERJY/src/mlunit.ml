(* unit_test.ml
* common prefixes
* constructors : U_
* functions : u_
*)

exception U_Failure of string
exception U_Error of string

(* -------------------------------------------------------------------------
* auxiliary functions
* ---------------------------------------------------------------------- *)
let i32 (i: int) = Int32.of_int i ;;

(* -------------------------------------------------------------------------
* print routines
* prefix: u_p
* ---------------------------------------------------------------------- *)

let u_pfl () = flush stderr
let u_ps s = prerr_string s; u_pfl()
let u_pn () = prerr_newline ()
let u_psn s = prerr_endline s
let u_pi i = u_ps " "; prerr_int i; u_pfl()
let u_pin i = u_ps " "; u_pi i ; u_pn()
let u_psln sl = List.iter (fun s -> u_psn s) sl
(* ---------------------------------------------------------------------- *)

let u_psa args =
	Array.iter
		(fun x ->
					u_ps (" \"");
					u_ps " "; u_ps (String.escaped x);
					u_ps ("\"");
		) args

let u_psan args = u_psa args; u_pn()

(* -------------------------------------------------------------------------
* channel options
* ---------------------------------------------------------------------- *)

let cin_opt = ref (None : in_channel option ) ;;
let cout_opt = ref (None : out_channel option ) ;;

let cin () = match !cin_opt with | None -> failwith "?cin" | Some c -> c ;;
let cout () = match !cout_opt with | None -> failwith "?cout" | Some c -> c ;;

let cin_set (ic: in_channel) = cin_opt := Some (ic);;
let cout_set (oc: out_channel) = cout_opt := Some (oc);;

let close_inout () =
	(match !cin_opt with | None -> () | Some c -> close_in c);
	(match !cout_opt with | None -> () | Some c -> close_out c);
	cin_opt := None;
	cout_opt := None;
;;

(* -------------------------------------------------------------------------
* assert and test case functions
* ---------------------------------------------------------------------- *)
let u_ok_flag = ref false;;

let u_ok_stack = ref ([false] : bool list);;  (* the initial value *)

let u_ok_stack_check () =
	match !u_ok_stack with
	| [] ->
			u_psn "u_ok_flag: too many u_ok_pop () or u_end (..)";
			false
	| [false] -> true
	| [true] -> u_psn "u_ok_flag: inconsistent stack"; false
	| h:: t -> u_psn "u_ok_flag: u_ok_begins not matched: ";
			u_pi ((List.length !u_ok_stack) - 1);
			u_pn ();
			false
;;

let u_ok_flag () =
	match !u_ok_stack with
	| [] -> assert (u_ok_stack_check ());
			false (* "u_ok_flag: too many u_ok_pop ()" *)
	| [h] -> assert (u_ok_stack_check ()); h
	| h:: _ -> h
;;

let u_ok_push b =
	u_ok_stack := b :: !u_ok_stack
;;

let u_ok_pop () =
	match !u_ok_stack with
	| [] -> assert (u_ok_stack_check())
	| [h] -> assert (u_ok_stack_check())
	| h:: t -> u_ok_stack := t
;;

let nb_failures = ref 0 ;;
let nb_errors = ref 0 ;;
let nb_asserts = ref 0 ;;
let nb_cases = ref 0 ;;

let u_report () =
	let testfiles_ok = u_ok_stack_check() in
	if not testfiles_ok then (
		u_psn "== ERRORS == in testfiles, see above"
	);
	u_ps "number of testcases:" ; u_pi (!nb_cases);
	u_ps " / asserts:" ; u_pi (!nb_asserts);
	u_ps " / errors:" ; u_pi (!nb_errors);
	u_ps " / failures:"; u_pi (!nb_failures);
	u_pn();
	u_psn (if (testfiles_ok) && ((!nb_failures + !nb_errors) = 0)
			then "== OK =="
			else "== ERRORS == ERRORS == ERRORS =="
		);
;;

let u_test_begin (fname: string) (ok_flag: bool) =
	u_psn ("------------- begin: \""^fname^"\"");
	u_ok_push ok_flag;
;;
let u_test_end (fname: string) =
	u_psn ("-------------   end: \""^fname^"\"");
	if (u_ok_flag ()) then u_pn ();
	u_ok_pop ();
	u_report();
;;

let u_info info = if u_ok_flag () then u_psn ("=**= Info: "^info);;

let u_remark info = u_psn ("---- Remark: "^info);;

let u_assert (msg : string) (b : bool) =
	if (not b) then raise (U_Failure msg) else (incr nb_asserts)

let u_assert_string (msg : string) (s_expect: string) (s_other: string) =
	if (not (s_expect = s_other)) then (
		let msg = msg^": string expecting: \""^s_expect^
			"\" but is: \""^s_other^"\"" in
		raise (U_Failure msg)
	) else (incr nb_asserts)

let u_assert_int (msg : string) (i_expect: int) (i_other: int) =
	if (not (i_expect = i_other)) then (
		let s_expect = string_of_int i_expect in
		let s_other = string_of_int i_other in
		let msg = msg^": int expecting: "^s_expect^" but is: "^s_other in
		raise (U_Failure msg)
	) else (incr nb_asserts)

let u_assert_float (msg : string) (f_expect : float) (f_other : float) =
	if not (f_expect = f_other) then (
		let s_expect = string_of_float f_expect in
		let s_other = string_of_float f_other in
		let msg = msg^": float expecting: "^s_expect^" but is: "^s_other in
		raise (U_Failure msg)
	) else (
		incr nb_asserts
	)

let u_fail (msg : string) = raise (U_Failure msg)

let u_exn_case (label : string)
		(expected_exns : exn list) (test_case : unit -> unit) =
	incr nb_cases;
	if u_ok_flag () then u_psn ("CASE "^label);
	try
		test_case ();
		if (u_ok_flag()) then (
			u_psn ("- OK "^label)
		)
	with
	| U_Failure msg -> u_psn ("** Failure: "^label^": "^msg); incr nb_failures
	| U_Error msg -> u_psn ("**** Error: "^label^": "^msg); incr nb_errors
	| e -> if (List.mem e expected_exns) then (
				if (u_ok_flag()) then (
					u_psn ("OK: "^label^": exception: "^(Printexc.to_string e))
				)
			) else (
				incr nb_errors;
				u_psn ("**** Error: "^label^
						": unexpected exception: "^(Printexc.to_string e) )
			)

let u_case (label : string) (test_case : unit -> unit) =
	u_exn_case label [] test_case
(* ---------------------------------------------------------------------- *)
;;
