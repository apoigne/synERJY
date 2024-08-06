(* sim_target.ml *)

open Sim_type   (* sim_type.ml  *)

(* ************************************************************************
execution and i / o with the simulation target
************************************************************************ *)
type tsim_process =
	| TARGET_NEW
	| TARGET_READS
	| TARGET_WRITES
	| TARGET_ENDED
	| TARGET_GARBAGE

let sim_target_state = ref TARGET_NEW ;;

let sim_target_error e =
	sim_target_state := TARGET_GARBAGE;
	Sim_error.err e

let sim_target_restart_ok_with filename =
	if !sim_target_state = TARGET_NEW
	then true
	else if !sim_target_state = TARGET_GARBAGE
	then false
	else
		( match sim.target_md5 with
			| None -> false  (* not yet started *)
			| Some(md5) -> md5 = Digest.file filename
		)

let opt_channel_to_target : out_channel option ref = ref None ;;
let opt_channel_from_target : in_channel option ref = ref None ;;
let opt_target_pid : int option ref = ref None ;;

let channel_to_target () =
	match !opt_channel_to_target with
	| None -> stdout
	| Some (ch) -> ch

let channel_from_target () =
	match !opt_channel_from_target with
	| None -> stdin
	| Some (ch) -> ch

let opt_parsebuffer : parsebuf_t option ref = ref None ;;

let parsebuffer () =    (* assign the parsebuffer on first access *)
	match !opt_parsebuffer with
	| None ->
			let buf = (Sim_parse.parsebuf_from_channel(channel_from_target()))
			in
			opt_parsebuffer := (Some buf);
			buf
	| Some buf ->
			buf

(* ------------------------------------------------------------------------
simulation target process management and i / o
------------------------------------------------------------------------ *)

let sim_target_close_channels () =
	( match !opt_channel_to_target with
		| None -> ()
		| Some (s) -> close_out s; opt_channel_to_target := None
	);
	( match !opt_channel_from_target with
		| None -> ()
		| Some (s) -> close_in s; opt_channel_from_target := None
	);
	opt_parsebuffer := None

let sim_target_put_abort () =
	if (!sim_target_state = TARGET_READS) then (
		Sim_parse.output_endline (channel_to_target()) "++abort";
		sim_target_state := TARGET_WRITES;
	) else (
		sim_target_error (Sim_error.Cannot_write_to_target
				"sim_target_put_abort")
	)

let sim_target_get_abort () =
	if (!sim_target_state = TARGET_WRITES) then (
		let r = Sim_parse.parse_sim_abort (parsebuffer()) in
		sim_target_state := TARGET_ENDED;
		r
	) else (
		sim_target_error (Sim_error.Cannot_write_to_target "sim_target_get_abort")
	)

let rec sim_target_delete () =
	match !sim_target_state with
	| TARGET_NEW -> ()
	| TARGET_ENDED -> ()
	| TARGET_GARBAGE -> ()
	
	| TARGET_READS ->
			( match !opt_target_pid with
				| None ->
						sim_target_close_channels ();
						sim_target_state := TARGET_GARBAGE
				| Some (s) ->
				(* send "abort" and expect "abort" as answer *)
						sim_target_put_abort ();
						let _ = sim_target_get_abort () in
						
						sim_target_state := TARGET_GARBAGE;
						opt_target_pid := None;
						sim_target_close_channels ();
						sim_target_state := TARGET_ENDED;
			)
	| TARGET_WRITES ->
			sim_target_close_channels ();
			sim_target_state := TARGET_GARBAGE

let sim_target_base_create () =
	let (cmd_get, cmd_put) = Unix.pipe()
	and (answer_get, answer_put) = Unix.pipe()
	and (* prepend directory "." to a relative file name in order to call
	* the programm successfully also if '.' is not included in PATH
	*)
	sim_target_call =
		if (Filename.is_relative sim.target_cmd)
		then Filename.concat "." sim.target_cmd
		else sim.target_cmd
	in
	(* nothing read yet *)
	let target_pid =
		Unix.create_process
			sim_target_call
			(Array.of_list (sim_target_call:: sim.target_args))
			cmd_get
			answer_put
			Unix.stderr
	in
	opt_target_pid := Some (target_pid);
	opt_channel_to_target := Some (Unix.out_channel_of_descr cmd_put);
	let inchannel = Unix.in_channel_of_descr answer_get in
	opt_channel_from_target := Some (inchannel);
	Unix.close cmd_get;
	Unix.close answer_put

let sim_target_create () =
	if !sim_target_state = TARGET_NEW then (
		( try
			sim.target_md5 <- Some(Digest.file sim.target_cmd);
			sim_target_base_create();
			sim_target_state := TARGET_WRITES
		with e ->
				sim_target_state := TARGET_GARBAGE;
				sim_target_delete();
				sim_target_error (Sim_error.Exception("cannot_start_sim_target", e))
		)
	) else (
		sim_target_delete();
		if !sim_target_state = TARGET_ENDED then (
			( try
				if (sim_target_restart_ok_with sim.target_cmd) then (
					sim_target_base_create() ;
					sim_target_state := TARGET_WRITES
				) else (
					sim_target_error (Sim_error.Target_binary_changed)
				)
			with e ->
					sim_target_state := TARGET_GARBAGE;
					sim_target_delete();
					sim_target_error
						(Sim_error.Exception("Cannot_start_sim_target", e))
			)
		) else (
			sim_target_error (Sim_error.Violation("Cannot_start_sim_target"))
		)
	)

let sim_target_clean () =
	sim_target_delete();
	sim_target_close_channels();
	sim_target_state := TARGET_NEW;
	sim.target_md5 <- None

let sim_get_targetheader () =
	if (!sim_target_state = TARGET_WRITES) then (
		let r = Sim_parse.parse_targetheader (parsebuffer()) in
		sim_target_state := TARGET_READS;
		r
	) else (
		sim_target_error (Sim_error.Cannot_read_from_target
				"sim_get_targetheader")
	)

let sim_put_instantplus (ip: instantplus_t) =
	if (!sim_target_state = TARGET_READS) then (
		Sim_parse.output_instantplus (channel_to_target()) ip;
		sim_target_state := TARGET_WRITES;
	) else (
		sim_target_error (Sim_error.Cannot_write_to_target
				"sim_put_instantplus")
	)

let sim_get_instantminus () =
	if (!sim_target_state = TARGET_WRITES) then (
		let r = Sim_parse.parse_instantminus (parsebuffer()) in
		sim_target_state := TARGET_READS;
		r
	) else (
		sim_target_error (Sim_error.Cannot_read_from_target
				"sim_get_instantminus")
	)
