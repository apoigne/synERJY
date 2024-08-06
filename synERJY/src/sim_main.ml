(* sim_main.ml *)

open Ast

open Sim_type   (* sim_type.ml   *)
open Sim_util   (* sim_util.ml  *)

(* ------------------------------------------------------------------------
* command line arguments
* ----------------------------------------------------------------------- *)
let process_commandline_args () =
	let anonymous_args = ref []
	in
	let collect_anonymous_args narg = anonymous_args := narg :: !anonymous_args
	in
	Arg.parse
		[ ("-f", Arg.String( fun s -> se.cmd_file <- s ),
			"file # the command file loaded initially");
		("-st", Arg.Set(stub_active),
			"# the trace file loaded initially" );
		("-t", Arg.String( fun s -> sim.tr_file <- Some s ),
			"# calls unit test" );
		("-p", Arg.String( fun s -> se.projectpath <- s ),
			"# sets project path" );
		("-xst", Arg.Set(xstub_active),
			"# activate output of stub-calls" );
		("--", Arg.Rest(collect_anonymous_args),
			"# anonymous args are used as program call with arguments")
		]
		collect_anonymous_args
		"Usage: sim binary-file-name {arguments}";
	
	(* return the anonymous args the same order as in the command line *)
	List.rev !anonymous_args

(* --------------------------------------------------------------------------
* process command file
* ----------------------------------------------------------------------- *)
let rec process_cmd_file cmdfile =
	Lex_wrapper.prc_cmdfile process_cmd
		( fun sl -> pN(); List.iter pS sl; pN() )
		cmdfile (*;
Puctrl.status#has_changed () *)

and process_cmd = function
	| CmdEof -> raise End_of_file
	| CmdMkBuild -> pL ("# make_C_and_bin   Simulation")
	| CmdMkCode -> pL ("# make_C_only      Simulation")
	| CmdConfSignal s -> sim.conf_signals <- s
	| CmdPrint s -> pL s
	| CmdQuit -> exit 0
	| _ -> ()

(* --------------------------------------------------------------------------
* init_sim startup of the simulator with the stepper interface
* ----------------------------------------------------------------------- *)

let init_sim () =
	( try
		let anonargs = process_commandline_args() in
		sim.target_args <- [];
		
		(*
		* target specification in the command line
		* has precedence to commandfile settings.
		*)
		( match anonargs with
			| []
			-> sim.target_cmd <- se.simbinary
			| cmd :: args
			-> sim.target_cmd <- cmd;
					sim.target_args <- args
		);
		pN();
		
		Sim_react.simulation_start true;  (* coldstart *)
		
		match sim.tr_file with
		| None ->
				pL ("# target: "^sim.target_cmd);
				List.iter (fun s -> pS " "; pS s) sim.target_args;
				pN();
				Util_tk.tk_start ~window_name:"" ~class_name:"" "";
				let serc = Filename.concat se.home ".serc" in
				if Sys.file_exists serc then (
					process_cmd_file serc
				) else (
					let serc = Filename.concat se.se_home ".serc" in
					if Sys.file_exists serc
					then process_cmd_file serc
					else ()
				);
				se.open_directory <- Sys.getcwd();
				se.save_directory <- Sys.getcwd();
				
				Util_tk.mk_environment ();
				
				let _, sim_size, sim_weight = se.sim_font in
				Tk.Font.set Tk.Font.sim_font sim_size sim_weight;
				let root_name = Sim_tool.control#root_object#get_full_name in
				let stpfile = Ast.se.file_prefix^root_name^".sim" in
				if Sys.file_exists stpfile then (
					stp.mk_tkstepper ();
					stp.initial_load stpfile;
				) else (
					Sim_base.root_inputs_set_selected true;
					Sim_base.root_signals_select ();
					stp.mk_tkstepper ();
				);
				
				( try Util_tk.tk_main_loop ();
				with _ -> pL("exception in tk_main_loop");
				)
		| Some tr ->
				if tr = "" then (
					Sim_error.msg "no trace file specified"
				) else (
					Sim_tool.control#open_trace_for_unit_test tr;
					Sim_base.root_inputs_set_selected true;
					Sim_tool.control#run_unit_test
				)
	with
	| Sim_error.SimParse (line, msg) ->
		 pL ("line: "^string_of_int line)
	| Sim_error.SimError(msg) ->
		 pL msg
	| e ->
		 pL ("sim: unexpected error: "^(Printexc.to_string e))
	);
	
	(* clean up *)
	(try Sim_react.simulation_stop () with _ -> ());
	(try Unix.unlink sim.tmp_tracefile with _ -> ());

;;

let _ = Callback.register "init_sim" init_sim ;;
