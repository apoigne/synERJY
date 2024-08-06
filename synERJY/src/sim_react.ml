open Sim_type
open Sim_util
open Sim_base

(* ------------------------------------------------------------------------
trace channel
------------------------------------------------------------------------ *)

let opt_trace = ref None

let trace_channel () =
	match !opt_trace with
	| None -> Pervasives.stdout
	| Some(chn) -> chn

let trace_set (chn : out_channel) =
	opt_trace := Some(chn)

let trace_clear () =
	match !opt_trace with
	| None -> ()
	| Some(chn) ->
			close_out chn;
			opt_trace := None

(* -----------------------------------------------------------------------
tracing the current simulation
----------------------------------------------------------------------- *)

let tracefile_rights = 416 ;; (* ug+r u+w : -rw-r----- *)

let create_trace file =
	let ochn = open_out_gen [Open_creat; Open_wronly; Open_trunc]
			tracefile_rights
			file
	in
	trace_set ochn

let trace_is_open () = match !opt_trace with | None -> false | _ -> true

let close_trace () =
	trace_clear ()

let append_trace () =
	let ochn =
		open_out_gen [Open_creat; Open_wronly; Open_append]
			tracefile_rights
			sim.tmp_tracefile
	in
	trace_set ochn

(* -----------------------------------------------------------------------
save (copy) the trace in its current state to a file
----------------------------------------------------------------------- *)
let save_trace_into fname =
	close_trace ();
	let outchn = open_out fname in
	(* Write the definitions of the signals into the trace file *)
	let used_signals = ref ([] : signalspec_t list) in
	List.iter
		( fun (_, f) ->
					if f#is_sim_used then
						used_signals := f#signalspec :: !used_signals
		) !root_signals ;
	( try
		let inchn = open_in sim.tmp_tracefile in
		( try
			while true do output_char outchn (input_char inchn) done
		with End_of_file -> ()
		);
		close_in inchn;
	with _ -> ()
	);
	close_out outchn;
	append_trace()

(* --------------------------------------------------------------------------
* break
* ----------------------------------------------------------------------- *)
let _break_request_flag = ref false;;

let is_break_request () = !_break_request_flag ;;
let set_break_request b = _break_request_flag := b;;

(* ------------------------------------------------------------------
* get declarations
* ------------------------------------------------------------------ *)
let get_signal_declarations sigspecL =
	let make_signal_entry name ssp =
		if not ((String.lowercase name) = "now") then (
			[(name, (sE_target#make_field (root_object_dummy#as_parent)
						name (SSignal ssp))
			)]
		) else []
	in
	let make_input_entry name ssp =
		if ssp.sigflow = SInput && not ((String.lowercase name) = "now") then (
			[(name, new c_input name (SSignal ssp) (root_object_dummy#as_parent)
					sE_target
				
			)]
		) else []
	in
	
	root_signals := [];
	root_inputs := [];
	root_outputs := [];
	List.iter
		( fun ssp ->
					let sig_entry = make_signal_entry ssp.signame ssp in
					root_signals := !root_signals @ sig_entry;
					if ssp.sigflow = SOutput
					then root_outputs := !root_outputs @ sig_entry;
					root_inputs :=
					!root_inputs @ (make_input_entry ssp.signame ssp)
		) sigspecL;
	
	root_signals := (
		List.rev (
				List.sort
					( fun (n1, _) (n2, _) -> compare n1 n2
					) !root_signals
			)
	);
	root_inputs := (
		List.rev (
				List.sort
					( fun (n1, _) (n2, _) -> compare n1 n2
					) !root_inputs
			)
	)

let get_class_declarations simclassL =
	List.iter
		( fun cl ->
					let new_class = new c_sE_class cl sE_target in
					sE_target#classes#add new_class
		) simclassL

(* -----------------------------------------------------------------------
do_sample_for_next_instant
----------------------------------------------------------------------- *)
let reset_event () =
	(* set all signals to not present *)
	List.iter (
			fun (_, f) ->
					f#unset_present
		) !root_signals ;
	(* reset the debugs *)
	sE_target#objects#reset_dbgs

(* ----------------------------------------------------------------------
* the target under simulation
* start the target process of the simulated program
* and get the configuration information
* stop
*)

let simulation_start coldstart =
	let bin = sim.target_cmd in
	if not (Sys.file_exists bin)
	then Sim_error.msg ("cannot read "^bin)
	else ();
	
	if coldstart then (
		Sim_target.sim_target_clean();
	);
	
	Sim_target.sim_target_create ();
	
	(* Create the trace file, eventually close on restart *)
	if trace_is_open() then close_trace ();
	create_trace sim.tmp_tracefile;
	
	let hdr = Sim_target.sim_get_targetheader () in
	
	if coldstart then (
		
		root_signals := [];
		
		sim.target_header <- hdr;
		
		get_class_declarations hdr.target_classes ;
		get_signal_declarations hdr.target_signals ;
		
		sim.root_object_address <- hdr.target_config.root_obj;
		sim.root_class_name <- hdr.target_config.root_class;
	);
	
	sE_target#objects#reset_instants;
	sE_target#fields#reset_instants;
	
	reset_instant_count();
	
	Sim_play.replay#restart;
	Sim_play.replay#read_instant

let simulation_stop () =   (* this routine is idempotent *)
	close_trace();
	Sim_target.sim_target_delete()

(* -----------------------------------------------------------------------
* get and put data
*)
(* let do_get_attribute (f:#c_sE_target c_field) =
assert f#is_attribute;
let rec traverse l (pos: int32) =
match l with
| [] ->
(match (Sim_target.sim_get_data f#valuetype pos) with
| SVal sv ->
(match sv with
| Sliteral (t, s, i) -> s
| Sint i -> Int32.to_string i
| Sbool b -> string_of_bool b
| Sdouble d -> d
| Sstring s -> s
)
| _ -> Sim_error.intern
"do_get_attribute: wrong value from target"
)
| h:: t ->
(match h with
| SInPlace d -> traverse t (Int32.add pos d)
| SByReference d ->
(match (Sim_target.sim_get_data "ref" (Int32.add pos d))
with
| SVal sv ->
(match sv with
| Sint r -> traverse t r
| _ -> Sim_error.intern "do_get_attribute: ref expected"
)
| _ -> Sim_error.intern
"do_get_attribute: no ref from target"
)
)
in
f#set_present (traverse f#access_path Int32.zero);
()

let do_put_attribute (f:#c_sE_target c_field) =
assert f#is_attribute;
(* XXX Not used *)
()
*)
(* -----------------------------------------------------------------------
* do_reaction
* ----------------------------------------------------------------------- *)
let do_reaction ?(is_unit_test = false) () =
	try reset_event ();
		
		(* send emits for all present input signals *)
		let emitL =
			List.fold_left
				( fun l (n, f) -> if f#is_present then f#emit:: l else l
				) [] !root_inputs in
		let ip = { ip_break = (is_break_request ());
			ip_emit = emitL;
			ip_time = Int64.zero
		} in
		(* timing irrelevant; will be handled by se.a.out *)
		
		Sim_target.sim_put_instantplus ip;
		Sim_parse.output_instantplus_to_trace (trace_channel ()) ip;
		
		set_break_request false;
		
		let im = Sim_target.sim_get_instantminus () in
		
		List.iter
			( function
				| Sim_Emit emit ->
						( try
							let f = List.assoc (emit.emitname) !root_signals in
							emit.emitflow <- Some f#flow;
							f#set_present;
							f#put_value emit.emitarg;
							f#set_sim_used;
							if f#is_input then (
								let i = List.assoc (emit.emitname) !root_inputs in
								if not(i#is_permanent) then (i#unset_present);
							)
						with Not_found ->
								Sim_error.msg ("signal not expected: "^emit.emitname)
						)
				| Sim_Exc exc ->
						Sim_error.msg (exeption2string exc)
				| Sim_Dbg dbg ->
						if not is_unit_test then (
							try let o = sE_target#objects#find dbg.sim_obj in
								o#add_dbg dbg
							with Not_found ->
									Sim_error.msg ("object not found: "^dbg.sim_obj)
						)
			) im.im_outmsg;
		
		sim_dyn.delta_time <- im.im_time;
		
		Sim_parse.output_instantminus (trace_channel ()) im;
		
		(* sE_target#fields#iter
		(fun f ->
		if f#is_attribute && f#is_selected then do_get_attribute f
		);*)
		
		(* save all field-states to prev_instant, which just happened *)
		sE_target#objects#iter ( fun f -> f#save );
		sE_target#fields#iter ( fun f -> f#save );
		
		Sim_util.incr_instant_count();
	(* now:
	* prev_instant() is the number of the instant just been processed
	* next_instant() is the number of the new instant to come
	*)
	with
	| Sim_error.SimParse (line, msg) ->
			save_trace_into (Ast.se.Ast.file_prefix^"$last"^sim.tr_suffix);
			simulation_stop ();
			Sim_error.msg
				("[[React]] "^msg^" \nin line: "^string_of_int line)
	| Sim_error.SimError e ->
			Sim_error.msg e
	| e ->
			Sim_error.err(Sim_error.Exception ("Sim_react.do_reaction1", e))
