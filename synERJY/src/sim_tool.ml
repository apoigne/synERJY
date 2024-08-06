open Sim_type
open Sim_util
open Sim_base
open Sim_play

(* --------------------------------------------------------------------------
* tool CONTROL with input editor
* ----------------------------------------------------------------------- *)

class c_control =

object (self)
	inherit c_sim_top () as super
	
	val mutable _root_class = (None : c_sE_target c_sE_class option)
	val mutable _root_object = (None : c_sE_target c_sE_object option)
	val mutable _unit_test_obj = (None : c_replay_file option)
	
	method oid = "control_ti"
	method classname = "c_control"
	
	method root_class =
		match _root_class with
		| None -> assert false
		| Some(rc) -> rc
	
	method root_object =
		(match _root_object with
			| None ->
					if not (sE_target#classes#includes sim.root_class_name) then (
						failwith ("root-class not defined "^sim.root_class_name)
					);
					_root_class <- Some(sE_target#classes#select
							sim.root_class_name);
					let r = sE_target#make_object None ("conf_obj")
							self#root_class#name
							(SInPlace sim.root_object_address)
					in
					_root_object <- Some(r);
					
					sE_target#objects#add r;
					List.iter (fun (k, x) -> sE_target#fields#add x;
						) !root_signals;
					r#set_inputs (List.rev (List.map (fun (n, i) -> i) !root_inputs));
					r
			| Some (r) ->
					r;
		);
	
	(* ------------------------------------------------------------- break *)
	method set_break =
		Sim_react.set_break_request true;
		()
	
	method unset_break =
		Sim_react.set_break_request false;
		()
	
	(* ---------------------------------------------------------- instants *)
	
	(* The instant just before react or run. Before doing reactions
	* _last_instant must be adjusted to the current value of prev_instant.
	*)
	val mutable _last_instant = - 1
	method adjust_last_instant = _last_instant <- Sim_util.prev_instant ();
	
	(* react or run produce new instant(s) in the range
	* [get_last_instant + 1 .. get_prev_instant]
	*)
	method get_last_instant = _last_instant
	method get_prev_instant = Sim_util.prev_instant ()
	
	(* ---------------------------------------------------------- reaction *)
	
	method replay_input_accept (s_emit: emit_t) =
		(* set signal s to present according to s_emit.
		* A value replaced by replay_input_accept cannot be restored.
		*)
		let n = s_emit.emitname in
		let v = s_emit.emitarg in
		let f = try List.assoc (s_emit.emitname) !root_inputs
			with Not_found ->
					Sim_error.msg ("output signal "^n^" not available")
		in
		if (f#name = n) then (
			if f#is_permanent then ( (* keep permanent *)
				f#set_permanent;
			) else (
				f#set_present;
			);
			f#put_value v
		)
	
	method replay_input_reject (s_emit: emit_t) =
		(* set input signals s to "not present" unless they are permanent.
		* The permanence is kept, because it has been set by the user,
		* it cannot be set through replay.
		*)
		List.iter ( fun (n, f) ->
						if f#is_present && (f#get_name = n) then (
							f#unset_present;
						);
						();
			) !root_inputs
	
	method accept_replay =
		if replay#is_active then (
			( try
				replay#read_instant;
				replay#input_event_iter self#replay_input_accept;
			with
				e -> stp.update_log e
			);
			stp.update_replay ()
		)
	
	method react =
		self#adjust_last_instant;
		Sim_react.do_reaction();
		self#accept_replay;
		stp.update_instant ()
	
	method run =
		if (replay#is_active) then (
			(try
				self#adjust_last_instant;
				
				(* input signals for the first instant to be run
				* are selected according to replay and user editing
				* A break set for the first instant is ignored, because
				* run is started from a halt, maybe caused by this break.
				*)
				Sim_react.do_reaction();
				self#accept_replay;
				while not (replay_break()) do
					Sim_react.do_reaction();
					self#accept_replay;
				done;
				
				Sim_util.gc_statistics();
				()
			with
				e -> stp.update_log e
			);
			stp.update_instant ();
		)
	
	(* ------------------------------------------------------------ resets *)
	
	method soft_reset ~to_base_state =
		self#xstub ("soft_reset to_base_state = "^(string_of_bool to_base_state));
		
		if not (Sim_target.sim_target_restart_ok_with sim.target_cmd) then (
			let msg = ("cannot restart with new, modified or deleted file: "^
					sim.target_cmd)
			in
			self#message msg;
			false;
		) else (
			sE_target#objects#reset_instants;
			sE_target#fields#reset_instants;
			
			if to_base_state then (
				replay#clear;
				sE_target#objects#reset_selections;
				sE_target#objects#set_closed;
			);
			
			if (Sim_util.prev_instant()) > (- 1) then (
				Sim_react.simulation_stop ();
				Sim_react.simulation_start false; (* warmstart *)
			);
			
			self#adjust_last_instant;
			
			replay#reopen;
			self#accept_replay;
			stp.update_instant ();
			
			Sim_util.gc_statistics();
			true
		);
	
	method init =
		self#xstub ("init");
		root_inputs_set_selected true;
	
	(* ------------------------------------------------------ trace replay *)
	
	method open_trace filename =
		if Filename.check_suffix filename sim.tr_suffix then (
			if replay#includes filename then (
				self#message (filename^ ": already selected");
			) else (
				(try
					let rf = new c_replay_file filename in
					replay#add filename rf;
					rf#set_active true;    (* active after open *)
					rf#read_instant;
					rf#input_event_iter self#replay_input_accept;
					
					stp.update_replay ();
				with e ->
						stp.update_log e
				);
			);
		) else (
			self#message (filename^": suffix "^sim.tr_suffix^" expected");
		);
		()
	
	method get_replay_files =
		replay#all_names;
	
	method get_number_of_instants_of_trace filename =
		(replay#get filename)#number_of_instants;
	
	method get_actual_instant_of_trace filename =
		(replay#get filename)#instant_nb;
	
	method set_active_trace filename =
		let rf = (replay#get filename) in
		if rf#off then (
			rf#reopen;
			rf#set_active true;
			rf#read_instant;
		) else (
			rf#set_active true;
		);
		rf#input_event_iter self#replay_input_accept;
		stp.update_replay ();
		()
	
	method set_inactive_trace filename =
		let rf = (replay#get filename) in
		rf#set_active false;
		rf#input_event_iter self#replay_input_reject;
		stp.update_replay ();
		()
	
	method is_active_trace filename =
		(replay#get filename)#is_active;
	
	method delete_trace filename =
		let rf = (replay#get filename) in
		rf#input_event_iter self#replay_input_reject;
		rf#close;
		replay#remove filename;
		stp.update_replay ();
		()
	
	(* -------------------------------------------------------------- save *)
	
	method save_trace_into filename =
		let fname = if Filename.check_suffix filename sim.tr_suffix
			then filename
			else filename^sim.tr_suffix
		in
		Sim_react.save_trace_into fname;
		()
	
	(* ------------------------------------------------  command interpreter *)
	
	method command args =
		self#stub ("command --: "^args.(0)^" "^args.(1));
		print_endline("command --: "^args.(0)^" "^args.(1));
		()   (* TODO  not yet implemented *)
	
	(* --------------------------------------------- messages to log_console *)
	
	method message msg = stp.update_log (Sim_error.SimError msg)
	
	(* --------------------------------------------- messages to log_console *)
	
	method message_exc e =
		stp.update_log e
	
	(* ----------------------------------------------------------- unit test *)
	
	val mutable _unit_test_txt = ""
	val mutable _unit_test_error = false
	
	method open_trace_for_unit_test filename =
		if Filename.check_suffix filename sim.tr_suffix then (
			let rf = new c_replay_file filename in
			_unit_test_obj <- Some rf;
			replay#add filename rf;
			rf#read_instant;
			rf#input_event_iter self#replay_input_accept;
		) else (
			pL (filename^": suffix "^sim.tr_suffix^" expected \
					when opening file "^filename);
		)
	
	method accept_replay_for_unit_test =
		if replay#is_active then (
			( try
				replay#read_instant;
				replay#input_event_iter self#replay_input_accept
			with
				e -> stp.update_log e
			);
		)
	
	method run_unit_test =
		_unit_test_txt <- "";
		_unit_test_error <- false;
		self#adjust_last_instant;
		Sim_react.do_reaction ~is_unit_test: true ();
		self#unit_test_at_instant;
		self#accept_replay_for_unit_test;
		while not (replay_break()) do
			Sim_react.do_reaction ~is_unit_test: true ();
			self#unit_test_at_instant;
			self#accept_replay_for_unit_test;
		done;
		if _unit_test_error
		then P.ps _unit_test_txt
		else P.ps "#success"
	
	method private unit_test_at_instant =
		let err = ref false and txt = ref "" in
		match _unit_test_obj with
		| None -> failwith "Error: unit_test_at_instant"
		| Some rp ->
				let rp_out_event = rp#output_event in
				txt := "# instant: "^(!txt)^string_of_int self#get_prev_instant^"\n";
				List.iter
					( fun (name, f) ->
								( try let s_out = List.find
											( fun x -> name = x.emitname
											) rp_out_event in
									let v = s_out.emitarg in
									if f#is_absent then (
										err := true;
										txt := !txt ^"#     expected: "^name^"("^simval2str v^")\n"
										^"#     found:\n\n"
									) else if f#value = v then (
									) else (
										err := true;
										txt := !txt^"#  expected: "
										^name^"("^simval2str v^")\n"
										^"#  found:    "
										^name^"("^simval2str f#value^")\n\n"
									)
								with Not_found ->
										if f#is_present then (
											err := true;
											txt := !txt^"#   expected:\n"
											^"#   found:    "
											^name^"("^simval2str f#value^")\n\n"
										)
								);
					) !root_outputs;
				if !err then (
					_unit_test_error <- true;
					_unit_test_txt <- _unit_test_txt^ !txt
				)
	
end (* c_control *)

let control = new c_control
