open Sim_type

(* --------------------------------------------------------------------------
flag for replay break
-------------------------------------------------------------------------- *)

let _replay_break = ref false;;

let replay_break () = !_replay_break;;
let replay_break_set () = _replay_break := true;;
let replay_break_clear () = _replay_break := false;;

(* ------------------------------------------------------------------------
handle a replay stream from file
------------------------------------------------------------------------ *)
class c_replay_file fname =

object (self)
	inherit Sim_base.c_sim_top ()
	
	method classname = "c_replay_file"
	
	method filename = fname
	
	val mutable _channel_opt = (None : in_channel option);
	val mutable _parsebuf_opt = (None : parsebuf_t option);
	
	method is_open = match _channel_opt with | None -> false | _ -> true
	
	val mutable _header_opt = (None : traceheader_t option)
	
	val mutable _number_of_instants = 0
	method number_of_instants = _number_of_instants
	
	val mutable _first_instant_read = false
	
	val mutable _prev_instant_nb = - 1 (* number of the last processed instant *)
	method instant_nb = _prev_instant_nb + 1
	method incr_instant_nb = _prev_instant_nb <- _prev_instant_nb + 1
	method start_instant_nb = _prev_instant_nb <- - 1
	
	method prev_instant = _prev_instant_nb
	method instants_available = self#number_of_instants - self#instant_nb;
	
	val mutable _next_input_event = ([] : emit_t list)
	val mutable _next_output_event = ([] : emit_t list)
	
	method off = (not self#is_open) || (self#instants_available = 0)
	
	val mutable _active = false
	val mutable _activated_at_startup = false
	
	method is_active = _active
	method is_activated_at_startup = _activated_at_startup
	
	method set_active new_active =
		if (not (_parsebuf_opt = None) && self#is_open) then (
			_active <- new_active;
			if Sim_util.is_startup_instant() then (
				_activated_at_startup <- _active;
			);
		)
	
	method private parsebuf =
		match _parsebuf_opt with
		| None ->
				self#close;
				Sim_error.intern "parsebuf: invalid call"
		| Some pb -> pb
	
	method close =
		if self#is_open then (              (* close is idempotent *)
			_parsebuf_opt <- None;
			(match _channel_opt with | None -> () | Some c -> close_in c);
			_channel_opt <- None;
			_next_input_event <- [];
			_next_output_event <- [];
			self#set_active false;
			(* don't clear_error: hold the error state *)
		)
	
	method private basic_open =
		(* open the file to be run together with the simulation.
		* This file must be available to be read.
		*)
		assert (_parsebuf_opt = None);
		assert (not self#is_open);
		try
			let incha = open_in fname in
			_channel_opt <- Some incha;
			_parsebuf_opt <- Some (Sim_parse.parsebuf_from_channel incha);
			_first_instant_read <- false
		with e ->
				self#close;
				Sim_error.err
					(Sim_error.Exception
						(fname^": instant: "^string_of_int self#instant_nb, e))
	
	method proof_read =
		try
			let ip = ref instant_i in
			_number_of_instants <- 0;
			while ( ip := Sim_parse.parse_instant self#parsebuf; not !ip.tr_eof)
			do
				List.iter (self#check_signal _number_of_instants) !ip.tr_emit;
				List.iter (self#check_signal _number_of_instants) !ip.tr_outmsg;
				_number_of_instants <- _number_of_instants + 1
			done
		with
		| Sim_error.SimParse (line, msg) ->
				Sim_error.msg ("# [[ParseError]] "^msg^"\n#    in trace file: "
						^fname^"\n#   at line: "^string_of_int line^"\n");
		| Sim_error.SimError msg ->
				Sim_error.msg ("# "^msg)
		| e ->
				Sim_error.err
					(Sim_error.Exception("after instant: "^string_of_int _number_of_instants, e))
	
	(* ----------------------------------------------------- processing *)
	(* The following methods assume that only a well formed file
	* is processed. proof_read must have been successful.
	*)
	
	method check_signal instant { emitname = name; emitarg = arg } =
		let sgns = sim.target_header.target_signals in
		try let s = List.find (fun s -> name = s.signame) sgns in
			match s.sigvalt.idtype, arg with
			| "void", SNull ->
					()
			| _, SNull ->
					let err() = "# signal '"^name^"' is a pure signal \
						but has got an argument\n# in trace file: "^
						fname^"\n#   at instant: "^string_of_int instant
					in
					Sim_error.msg (err())
			| "void", _ ->
					let err() = "# the signal '"^name^"' expects no argument \
						but has got argument of type '"
						^s.sigvalt.idtype^"'\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^ string_of_int instant
					in
					Sim_error.msg (err())
			| _, SLiteral(t, _, _) when (
				Sim_util.is_primitive_typ s.sigvalt.idtype
				&& let t' = Sim_util.str2primitive_typ s.sigvalt.idtype in
				Util.le_type ~sub: t ~super: t' ) ->
					()
			| _, SLiteral(t, _, _) ->
					let err() = "# the signal '"^name^"' expects an \
						argument of type '"^s.sigvalt.idtype
						^"' but has got an argument of type '"
						^Err.type2str t^"'\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant
					in
					Sim_error.msg (err())
			
			| t, SObject vl ->
					let err1() = "# the signal '"^name^"' expects an \
						argument of type '"^t
						^"'. This type does not exist\n   in trace file: "
						^ fname^"\n#   at instant: " ^string_of_int instant
					and err2 f = "# the signal '"^name^"' expects an \
						argument of type '"^t
						^"' with a field '"^f^"'. The argument \
						does not have such a field\n# in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant
					and err3 f t1 t2
					= "# the signal '"^name^"' expects an \
						argument of type '"^t
						^"' with a field '"^f^"' of type '"^t1
						^"'. But the value provided is of type '"
						^Err.type2str t2^"'\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant in
					let cls = sim.target_header.target_classes in
					( try
						let c = List.find (fun c -> name = c.classnm) cls in
						List.iter
							( fun m ->
										try let (f, (t2, _, _)) =
												List.find
													(fun (f, v) -> f = m.spec.idname) vl in
											let t3 = Sim_util.str2primitive_typ
													m.spec.idtype in
											if Util.le_type ~sub: t2 ~super: t3
											then ()
											else Sim_error.msg
													(err3 f m.spec.idtype t2)
										with Not_found -> Sim_error.msg (err2 m.spec.idname)
							) c.members
					with _ ->
							Sim_error.msg (err1())
					)
			
			| _, SVector vl ->
					let err1() = "# the signal '"^name^"' expects a \
						vector with elements of type '"^s.sigvalt.idtype
						^"' but has got an argument that is not \
						a vector'\n# in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant in
					let n =
						( match s.sigvalt.iddim with
							| [n] -> n
							| _ -> Sim_error.msg (err1())
						) in
					let err2() = "# the signal '"^name^"' expects an \
						argument of type '"^s.sigvalt.idtype
						^"["^string_of_int n^"]' \
						but has got an argument with '"^
						string_of_int (List.length vl)^"' elements \
						\n# in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant
					and err3 t = "the signal '"^name^"' expects a vector \
						with elements of type '"^s.sigvalt.idtype
						^"' but has got an element of type '"
						^Err.type2str t^"'\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant in
					if n = List.length vl
					then ()
					else Sim_error.msg (err2() );
					List.iter
						( fun (t, _, _) ->
									let t' = Sim_util.str2primitive_typ
											s.sigvalt.idtype in
									if Util.le_type ~sub: t ~super: t'
									then ()
									else Sim_error.msg (err3 t)
						) vl
			
			| _, SMatrix vll ->
					let err1() = "# the signal '"^name^"' expects a \
						matix with elements of type '"^s.sigvalt.idtype
						^"' but has got an argument that is not \
						a matrix'\n# in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant in
					let (n1, n2) =
						( match s.sigvalt.iddim with
							| [n1; n2] -> (n1, n2)
							| _ -> Sim_error.msg (err1())
						) in
					let err2() = "# the signal '"^name^"' expects an \
						argument of type '"^s.sigvalt.idtype
						^"["^string_of_int n1^","^string_of_int n2
						^"]' \ but has got an argument that does
						not match in terms of the number of element "^
						"\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant
					and err3 t = "# the signal '"^name^"' expects a matrix \
						with elements of type '"^s.sigvalt.idtype
						^"' but has got an element of type '"
						^Err.type2str t^"'\n#   in trace file: "
						^ fname^"\n#   at instant: "
						^string_of_int instant in
					if n2 = List.length vll
					then ()
					else Sim_error.msg (err2() );
					List.iter
						( fun vl ->
									if List.length vl = n1
									then ()
									else Sim_error.msg (err2())
						) vll;
					List.iter
						( fun vl ->
									List.iter
										( fun (t, _, _) ->
													let t' = Sim_util.str2primitive_typ
															s.sigvalt.idtype in
													if Util.le_type ~sub: t ~super: t'
													then ()
													else Sim_error.msg (err3 t)
										) vl
						) vll
		
		with Not_found ->
				( let err() = "# signal name '"^name^"' does not exist \
						in the application.\n# in trace file: "
						^ fname^"\n#   at instant: "^string_of_int instant
					in
					Sim_error.msg (err())
				)
	
	method read_instant =
		if ( _first_instant_read ) then (
			self#incr_instant_nb;
		) else (
			_first_instant_read <- true
		);
		if (self#off) then (
			_active <- false;
			replay_break_set ();
		) else (
			let ip = Sim_parse.parse_instant self#parsebuf in
			_next_input_event <- ip.tr_emit;
			_next_output_event <- ip.tr_outmsg;
			if ip.tr_break then replay_break_set ();
		)
	
	method input_event_iter (f : emit_t -> unit) =
		List.iter f _next_input_event
	
	method output_event = _next_output_event
	
	method reopen =
		if self#is_open then self#close;
		
		if (self#number_of_instants > 0) then (
			self#basic_open;
			self#start_instant_nb;
			self#set_active (Sim_util.is_startup_instant() && _activated_at_startup)
		) else (
			replay_break_set();
		)
	
	initializer (
		self#basic_open;
		self#proof_read;
		self#reopen;
		self#set_active true
	)
	
end (* c_replay_file *)

(* ------------------------------------------------------------------------
c_replay, hold a bag of replay streams
------------------------------------------------------------------------ *)

class c_replay () =

object (self)
	inherit Sim_base.c_sim_top ()
	
	method classname = "c_replay"
	
	(* ---------------------------------------------- the storage  *)
	val _rep_table = (Hashtbl.create 17 : (string, c_replay_file) Hashtbl.t)
	val mutable _rep_sequence = ([] : string list)
	
	method add k repl =
		Hashtbl.add _rep_table k repl;
		_rep_sequence <- _rep_sequence @ [k]
	
	method remove filename =
		(try
			(Hashtbl.find _rep_table filename)#close;
			Hashtbl.remove _rep_table filename;
		with
		| Not_found -> ()
		);
		_rep_sequence <- Sim_util.list_remove _rep_sequence filename
	
	method clear =
		Hashtbl.iter (fun k r -> r#close) _rep_table;
		_rep_sequence <- []
	
	method reopen =
		Hashtbl.iter (fun k r -> r#reopen) _rep_table
	
	method get_memento =
		List.map
			( fun fn -> (fn, (self#get fn)#is_activated_at_startup)) _rep_sequence
	
	method restore_memento mem =
		self#clear;
		List.iter
			( fun fmem ->
						let fnm = fst fmem in
						(try
							let rpf = new c_replay_file fnm in
							self#add fnm rpf;
							rpf#set_active (snd fmem);
						with
							e -> Sim_error.err
									(Sim_error.Exception("restoring trace file "^fnm^": ", e))
						)
			) mem
	
	(* ------------------------------------------------- accessing *)
	method get k =
		Hashtbl.find _rep_table k
	
	method all_names =
		List.fold_left (fun s r -> s^" "^r) "" _rep_sequence;
	
	method includes k = Hashtbl.mem _rep_table k
	
	method is_active =
		let result = ref false in
		Hashtbl.iter
			( fun _ r ->
						if r#is_active then result := true;
			) _rep_table;
		!result
	
	(* ------------------------------------------------- processing *)
	method restart =
		replay_break_clear();
		Hashtbl.iter
			( fun _ r ->
						r#reopen;
			) _rep_table
	
	method input_event_iter (f : emit_t -> unit) =
		Hashtbl.iter
			( fun _ r ->
						if r#is_active then r#input_event_iter f
			) _rep_table
	
	method read_instant =
		Hashtbl.iter
			( fun _ r ->
						if r#is_active then r#read_instant
			) _rep_table
	
end (* c_replay *)

(* ------------------------------------------------------------------------
replay is the singleton object of c_replay within the simulator
------------------------------------------------------------------------ *)

let replay = new c_replay ();;
