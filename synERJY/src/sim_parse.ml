(* sim_parse.ml
* parse
* - messages from a target system under simulation
* - trace files
*
* unparse
* - commands to a target system
* - messages and commands into trace files
*
* Parsing uses functions of lex.mll and yacc.mly.
* The format produced by unparsing can be parsed again.
*)

open Sim_type

(* ------------------------------------------------------------------------
* for debugging
*)

let prerr_lexbuffer (pb : parsebuf_t) =
	let lb = pb.pb_lb in
	prerr_newline();
	prerr_endline ("lex_buffer_len: "^(string_of_int lb. Lexing.lex_buffer_len)
			^" start: "^(string_of_int lb. Lexing.lex_start_pos)
			^ " curr: "^(string_of_int lb. Lexing.lex_curr_pos)
			^ " last: "^(string_of_int lb. Lexing.lex_last_pos)
		);
	prerr_endline
		(String.sub lb.Lexing.lex_buffer
				lb.Lexing.lex_start_pos
				(lb.Lexing.lex_buffer_len - lb.Lexing.lex_start_pos));
	(* --------
	-------- *)
	()

(* ------------------------------------------------------------------------
* parsing
*)
let parsebuf_from_string s = { pb_lb = Lexing.from_string s; pb_line = 1 }
let parsebuf_from_channel cn = { pb_lb = Lexing.from_channel cn; pb_line = 1 }

let do_parse yacc_prs ?(is_trace = false) (pb : parsebuf_t) =
	let prev_pb = sim.parsebuf in
	sim.parsebuf <- pb;
	try
		let result = yacc_prs (Lex.prs_sim is_trace) pb.pb_lb in
		sim.parsebuf <- prev_pb;
		result
	with e ->
			let errline = sim.parsebuf.pb_line in
			sim.parsebuf <- prev_pb;
			match e with
			| Ly.Parse msg ->
					raise (Sim_error.SimParse (errline, msg))
			| e ->
					raise (Sim_error.SimParse (errline, Printexc.to_string e))

let parse_traceheader pb = do_parse (Yacc.prs_sim_traceheader) pb
let parse_sim_class pb = do_parse (Yacc.prs_sim_class) pb
let parse_sim_signals pb = do_parse (Yacc.prs_sim_signals) pb
let parse_sim_psignals pb = do_parse (Yacc.prs_sim_psignals) pb
let parse_sim_classes pb = do_parse (Yacc.prs_sim_classes) pb
let parse_sim_timing pb = do_parse (Yacc.prs_sim_timing) pb
let parse_targetheader pb = do_parse (Yacc.prs_sim_targetheader) pb
let parse_instantplus pb = do_parse (Yacc.prs_sim_instantplus) pb
let parse_outmsg pb = do_parse (Yacc.prs_sim_outmsg) pb
let parse_instantminus pb = do_parse (Yacc.prs_sim_instantminus) pb
let parse_sim_abort pb = do_parse (Yacc.prs_sim_abort) pb
let parse_instant pb = do_parse (Yacc.prs_sim_instant) ~is_trace: true pb

let parse_valopt pb = do_parse (Yacc.prs_sim_valopt) pb

let parse_val_string v =
	Yacc.prs_sim_valopt (Lex.prs_sim false) (Lexing.from_string v)
(* ------------------------------------------------------------------------
* unparsing
*)

let output_newline out =
	output_char out '\n';
	flush out

let output_endline (out : out_channel) (s : string) =
	output_string out s;
	output_newline out

let output_int out (i: int) =
	output_string out (string_of_int i)

let output_idspec out is =
	output_string out is.idtype;
	output_char out ' ';
	output_string out is.idname

let output_textsel out (ts : textsel_t) =
	output_string out "(";
	output_int out ts.tselfilex ; output_string out ",";
	output_int out ts.originline; output_string out ",";
	output_int out ts.origincol ; output_string out ",";
	output_int out ts.cornerline; output_string out ",";
	output_int out ts.cornercol ;
	output_string out ")"

let output_graficfiles out (is : int list) =
	output_string out "(";
	output_string out (String.concat "," (List.map (fun i -> string_of_int i) is));
	output_string out ")"

let output_signalspecs out (tsL: signalspec_t list) =
	List.iter
		( fun ts ->
					output_string out (
							match ts.sigflow with
							| SInput -> "$input"
							| SOutput -> "$output"
							| SLocal -> "$local"
						);
					output_string out
						( if ts.sigvalt.idname ="value" then (
								"<"^ts.sigvalt.idtype
								^ (if ts.sigvalt.iddim = []
									then ""
									else
										let dims = List.map string_of_int ts.sigvalt.iddim in
										"["^String.concat "," dims^"]"^"> "
								)
							) else (
								Sim_error.err (Sim_error.Violation
										"sim_parse: Signal type spec")
							)
						);
					output_string out (ts.signame);
					output_newline out
		) tsL

let output_emit out (plus: bool) (te: emit_t) =
	(* plus = true/false -- instantplus/instantminus mode *)
	output_char out (if plus then '+' else ' ');
	output_string out te.emitname;
	output_string out " (";
	output_string out (Sim_util.simval2str te.emitarg);
	output_endline out ")"

let output_outmsg out (tm: simoutmsg_t) =
	(* plus = true/false -- instantplus/instantminus mode *)
	match tm with
	| Sim_Emit te ->
			( match te.emitflow with
				| None -> Sim_error.intern "output_outmsg"
				| Some SOutput -> output_emit out false te
				| _ -> () (* only outputs for trace files *)
			)
	| Sim_Exc _ -> ()
	| Sim_Dbg _ -> ()

let output_instantplus out (tip: instantplus_t) = (* to target and tracefile*)
	List.iter (output_emit out true) tip.ip_emit;
	output_endline out
		( if tip.ip_time > Int64.zero
			then "++i " ^(string_of_time tip.ip_time)
			else "++i"
		)

let output_instantplus_to_trace out (tip: instantplus_t) = (* to tracefile*)
	if tip.ip_break then output_endline out "#break;";
	List.iter (output_emit out false) tip.ip_emit;
	output_endline out "->"

let output_instantminus out (tim: instantminus_t) = (* to tracefile *)
	List.iter (output_outmsg out) tim.im_outmsg;
	output_endline out ";"
