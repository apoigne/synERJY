open Ly
open P
open Ast
open Util
open Util_parse
open Yacc

(* --------------------------------------------------------------------------
WRAPPER FOR LEX - GENERATED FUNCTION. NEVER USE LEX DIRECTLY WHEN
SE SOURCES HAVE TO BE PARSED. USE ALWAYS

- package_token if import declarations are parsed
- src_token if a class is parsed
- parse_string if a string is parsed [uses src_token internally;
interferes with parsing classes]

instead.

due to ambiguities of Java - similar syntax definitions there may be a
read ahead of ''id0.id1. etc.etc. .idn'' tokens necessary. If they have
to be re - read, they are stored in ''already_read_tokens''.
''src_token'' cares about that.

''package_token'' AND ''src_token'' have to read one token ahead to terminate
correctly. This token has to be the first one read by a following call
of ''src_token'' and is stored in ''propagate_token''. This re - use is
facilitated by Puctrl.add_file
-------------------------------------------------------------------------- *)
type tpackage =
	| PackageDot
	| PackageName
	| ClassDot
	| ClassName

let already_read_tokens = ref []
let propagate_token = ref Yacc.GARBAGE

let is_id = function
	| A_CLASS _ -> true
	| _ -> false

let is_class = function
	| A_CLASS _ -> true
	| _ -> false

let rec package2id sl = function
	| [A_CLASS i] ->
			c2id (String.concat "." ((c2str i):: sl))
	| DOT:: il ->
			package2id (".":: sl) il
	| (A_CLASS i):: il ->
			package2id ((c2str i):: sl) il
	| (AN_ID i):: il ->
			package2id ((mf2str i):: sl) il
	| _ -> Err.intern "package2id"

let package2id iddotl =
	package2id [] iddotl

let dbg_tk (x : Yacc.token) = ()

let prs_tk lexbuf =
	let tk = Lex.prs_tk lexbuf in
	dbg_tk tk;
	propagate_token := tk;
	tk

let rec check_package expect tkl lexbuf =
	let tk = prs_tk lexbuf in
	match (expect, tk) with
	| PackageDot, DOT ->
			check_package PackageName (tk:: tkl) lexbuf
	| PackageName, tk when (is_id tk) ->
			check_package PackageDot (tk:: tkl) lexbuf
	| PackageName, tk when (is_class tk) ->
			check_package ClassDot (tk:: tkl) lexbuf
	| ClassDot, DOT ->
			check_package ClassName (tk:: tkl) lexbuf
	| ClassDot, _ -> already_read_tokens := [tk];
			A_CLASS (package2id tkl)
	| _ ->
			already_read_tokens := List.rev (tk:: tkl);
			src_token lexbuf

and src_token lexbuf =
	match !already_read_tokens with
	| [] ->
			let tk = prs_tk lexbuf in
			( match tk with
				| AN_ID s -> check_package PackageDot [tk] lexbuf
				| _ -> tk
			)
	| h:: t ->
			already_read_tokens := t;
			h

and package_token lexbuf =
	prs_tk lexbuf

let parse_string yacc_rule str =
	already_read_tokens := [];
	yacc_rule Lex.prs_tk (Lexing.from_string str)

(* -------------------------------------------------------------------------
process a command file.
Used in both pu_ctrl.ml AND IN gredit_tk.ml
Here the yacc / lex - functions may be used without wrapping (no packages :-)
------------------------------------------------------------------------- *)
let rec prc_cmdfile (prc_cmd : tcmd_to_do -> unit) (ps : string list -> unit) (cmdfile : string) =
	let err () = ps ["erroneous command in file \""; cmdfile;"\"\n"];
		raise End_of_file in
	let cmdflen = String.length cmdfile in
	if cmdflen <= 0 then (
		ps ["no resource file specified - ok\n\n"];
		raise End_of_file
	);
	let firstch = (String.get cmdfile 0) in
	if cmdfile = "-" || cmdfile = "STDIN" then (
		let lexb = Lexing.from_channel stdin in
		prc_cmd_stream prc_cmd ps lexb None cmdfile
	) else if firstch = '!' then (
		let cmdf = String.sub cmdfile 1 (cmdflen - 1) in
		ps ["execute shell command: "; cmdf];
		try
			let instream = Unix.open_process_in cmdf in
			while (true) do
				ps [input_line instream]
			done
		with
		| End_of_file -> ()
		| _ -> err()
	) else if firstch = '%' then (
		let cmdf = String.sub cmdfile 1 (cmdflen - 1) in
		let lexb = Lexing.from_string cmdf in
		ps ["execute immediate command: "; cmdf];
		prc_cmd_stream prc_cmd ps lexb None cmdfile
	) else if firstch = '<' then (
		try
			let cmdf = String.sub cmdfile 1 (cmdflen - 1) in
			let instream = Unix.open_process_in cmdf in
			let lexb = Lexing.from_channel instream in
			ps ["read commands from shell: "; cmdf];
			prc_cmd_stream prc_cmd ps lexb (Some instream) cmdfile
		with _ -> err()
	) else if (Sys.file_exists cmdfile) then (
		ps ["command file: "; cmdfile;" used\n"];
		try
			let instream = open_in cmdfile in
			let lexb = Lexing.from_channel instream in
			prc_cmd_stream prc_cmd ps lexb (Some instream) cmdfile
		with _ -> err()
	) else (
		ps ["file \""; cmdfile;"\" not found and ignored\n"];
		raise End_of_file
	)

and prc_cmd_stream prc_cmd ps lexbuf strm_opt cmdfile =
	Ly.push_pos2lc ();
	let close () =
		Ly.pop_pos2lc ();
		match strm_opt with
		| None -> ()
		| Some stream -> close_in stream
	in
	let error () =
		let (l, c) = pos2lc (Lexing.lexeme_start lexbuf) in
		close ();
		ps ["error in line.col "; i2s l;"."; i2s c; " of file "; cmdfile]
	in
	Ly.clr_pos2lc ();
	try
		while (true) do
			prc_cmd (Yacc.prs_cmd Lex.prs_cmd lexbuf)
		done
	with
	| End_of_file -> close ()
	| e -> error ()  (* closes itself *)

(* -------------------------------------------------------------------------
process a trace file (used in verilog.ml)
------------------------------------------------------------------------- *)
let rec prc_trace_file
		(prc_instant: Sim_type.instant_t -> unit) (ps : string list -> unit) (trace_file : string) =
	let err () = ps ["erroneous trace in file \""; trace_file;"\"\n"];
		raise End_of_file in
	if (Sys.file_exists trace_file) then (
		try
			let instream = open_in trace_file in
			let lexb = Lexing.from_channel instream in
			prc_instant_stream prc_instant ps lexb (Some instream) trace_file
		with _ -> err()
	) else (
		ps ["trace file \""; trace_file;"\" not found and ignored\n"];
		raise End_of_file
	)

and prc_instant_stream prc_instant ps lexbuf strm_opt trace_file =
	Ly.push_pos2lc ();
	let close () =
		Ly.pop_pos2lc ();
		match strm_opt with
		| None -> ()
		| Some stream -> close_in stream
	in
	let error () =
		let (l, c) = pos2lc (Lexing.lexeme_start lexbuf) in
		close ();
		ps ["error in line.col "; i2s l;"."; i2s c; " of file "; trace_file]
	in
	Ly.clr_pos2lc ();
	try
		while (true) do
			let inst = Yacc.prs_sim_instant (Lex.prs_sim true) lexbuf in
			if inst.Sim_type.tr_eof || inst.Sim_type.tr_break
			then raise End_of_file
			else prc_instant inst
		done
	with
	| End_of_file -> close ()
	| e -> error ()  (* closes itself *)
