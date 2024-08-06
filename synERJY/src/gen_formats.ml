open Ly
open Ast
open P
open Util
open Util_print

(* ==========================================================================
GENERATE code for the REACTIVE PART of an application
- Blif : Blif Hardware format
- Lustre : Lustre programs as input for NP_tools
- Verilog : Verilog format for model checking in VIS
========================================================================== *)
let sig2str s = sig2underscorestr s
let ll2str ll = lst_sep2str Ly.lbl2str ":" ll

(* -------------------------------------------------------------------------
auxiliaries handling input and output
------------------------------------------------------------------------- *)
let control_eqns sa =
	List.fold_right
		( fun d l -> match d.sc_act with | None -> d:: l | Some _ -> l
		) sa.sa_eqns []

let action_eqns sa =
	List.fold_right
		( fun d l -> match d.sc_act with | None -> l | Some _ -> d:: l
		) sa.sa_eqns []

let input_sigs sa =
	List.fold_left
		( fun l xs ->
					if is_input_sig xs
					then (sig2str (sig2sname xs)):: l
					else l
		) [] sa.sa_sigs

let input_sgns sa =
	List.fold_left
		( fun l xs ->
					if is_input_sig xs
					then (Sc.sgn2str (sig2sgn xs)):: l
					else l
		) [] sa.sa_sigs

let data_inputs sa =
	List.fold_left
		( fun l d ->
					match d.sc_act with
					| None -> l
					| Some a ->
							( match a with
								| ActBool (_, b, _, _) ->
										(Sc.sgn2str b):: l
								| ActTimeCond(_, b, _, _, _) ->
										(Sc.sgn2str b):: l
								| _ -> l
							)
		) [] sa.sa_eqns

let data_inputs_pos sa =
	List.fold_left
		( fun l d ->
					match d.sc_act with
					| None -> l
					| Some a ->
							( match a with
								| ActBool(ll, b, _, _) ->
										((Sc.sgn2str b)^" -> "^(ll2str ll)):: l
								| ActTimeCond(ll, b, _, _, _) ->
										((Sc.sgn2str b)^" -> "^(ll2str ll)):: l
								| _ -> l
							)
		) [] sa.sa_eqns

let output_sigs sa =
	List.fold_left
		( fun l xs ->
					if is_output_sig xs
					then (sig2str (sig2sname xs)):: l
					else l
		) [] sa.sa_sigs

let local_sgns sa =
	List.fold_left
		( fun l d -> (Sc.sgn2str d.sc_sgn):: l
		) [] (control_eqns sa)

let action_sgns sa =
	List.fold_left
		( fun l d -> (Sc.sgn2str d.sc_sgn):: l
		) [] (action_eqns sa)

let registers sa =
	List.fold_left ( fun l d -> (Sc.sgn2str d.sc_sgn):: l ) [] sa.sa_mems

(* =========================================================================
Printing of comments
- on "internal" inputs
- on "action triggers"
========================================================================= *)
let print_action pc s e ro ll =
	pn(); pc ((Sc.sgn2str s)^" :: "^e);
	pn(); pc ("           "^(pobj ro));
	pn(); pc ("          "^(ll2str ll));
	pn()

let sgn2sig s ro = React2sya.sgn2sig s (ro2sigs ro)

let action_sgns_pos pc d =
	match d.sc_act with
	| None -> ()
	| Some a ->
			( match a with
				| ActStmt (lbl, e, ro) ->
						print_action pc d.sc_sgn (expr2str e) ro lbl
				| ActInit (lbl, s, _, ro) ->
						print_action pc d.sc_sgn
							("init: "^(sig2str (sig2prcsname (sgn2sig s ro)))) ro lbl
				| ActEmit (lbl, _, s, _, Rct e, _, _, ro) ->
						Err.intern "action_sgns_pos"
				| ActEmit (lbl, _, s, _, Val e, _, _, ro) when React2sya.Expr.is_null e ->
						()
				(* | ActEmit (lbl, _, s, Val e, _, ro)
				-> print_action pc d.sc_sgn
				((sig2str (sig2prcsname (sgn2sig s ro)))^
				"s <- "^(expr2str e))
				ro lbl *)
				| ActSigPres _
				| ActSigVal _
				| ActSigIn _
				| ActSigOut _ ->
						()
				| ActPrm (lbl, s, e, _, ro) ->
						print_action pc d.sc_sgn
							((Sc.sgn2str s)^" <- "^(expr2str e))
							ro lbl
				| ActAssgn (lbl, s, e, _, ro) ->
						print_action pc d.sc_sgn
							((sig2str (sig2prcsname (sgn2sig s ro)))^
								"s <- "^(expr2str e))
							ro lbl
				| ActBool (lbl, _, e, ro) ->
						print_action pc d.sc_sgn (expr2str e) ro lbl
				| ActTimeCond(lbl, _, _, e, ro) ->
						print_action pc d.sc_sgn ("@ "^(expr2str e)) ro lbl
				| ActDbg _ ->
						Err.intern "action_sgns_pos"
			)

let info_internal_inputs pc sa =
	ps "\n\n";
	pc " Input by Boolean Data Actions";
	ps "\n\n";
	List.iter (fun s -> pc s) (data_inputs_pos sa)

let info_internal_outputs pc sa =
	ps "\n\n";
	pc " Triggers of Data Actions";
	ps "\n\n";
	List.iter (action_sgns_pos pc) sa.sa_eqns

(* -------------------------------------------------------------------------
Generation of BLIF format
------------------------------------------------------------------------- *)
let list_split l =
	let x = ref [] and y = ref l in
	for i = 0 to (List.length l) / 2 - 1 do
		x := (List.hd !y)::!x;
		y := (List.tl !y)
	done;
	(List.rev !x,!y)

let p_latch x = ps ("\n.latch "^x^"_w "^x^" 0")
and p_sign x y = ps ("\n.names "^y^" "^x); ps "\n1 1"
and p_tt x = ps ("\n.names "^x); ps "\n1"
and p_ff x = ps ("\n.names "^x)
and p_not x y = ps ("\n.names "^y^" "^x); ps "\n0 1"
and p_or x y z = ps ("\n.names "^y^" "^z^" "^x); ps "\n00 0"
and p_and x y z = ps ("\n.names "^y^" "^z^" "^x); ps "\n11 1"

let rec p_eqn_hw x t =
	let and_l l = if List.length l = 1 then List.hd l else (AAnd l)
	and or_l l = if List.length l = 1 then List.hd l else (OOr l) in
	match t with
	| S y ->
			if Sc.is_reg y
			then p_latch (Sc.sgn2str y)
			else p_sign x (Sc.sgn2str y)
	| CC _ ->
			Err.intern "p_eqn_hw"
	| TT ->
			p_tt x
	| FF ->
			p_ff x
	| NNot (S y) ->
			p_not x (Sc.sgn2str y)
	| NNot t ->
			p_eqn_hw (x^"f") t ;
			p_not x (x^"f")
	| AAnd [S y] ->
			p_sign x (Sc.sgn2str y)
	| AAnd [S y; S z] ->
			p_and x (Sc.sgn2str y) (Sc.sgn2str z)
	| AAnd [t; S z] ->
			p_eqn_hw (x^"f") t ;
			p_and x (x^"f") (Sc.sgn2str z)
	| AAnd [S y; t] ->
			p_eqn_hw (x^"t") t ;
			p_and x (Sc.sgn2str y) (x^"t")
	| AAnd [S y; t1; t2] ->
			p_eqn_hw (x^"t") (AAnd [t1; t2]) ;
			p_and x (Sc.sgn2str y) (x^"t")
	| AAnd tl ->
			let (l, r) = list_split tl in
			p_eqn_hw (x^"f") (and_l l) ;
			p_eqn_hw (x^"t") (and_l l) ;
			p_and x (x^"f") (x^"t")
	| OOr [S y] ->
			p_sign x (Sc.sgn2str y)
	| OOr [S y; S z] ->
			p_or x (Sc.sgn2str y) (Sc.sgn2str z)
	| OOr [t; S z] ->
			p_eqn_hw (x^"f") t ;
			p_or x (x^"f") (Sc.sgn2str z)
	| OOr [S y; t] ->
			p_eqn_hw (x^"t") t ;
			p_or x (Sc.sgn2str y) (x^"t")
	| OOr [S y; t1; t2] ->
			p_eqn_hw (x^"t") (OOr [t1; t2]) ;
			p_or x (Sc.sgn2str y) (x^"t")
	| OOr tl ->
			let (l, r) = list_split tl in
			p_eqn_hw (x^"f") (or_l l) ;
			p_eqn_hw (x^"t") (or_l r) ;
			p_or x (x^"f") (x^"t")

let make_blif sa =
	let inputs = (input_sigs sa)@(data_inputs sa)
	and outputs = output_sigs sa
	and eqns = control_eqns sa
	and alpha = Sc.sgn2str sa.sa_alpha in
	ps ".model MAIN";
	ps "\n\n.inputs "; lst_sep2BpsB "" "" ps " " (alpha:: inputs);
	ps "\n\n.outputs "; lst_sep2BpsB "" "" ps " " outputs;
	pn();
	List.iter (fun d -> p_latch (Sc.sgn2str d.sc_sgn)) sa.sa_mems;
	pn();
	List.iter
		( fun d ->
					if is_input_sig d then (
						p_sign (Sc.sgn2str (sig2sgn d)) (sig2str (sig2sname d))
					) else if is_output_sig d then (
						p_sign (sig2str (sig2sname d)) (Sc.sgn2str (sig2sgn d))
					) else ()
		) sa.sa_sigs;
	pn();
	List.iter
		( fun d -> p_eqn_hw (Sc.sgn2str d.sc_sgn) d.sc_def
		) eqns;
	pn();
	List.iter
		( fun d -> p_eqn_hw ((Sc.sgn2str d.sc_sgn)^"_w") (d.sc_def)
		) sa.sa_mems;
	pn();
	ps "\n.end"

(* -------------------------------------------------------------------------
Generation of Lustre format for NP
------------------------------------------------------------------------- *)
let rec npexp2str =
	function
	| S x -> Sc.sgn2str x
	| CC x -> Err.intern "npexp2str"
	| TT -> "true"
	| FF -> "false"
	| NNot x -> "not("^(npexp2str x)^")"
	| AAnd el -> "("^(lst_sep2str npexp2str " and " el)^")"
	| OOr el -> "("^(lst_sep2str npexp2str " or " el)^")"

let pc_lustre s = ps "--    "; ps s; pn()

let make_lustre sa =
	let inputs = (input_sigs sa)@(data_inputs sa)
	and outputs = (output_sigs sa)@(action_sgns sa)
	and locals = (input_sgns sa)@(local_sgns sa)
	and regs = registers sa
	and eqns = sa.sa_eqns
	and alpha = Sc.sgn2str sa.sa_alpha
	and beta = Sc.sgn2str sa.sa_beta
	and peqn l r = ps "\n  "; ps l; ps " = "; ps r; ps ";"
	in
	let rec print_sgns =
		function
		| [] -> ()
		| [s] -> ps "\n "; ps s
		| s:: l -> ps "\n "; ps s; ps ","; print_sgns l
	in
	ps ("node MAIN (\n");
	print_sgns inputs;
	ps " :bool";
	ps "\n\n) returns (\n";
	print_sgns outputs;
	ps " :bool";
	ps "\n\n)";
	ps "\n\nvar\n\n";
	lst_sep2BpsB "" "" ps "," (([alpha; beta]@locals@regs)@(List.map (fun x -> x^"_w") regs));
	ps ":bool;";
	ps "\n\nlet";
	ps "\n\n  "; ps alpha; ps " = true -> false;";
	ps "\n  "; ps beta; ps " = false -> true;\n";
	List.iter
		( fun d ->
					if is_input_sig d
					then peqn (Sc.sgn2str (sig2sgn d)) (sig2str (sig2sname d))
					else ()
		) sa.sa_sigs;
	pn();
	List.iter
		( fun d ->
					if is_output_sig d
					then peqn (sig2str (sig2sname d)) (Sc.sgn2str (sig2sgn d))
					else ()
		) sa.sa_sigs;
	pn();
	List.iter
		( fun d -> peqn (Sc.sgn2str d.sc_sgn) (npexp2str d.sc_def)
		) eqns;
	pn();
	List.iter
		( fun d ->
					peqn ((Sc.sgn2str d.sc_sgn)^"_w")(npexp2str d.sc_def);
					peqn (Sc.sgn2str d.sc_sgn)
						("false -> pre("^(Sc.sgn2str d.sc_sgn)^"_w)\n")
		) sa.sa_mems;
	ps "\n\ntel";
	info_internal_inputs pc_lustre sa;
	info_internal_outputs pc_lustre sa

(* -------------------------------------------------------------------------
Generation of Verilog format for model checking with VIS
------------------------------------------------------------------------- *)
let rec vlexp2str =
	function
	| S x -> Sc.sgn2str x
	| CC x -> Err.intern "vlexp2str"
	| TT -> "1"
	| FF -> "0"
	| NNot x -> "! ("^(vlexp2str x)^")"
	| AAnd el -> "("^(lst_sep2str vlexp2str " & " el)^")"
	| OOr el -> "("^(lst_sep2str vlexp2str " | " el)^")"

let pc_vis s = ps "// "; ps s; pn()

let make_vis_model sa =
	let eqns = control_eqns sa
	and alpha = Sc.sgn2str sa.sa_alpha
	and beta = Sc.sgn2str sa.sa_beta in
	ps ("module MAIN;\n");
	List.iter
		( fun d ->
					if is_input_sig d then (
						ps "\n\ninput   "; print_sgn (sig2sgn d); ps ";";
						ps "\nreg     "; ps (sig2str (sig2sname d)); ps ";";
						ps "\ninitial "; ps (sig2str (sig2sname d)); ps " = 0;";
						ps "\nalways  "; ps (sig2str (sig2sname d)); ps " = ";
						print_sgn (sig2sgn d); ps ";"
					) else ()
		) sa.sa_sigs;
	pn();
	List.iter
		( fun s ->
					ps "\ninput   "; ps s; ps ";";
		) (data_inputs sa);
	pn();
	List.iter
		( fun d ->
					if is_output_sig d then (
						ps "\n\nreg      "; ps (sig2str (sig2sname d)); ps ";";
						ps "\ninitial  "; ps (sig2str (sig2sname d)); ps " = 0;";
						ps "\nalways   "; ps (sig2str (sig2sname d)); ps " = ";
						print_sgn (sig2sgn d); ps ";"
					) else ()
		) sa.sa_sigs;
	
	pn();
	ps "\nreg    " ; ps alpha; ps ";";
	ps "\nreg    " ; ps beta; ps ";";
	List.iter (fun d -> ps "\nwire   "; ps (Sc.sgn2str d.sc_sgn); ps ";") eqns;
	List.iter
		( fun d -> ps "\nreg    "; ps (Sc.sgn2str d.sc_sgn); ps ";";
					ps "\nwire   "; ps (Sc.sgn2str d.sc_sgn); ps "_w;"
		) sa.sa_mems;
	List.iter
		( fun d -> ps"\nassign  "; ps (Sc.sgn2str d.sc_sgn); ps " = ";
					ps (vlexp2str d.sc_def); ps ";"
		) eqns;
	List.iter (fun d -> ps "\nassign "; ps (Sc.sgn2str d.sc_sgn); ps "_w = ";
					ps (vlexp2str d.sc_def); ps ";"
		) sa.sa_mems;
	pn();
	ps "\n\ninitial";
	ps "\nbegin";
	ps "\n "; ps alpha; ps " = 1;";
	ps "\n "; ps beta; ps " = 0;";
	List.iter
		( fun d -> ps "\n "; ps (Sc.sgn2str d.sc_sgn); ps " = 0;"
		) sa.sa_mems;
	ps "\nend";
	ps "\n\nalways";
	ps "\nbegin";
	ps "\n "; ps alpha; ps " = 0;";
	ps "\n "; ps beta; ps " = 1;";
	List.iter (fun d -> ps "\n "; ps (Sc.sgn2str d.sc_sgn); ps " = ";
					ps (Sc.sgn2str d.sc_sgn); ps "_w;") sa.sa_mems;
	ps "\nend";
	ps "\n\nendmodule\n";
	(* data inputs *);
	info_internal_inputs pc_vis sa

(* -------------------------------------------------------------------------
Generation of SMV format
------------------------------------------------------------------------- *)
let rec smvexp2str =
	function
	| S x -> Sc.sgn2str x
	| CC x -> Err.intern "npexp2str"
	| TT -> "1"
	| FF -> "0"
	| NNot x -> "~("^(smvexp2str x)^")"
	| AAnd el -> "("^(lst_sep2str smvexp2str " & " el)^")"
	| OOr el -> "("^(lst_sep2str smvexp2str " | " el)^")"

let find_ltl cid =
	List.fold_left
		( fun l spec ->
					match spec with
					| Ctl _ -> spec:: l
					| _ -> l
		) [] (Util.i_ast cid).specifications

let pc_smv s = ps "/* "; ps s; ps " */\n"

let make_smv sa specs =
	let inputs = (input_sigs sa)@(data_inputs sa)
	and outputs = (output_sigs sa)
	and locals = (input_sgns sa)@(local_sgns sa)
	and regs = registers sa
	and eqns = sa.sa_eqns
	and alpha = Sc.sgn2str sa.sa_alpha
	and beta = Sc.sgn2str sa.sa_beta
	and peqn l r = ps "\n  "; ps l; ps " := "; ps r; ps ";" in
	let rec print_sgns =
		function
		| [] -> ()
		| [s] -> ps s
		| s:: l -> ps s; ps ","; print_sgns l
	in
	ps ("module main ("); print_sgns (inputs@outputs); ps ")";
	ps "\n\n{";
	ps "\n\ninput "; print_sgns inputs; ps " : boolean;";
	ps "\n\noutput "; print_sgns outputs; ps " : boolean;";
	ps "\n\n";
	print_sgns ([alpha; beta]@locals@regs);
	ps " : boolean;";
	ps "\n";
	List.iter
		( fun d ->
					if is_input_sig d
					then peqn (Sc.sgn2str (sig2sgn d)) (sig2str (sig2sname d))
					else ()
		) sa.sa_sigs;
	ps "\n";
	List.iter
		( fun d ->
					if is_output_sig d
					then peqn (sig2str (sig2sname d)) (Sc.sgn2str (sig2sgn d))
					else ()
		) sa.sa_sigs;
	ps "\n";
	List.iter
		( fun d ->
					match d.sc_act with
					| None -> peqn (Sc.sgn2str d.sc_sgn) (smvexp2str d.sc_def)
					| _ -> ()
		) eqns;
	ps "\n";
	ps "\n  "; ps "init("; ps alpha; ps ") := 1;";
	ps "\n  "; ps "next("; ps alpha; ps ") := 0;";
	ps "\n  "; ps "init("; ps beta; ps ") := 1;";
	ps "\n  "; ps "next("; ps beta; ps ") := 1;";
	List.iter
		( fun d ->
					ps "\n  "; ps "init("; ps (Sc.sgn2str d.sc_sgn); ps ") := 0;";
					peqn ("next("^(Sc.sgn2str d.sc_sgn)^")") (smvexp2str d.sc_def)
		) sa.sa_mems;
	
	ps "\n\nProperties to be checked\n\n";
	List.iter Gen_formula.make_smv_spec specs;
	
	ps "\n\n}";
	
	info_internal_inputs pc_smv sa

(* -------------------------------------------------------------------------
generation of pure synchronous components
------------------------------------------------------------------------- *)
type hw_mode = Blif | Lustre | VIS | SMV

let make_pure_sa mode c sa specs =
	let suffix =
		match mode with
		| Blif -> ".blif"
		| Lustre -> ".lus"
		| VIS -> ".v"
		| SMV -> ".smv" in
	let fn = se.file_prefix^c^suffix in
	let oc = open_out fn in
	push_print_fct (output_string oc);
	try
		let vt =
			"\n\nfor model checking load model in vis:
			\n read_verilog "^fn^
			"\n  init_verify -b" in
		let (pt, st) =
			match mode with
			| Blif -> make_blif sa; ("Blif","")
			| Lustre -> make_lustre sa; ("Lustre","")
			| VIS -> make_vis_model sa; ("VIS-verilog", vt)
			| SMV -> make_smv sa specs; ("SMV","")
		in
		pop_print_fct ();
		close_out oc;
		ps ("\n"^pt^"-format generated. file: "^ fn^ st^"\n")
	with x ->
			pop_print_fct ();
			close_out oc;
			raise x

let make_pure_class mode cid specs =
	try
		let sa = React2sc.rctclass2appl (i_sync_beh cid) in
		make_pure_sa mode (c2str cid) sa specs
	with _ ->
			Err.msg (Err.NoCode)

let make_pure_appl mode cid specs =
	try
		let sa =
			 match Gen_application.ac.Gen_application.ac_sca with
			| None -> Err.msg (Err.NoCode)
			| Some sa -> sa
		in
		make_pure_sa mode (c2str cid) sa specs
	with
	| Error(t) -> pn (); ps t; pF ()
	| _ -> pn (); ps "Internal error of make_pure_appl\n"; pF ()

let err m x =
	let s = Printexc.to_string x in
	ps "\n[[InternalError]] ";
	ps m;
	ps ": could not handle exception: ";
	ps s;
	pn();
	pF()

let make_vis_model cstr () =
	if cstr = "Application" then (
		try
			se.target_sys <- Verification;
			Gen_application.make ();
			make_pure_appl VIS (type2id se.conf_class) []
		with
		| Error(t) -> pn (); ps t; pF ()
		| x -> err "make_vis_model" x
	) else (
		try
			let cid = Lex_wrapper.parse_string Yacc.prs_classid cstr in
			make_pure_class VIS cid []
		with
		| Error(s) -> pn (); ps s; pn (); pF ()
		| x -> err "make_vis_model" x
	)

(* let make_vis_spec cstr fstr () =
if Gen_application.valid () then (
(* needs attention *)
) else (
make_vis_model cstr ()
);
try
let cid = if cstr = "Application"
then type2id se.conf_class
else Lex_wrapper.parse_string Yacc.prs_classid cstr in
let fid = Lex_wrapper.parse_string Yacc.prs_id fstr in
let spec = get_tag (Ly.mf2lbl fid) (i_ast cid).specifications in
Gen_spec.make_vis_spec
(se.file_prefix^(String.lowercase (cstr^"_"^fstr)))
spec
with
(* _ -> Err.ps (Err.TagError(cstr,fstr)) BAD DESIGN, to be redone *)
| x -> err "make_vis_spec" x
*)
let make_smv_model model specs =
	if model = "Application" then (
		try
			Gen_application.make ();
			make_pure_appl SMV (type2id se.conf_class) specs
		with
		| Error(t) -> pn (); ps t; pF ()
		| x -> err "make_smv_model:1" x
	) else (
		try
			let cid = Lex_wrapper.parse_string Yacc.prs_classid model in
			make_pure_class SMV cid specs
		with
		| Error(s) -> pn (); ps s; pn (); pF ()
		| x -> err "make_smv_model:2" x
		(* _ -> Err.ps (Err.NoClass(cstr)) BAD DESIGN, to be redone *)
	)

(* let make_smv_spec cstr fstr () =
if cstr = "Application" then (
try
se.target_sys <- Verification;
Gen_application.make ();
if fstr = "All" then (
let specs = (i_ast (type2id se.conf_class)).specifications in
make_pure_appl SMV (type2id se.conf_class) specs
) else (
Err.intern "make_smv_spec: no All"
)
with
| Error(t) -> pn (); ps t; pF ()
| x -> err "make_smv_spec" x
) else (
try
let cid = Lex_wrapper.parse_string Yacc.prs_classid cstr in
if fstr = "All" then (
let specs = (i_ast cid).specifications in
make_pure_class SMV cid specs
) else (
Err.intern "make_smv_spec: no All"
)
with
| Error(s) -> pn (); ps s; pn (); pF ()
| x -> err "make_smv_spec" x
(* _ -> Err.ps (Err.NoClass(cstr)) BAD DESIGN, to be redone *)
)
*)

(* -------------------------------------------------------------------------
Generate BlIF or Lustre
------------------------------------------------------------------------- *)
let make_format form =
	try
		Gen_application.make ();
		make_pure_appl form (type2id se.conf_class) []
	with
	| Error(t) -> pn (); ps t; pF ()
	| x -> err "make_format" x
