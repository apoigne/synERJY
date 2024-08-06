open Ast
open Util
open P
open Util_print
open Gen_application
open Util_gen

(* ------------------------------------------------------------------------- *)
type trekind =
	| Header
	| Decl
	| Appl
	| AppE

let runtime_engine kind mthd =
	let pf =
		match (kind, mthd) with
		| Appl, _
		| AppE, _ -> ""
		| _,"init" -> "inline void "
		| _,"instant" -> "inline se_Int "
		| _,"output" -> "se_Int "
		| _,"update" -> "se_Int "
		| _ -> Err.intern "runtime_engine"
	and sf =
		match kind with
		| AppE ->
				( match se.target_sys with
					| Simulink -> "((void*)0)"
					| Scicos -> "((void*)0)"
					| _ -> "()"
				)
		| Appl ->
				( match se.target_sys with
					| Simulink -> "(simulinkS);\n"
					| Scicos -> "(block);\n"
					| _ -> "();\n"
				)
		| Decl ->
				( match se.target_sys with
					| Simulink -> " (SimStruct *simulinkS)\n{\n"
					| Scicos -> " (scicos_block *block)\n{\n"
					| _ -> " (void)\n{\n"
				)
		| Header ->
				( match se.target_sys with
					| Simulink -> " (SimStruct *simulinkS);\n"
					| Scicos -> " (scicos_block *block);\n"
					| _ -> " (void);\n"
				)
	and cc = Ly.c2str (type2id se.conf_class)
	in
	pf^cc^"_runtime_engine_"^mthd^sf

(* -------------------------------------------------------------------------
information about builtins:
typedef used in se_def.h
format used by the simulator
cast used by the simulator
init value
------------------------------------------------------------------------- *)
let cc_initval cid =
	try
		let (_, _, _, _, initval) = builtin_info cid in
		ps initval
	with Not_found ->
			ps "(pat_"; psc cid; ps ") 0"

let cc_builtintypedef cid =
	let (td, _, _, _, _) = builtin_info cid in
	td

let rec cc_type2str typ =
	let cid = type2id typ in
	let atl = type2tpl typ in
	let prc = Ly.c2str cid in
	if is_array_typ typ
	then prc
	else prc^(lst_sep2BstrB "__" "" cc_type2str "__" atl)

let cc_typedef_with_no_builtin_prefix typ not_builtin_prefix =
	if typ = Null
	then "void"
	else let cid = type2id typ in
		try
			cc_builtintypedef cid
		with Not_found ->
				not_builtin_prefix^(cc_type2str typ)

let cc_typedef typ =
	cc_typedef_with_no_builtin_prefix typ "pat_"

let cc_entity { p_name = n; p_type = t } =
	if is_sensor_or_signal_typ t then (
		"void *"^(Ly.mf2str n)
	) else (
		let typ = subst_map t cc.cc_v2t in
		let cid = type2id typ in
		( try
			cc_builtintypedef cid
		with Not_found ->
				"pat_"^(cc_type2str typ)
		)^" "^(Ly.mf2str n)
	)

let cc_field_local f =
	let cid = type2id f.p_type in
	try
		(cc_builtintypedef cid)^" "^(Ly.mf2str f.p_name)
	with Not_found ->
			"pat_"^(Ly.c2str cid)^" "^(Ly.mf2str f.p_name)

let get_scan_rtn cid =
	if cid = id_bool then "scan_bool" else
	if cid = id_byte then "scan_byte" else
	if cid = id_short then "scan_short" else
	if cid = id_int then "scan_int" else
	if cid = id_long then "scan_long" else
	if cid = id_char then "scan_char" else
	if cid = id_float then "scan_float" else
	if cid = id_double then "scan_double" else
	if cid = id_string then "scan_String" else
	if cid = id_time then "scan_time" else
		"scan_"^(Ly.c2str cid)

(* =========================================================================
code generation is possible using different styles:
- ExprWordSty each wire and mem occupies a small word (e.g. char)
which is identified by its unique name.
For the reactive engine boolean expressions are
generated using the identifiers as they are
For data expressions the same is true
- ExprBitSty each wire and mem gets a bit. The mapping between its
unique name and the bit is stored in a hashtable.
For the reactive engine boolean expressions are
generated which use bit - set and - test Makros
For data expressions the identifiers are used as they
are, BUT: boolean DATA of VALUED signals is treated
very different in the compiler: it is interpreted
reactively (~~using wires & bits). For these names
bit - set and - test Makros have to be used
- JmpBitStyle each wire and mem gets a bit. The mapping between its
unique name and the bit is stored in a hashtable.
For the reactive engine boolean expressions are compiled
to use conditional jumps to two labels, one for a TRUE,
the other for a FALSE results. Similiar to BDD's an
expression is decomposed to a binary tree
- ... NotYetWritten
NOTES:
- for all bit - styles: each memory bit automatically allocates a wire bit
carrying the value for the next instant. All these new values (...__n)
are copied wordwise to improve performance. The first memory allocated
MUST be alfa!
========================================================================= *)
type tcode_gen_style =
	| ExprBitSty
	| JmpBitStyle
	| ExprWordSty

(* -------------------------------------------------------------------------
bit allocation
------------------------------------------------------------------------- *)
let bits_per_word = 8

type tbitalloc =
	{ mutable wordnum : int; mutable bitnum : int ; prefix : string }

let wire_bits = { wordnum = 0; bitnum = 0; prefix ="__w"; }
let mem_bits = { wordnum = 0; bitnum = 0; prefix ="__m"; }

let bit_name2wb = Hashtbl.create 23

let bit_reset () =
	wire_bits.wordnum <- 0;
	wire_bits.bitnum <- 0;
	mem_bits.wordnum <- 0;
	mem_bits.bitnum <- 0;
	Hashtbl.clear bit_name2wb

let bit_nxt bits str =
	try
		let _ = Hashtbl.find bit_name2wb str in
		Err.intern ("bit_nxt-double:"^str)
	with Not_found ->
			let bit = bits.bitnum in
			let wb = if bit = bits_per_word then (
					bits.bitnum <- 1;
					let num = bits.wordnum + 1 in
					bits.wordnum <- num;
					(bits.prefix^(i2s num), 0)
				) else (
					bits.bitnum <- bit + 1;
					(bits.prefix^(i2s bits.wordnum), bit)
				)
			in
			Hashtbl.add bit_name2wb str wb;
			wb

let bit_get str =
	try
		Hashtbl.find bit_name2wb str
	with Not_found ->
			Err.intern ("bit_get:"^str)

(* -------------------------------------------------------------------------
SWITCH to trigger different code generator for boolean expressions:
- bdd like (jump tables)
- bit operations optimized
- expression optimzed
------------------------------------------------------------------------- *)
let boolgen = ref ExprWordSty

let alloc_wire s =
	let sn = Sc.sgn2str s in
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			let (w, b) = bit_nxt wire_bits sn in
			ps "\n/* "; ps sn;
			let l = 10 - String.length sn in
			let l = if l < 1 then 1 else l in
			for i = 0 to l do
				ps " "
			done;
			ps w; ps "."; pi b; ps " */"
	| ExprWordSty ->
			ps "\nEDEF("; ps sn; ps ");"

let alloc_mem s =
	let sn = Sc.sgn2str s in
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			let (mem_word, bit) = bit_nxt mem_bits sn in
			let nxt_word = mem_word^"__n" in
			Hashtbl.add bit_name2wb (sn^"__n") (nxt_word, bit);
			ps "\n/* "; ps sn;
			let l = 10 - String.length sn in
			let l = if l < 1 then 1 else l in
			for i = 0 to l do ps " " done;
			ps mem_word; ps "."; pi bit; ps " */"
	| ExprWordSty ->
			ps "\nEDEF("; ps sn; ps ");";
			if Sc.is_reg s || Sc.is_pre s || Sc.alpha = s
			then ()
			else ( ps "\nEDEF("; ps sn; ps "__n);" )

let finish_alloc () =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			ps "\nstatic unsigned char ";
			for i = 0 to wire_bits.wordnum do
				ps wire_bits.prefix; pi i; ps ","
			done;
			if mem_bits.wordnum = 0
			then ()
			else ( for i = 0 to (mem_bits.wordnum - 1) do
					ps mem_bits.prefix; pi i; ps ",";
					ps mem_bits.prefix; pi i; ps "__n,"
				done
			);
			ps mem_bits.prefix; pi mem_bits.wordnum; ps ",";
			ps mem_bits.prefix; pi mem_bits.wordnum; ps "__n;\n"
	| ExprWordSty ->
			() (* already done *)

let wire_reset () =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			for i = 0 to wire_bits.wordnum do
				pT 1; ps wire_bits.prefix; pi i; ps " = "
			done;
			ps "0;\n";
			pT 1; ps mem_bits.prefix; ps "0__n = 0; /* alfa=0 */\n";
			if mem_bits.wordnum = 0
			then ( (* only one memory word (with alfa) *) )
			else ( for i = 1 to mem_bits.wordnum do
					pT 1; ps mem_bits.prefix; pi i; ps "__n = "
				done;
				ps " 0;\n"
			)
	| ExprWordSty ->
			() (* implicitly done, as all wire/mem are computed *)

let mem_reset () =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			pT 1; ps mem_bits.prefix; ps "0 = 1; /* alfa=1 */\n";
			if mem_bits.wordnum = 0
			then ( (* only one memory word (with alfa) *) )
			else ( pT 1;
				for i = 1 to mem_bits.wordnum do
					ps mem_bits.prefix; pi i; ps " = "
				done;
				ps " 0;\n"
			)
	| ExprWordSty ->
			() (* implicitly done, as all wire/mem are computed *)

let wire2mem s =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			() (* done by copying whole words *)
	| ExprWordSty ->
			if Sc.is_reg s
			then (
			(* control register do not need to be copied *)
			) else (
				let sn = Sc.sgn2str s in
				pT 1; ps sn; ps " = "; ps sn; ps "__n;\n"
			)

(* --------------------------------------------------------------------------
set_from_expr ... sets a bit 'lhs' to true if bit 'rhs' is true, but does
nothing if it is false (it is asumed, that a bit is
initialized with false

copy_input_boolval sets a bit 'lhs' to 'rhs'. FOR BOOL VALUED INPUT ONLY
-------------------------------------------------------------------------- *)
let set_from_expr tab ~lhs ~rhs =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			let (w, b) = bit_get lhs in
			pT tab; ps rhs; ps " && SET_TRUE("; ps w; ps ","; pi b;
			ps ");\n"
	| ExprWordSty ->
			pT tab; ps lhs; ps " = "; ps rhs; ps ";\n"

let copy_input_boolval tab ~lhs ~rhs =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			let (w, b) = bit_get lhs in
			pT tab; ps "if ("; ps rhs; ps ") { SET_TRUE(";
			ps w; ps ","; pi b; ps "); } else { SET_FALSE(";
			ps w; ps ","; pi b; ps "); };\n"
	| ExprWordSty ->
			pT tab; ps lhs; ps " = "; ps rhs; ps ";\n"

let test s =
	match !boolgen with
	| ExprBitSty ->
			let (w, b) = bit_get s in
			"TEST_VAL("^w^","^(i2s b)^")"
	| JmpBitStyle ->
			let (w, b) = bit_get s in
			"TEST_VAL("^w^","^(i2s b)^")"
	| ExprWordSty ->
			s

let rctengine_reset tab sa =
	match !boolgen with
	| ExprBitSty
	| JmpBitStyle ->
			mem_reset ()
	| ExprWordSty ->
			pT 1; ps (Sc.sgn2str (sa.sa_alpha)); ps " = 1;\n";
			let clr_mem d =
				match d.sc_act with
				| None ->
						pT tab; ps (Sc.sgn2str d.sc_sgn);
						ps " = 0;\n"
				| Some _ -> ()
			in
			List.iter clr_mem sa.sa_mems

(* ========================================================================= *)

(* -------------------------------------------------------------------------
context name
------------------------------------------------------------------------- *)
let cc_name context =
	if context.at_exp && not (is_primitive_typ context.at_type)
	then "((pat_"^cc_type2str context.at_type^")&("^context.at_name^"))"
	else context.at_name

(* -------------------------------------------------------------------------
dynamic bind w.r.t. class cid [[i.e. consider its subclasses]]
------------------------------------------------------------------------- *)
let get_anontyp typ =
	match typ with
	| Typ(cid, pl) ->
			let ast = i_ast cid in
			( match ast.classkind with
				| Anonymous ccid ->
						Some (Typ(ccid, pl))
				| _ -> None
			)
	| _ -> Err.intern "get_anontyp"

let rec get_implementator starttyp targetcid =
	if type2id starttyp = targetcid then (
		starttyp
	) else (
		match get_anontyp starttyp with
		| Some typ -> get_implementator typ targetcid
		| None -> get_implementator (get_supertype starttyp) targetcid
	)

let rec bs_dynbind dynamictyp staticsubtyp = function
	| [] ->
			[(dynamictyp,[staticsubtyp])]
	| (c, cl):: ccl ->
			if c = dynamictyp
			then (dynamictyp, (staticsubtyp:: cl)):: ccl
			else (c, cl):: (bs_dynbind dynamictyp staticsubtyp ccl)

let rec mk_dynbind statictyp mthd len bdgs = function
	| [] ->
			Hashtbl.add cc.cc_dynbind (statictyp, mthd, len) bdgs;
			bdgs
	| t:: tl ->
			let dcl = get_decl_after_tc (type2id t) mthd len in
			let dynamictyp = get_implementator t dcl.origin in
			let bdgs = bs_dynbind dynamictyp t bdgs in
			mk_dynbind statictyp mthd len bdgs tl

let rec subtypes (typ : ttype) = function
	| [] ->
			Err.intern "subtypes"
	| (t, tl):: ttl ->
			if t = typ
			then ( tl : ttype list )
			else subtypes typ ttl

let rec cascade_bind_iter this suf = function
	| [] -> Err.intern "cascade_bind_iter"
	| [(t, _)] ->
			let typ = cc_type2str t in
			"f_"^typ^(suf typ this)
	| (t, il):: bcl ->
			let typ = cc_type2str t
			and chk = this^"->objhead.cid == c_" in
			(lst_sep2str (fun i -> chk^(cc_type2str i)) " || " il)^
			" ? f_"^typ^(suf typ this)^" : "^
			(cascade_bind_iter this suf bcl)

let dynamic_bind lbl context mthd len suf =
	let statictyp = context.at_type
	and this = cc_name context in
	let bdgs = try Hashtbl.find cc.cc_dynbind (statictyp, mthd, len)
		with Not_found ->
				mk_dynbind statictyp mthd len []
					(subtypes statictyp ac.ac_sub)
	in
	if debug_level DbgBinding then (
		let ps = P.primary_ps in
		( match bdgs, context.at_exp with
			| [] , _ -> Err.intern "dynamic_bind"
			| [_], true -> ps "\n[[StaticallyBound-NoNullTest-"
			| [_], false -> ps "\n[[StaticallyBound-NeedsNullTest-"
			| _ , true -> ps "\n[[DynamicallyBound-NoNullTest-"
			| _ , false -> ps "\n[[DynamicallyBound-NeedsNullTest-"
		);
		ps (Err.type2str statictyp); ps "-"; ps (Ly.mf2str mthd);
		ps "-"; ps (i2s len); ps "]]"
	);
	(* generate the if-then-else cascade for dynamic binding *)
	if context.at_exp
	&& List.exists (fun (t, _) -> t = context.at_type) bdgs then (
		let typ = cc_type2str context.at_type in
		"f_"^typ^suf typ this
	) else (
		let exc = "mkExc("^i2s (get_sysprop se.conf_class id_exc_nullptr)^
			","^(Ly.lbl2str lbl)^")" in
		let manybdg = (match bdgs with [_] -> false | _ -> true) in
		let reqlvar = (not context.at_exp) || manybdg in
		let this = if reqlvar then "((pat_Object)"^this^")" else this in
		let pre = if context.at_exp then "" else this^"==Null && "^exc^", " in
		let bind = "("^pre^cascade_bind_iter this suf bdgs^")" in
		bind
	)

let rec dynamic_cast cidp = function
	| [] ->
			Err.intern "dynamic_cast"
	| [t] ->
			cidp^" == c_"^(Err.type2str t)
	| t:: tl ->
			let c = cidp^" == c_"^(Err.type2str t)
			and cl = dynamic_cast cidp tl in
			c^" || "^cl

(* -------------------------------------------------------------------------
compute size of arrays, which can be expanded
------------------------------------------------------------------------- *)
let start () = ()
let stop () = ()

let arraytyp2typpar typ =
	let typ = arraytyp2valtyp typ in
	subst_map typ cc.cc_v2t

let array2size name dcl =
	let err () = Err.msg (Err.NYI ("this kind of array initialization is not \
					implemented (e.g. negative bounds"))
	and cid = dcl.origin
	and init = get_fldinitexpr_after_tc dcl in
	match init.expr with
	| New(typ, apl) ->
			( match (typ, apl) with
				| Array(t, _, _),[i] ->
						let inum = int64_2_int (eval_const_expr cid i) in
						if inum <= 0
						then err()
						else (inum, 0, subst_map t cc.cc_v2t)
				| Array(t, _, _),[i1; i2] ->
						let inu1 = int64_2_int (eval_const_expr cid i1)
						and inu2 = int64_2_int (eval_const_expr cid i2) in
						if inu1 <= 0 || inu2 <= 0
						then err()
						else (inu1, inu2, subst_map t cc.cc_v2t)
				| _ -> Err.intern "array2size:1"
			)
	| ArrayLiteral (Dim1 l) ->
			let at = arraytyp2typpar (some init.etyp "array2size:2")
			and len = List.length l.av1 in
			if len <= 0
			then err()
			else (len, 0, at)
	| ArrayLiteral (Dim2 l) ->
			let at = arraytyp2typpar (some init.etyp "array2size:3")
			and l1 = List.length l.av2
			and l2 = List.length (List.hd l.av2) in
			if l1 <= 0 || l2 <= 0
			then err()
			else (l1, l2, at)
	| _ -> Err.intern "array2size:4"

let cc_length context xe dim len =
	let typ = some xe.etyp "cc_length" in
	if context.at_exp then (
		let name = context.at_name^"."^len in
		mk_ctxt name typ
	) else (
		let lbl = Ly.lbl2str xe.elbl in
		let tmpvar = alloc_tmpvar () in
		let tmpptr = tmpvar2ptr (Some tmpvar) in
		let name = "({"^tmpptr^"=(pat_Object)"^cc_name context
			^";check_pointer("^tmpptr^","^lbl^");"
			^"(se_Int)((pat_array"^dim^")"^tmpptr^")->"^len^";})"
		in
		free_tmpvar tmpvar;
		mk_ctxt name typ
	)

(* =========================================================================
C - code generation for expressions
========================================================================= *)
(* -------------------------------------------------------------------------
code generation for expressions
------------------------------------------------------------------------- *)
let cc_constant ?(lbl = Ly.nolbl) (t, s, v) =
	if is_integral_typ t || t = t_time || le_type ~sub: t ~super: t_cint64
	|| le_type ~sub: t ~super: t_cuint64 then (
		Int64.to_string (some v "cc_constant")
	) else if t = t_time then (
		let v = some v "cc_constant" in
		if Int64.rem v (Int64.of_int se.timescale) = Int64.zero
		then Int64.to_string (Int64.div v (Int64.of_int se.timescale))
		else Err.msg
				(Err.TimeGranularity (Int64.to_string v, i2s se.timescale,
						type2id cc.cc_typ, lbl))
	) else if t = t_float || t = t_double then (
		s
	) else if t = t_bool then (
		if s ="true" then "True" else
		if s ="false" then "False" else
			Err.intern "cc_constant"
	) else if t = t_string then (
		"\""^(String.escaped s)^"\""
	) else (
		Err.intern "cc_constant"
	)

let cc_field context mf_typ mf =
	let mf_typ = some mf_typ "cc_field" in
	let mf_typ = subst_map mf_typ cc.cc_v2t in
	let cid = type2id context.at_type in
	try
		let dcl = get_decl_after_tc cid mf (- 1) in
		(* ------- all cases are field accesses ------------------------- *)
		try if is_sensor_or_signal_typ mf_typ then
				mk_ctxt "Null" mf_typ
			else
				let fld = if dcl.scope = Class then (
						match native2name dcl with
						| Some s -> s
						| None -> "sta_"^(Ly.c2str dcl.origin)^"_"^
								(Ly.mf2str mf)
					) else if dcl.scope = Single then (
						match native2name dcl with
						| Some s -> s
						| None -> "sgl_"^(Ly.c2str dcl.origin)^"_"^
								(Ly.mf2str mf)
					) else if context.at_exp then (
						context.at_name^"."^(Ly.mf2str mf)
					) else (
						context.at_name^"->"^(Ly.mf2str mf)
					)
				in
				let can_exp = can_expand dcl in
				let typ = if can_exp
					then get_expandtyp dcl
					else mf_typ in
				{ at_name = fld;
					at_type = typ;
					at_exp = can_exp;
				}
		with _ -> Err.intern "cc_field"
	with _ ->
	(* ------- formal or local parameter -------------------------------- *)
			mk_ctxt (Ly.mf2str mf) mf_typ

let rec assgn2exp_flds ?(fld_exp = false) f rhs dcl =
	let target = if is_static dcl
		then (if dcl.scope = Class then "&sta_" else "&sgl_")
		^(cc_type2str cc.cc_typ)^"_"^(Ly.mf2str f)
		else "&"^cc.cc_this_ctxt.at_name^"->"^(Ly.mf2str f) in
	match rhs.expr with
	| New(typ, apl) ->
			( match typ with
				| Array(_, DimLen 1, _)
				| Array(_, _, DimLen 1) ->
						let (len, _, typ) = array2size f dcl in
						let at = cc_typedef typ in
						"ARRAYI1("^target^","^(i2s len)^",sizeof("^at^"))"
				| Array(_, _, _) ->
						let (l1, l2, typ) = array2size f dcl in
						let at = cc_typedef typ in
						"ARRAYI2("^target^","^(i2s l1)^","^(i2s l2)^",sizeof("^at^"))"
				| _ ->
						let prt = cc_type2str typ in
						"f_"^prt^"_"^(Ly.c2str (type2id typ))^(apl2str apl)^
						"(i_"^prt^"((pat_"^prt^")"^target^")"^
						(lst_sep2BstrB "," "" cc_expr "," apl)^")"
			)
	| ArrayLiteral (Dim1 l) ->
			"ARRAYC1("^target^",&"^(cc_type2str cc.cc_typ)^
			"_al"^(i2s l.an1)^","^(i2s (List.length l.av1))^",sizeof("^
			(cc_typedef (arraytyp2typpar (some rhs.etyp "assgn2exp_flds:1"))
			)^"))"
	| ArrayLiteral (Dim2 l) ->
			"ARRAYC2("^target^",&"^(cc_type2str cc.cc_typ)^"_al"^
			(i2s l.an2)^","^
			(i2s (List.length l.av2))^","^
			(i2s (List.length (List.hd l.av2)))^
			",sizeof("^
			(cc_typedef (arraytyp2typpar (some rhs.etyp "assgn2exp_flds:2"))
			)^"))"
	| _ when dcl.scope = Single ->
			let rt = dcl.signature.rt in
			let ctxt = cc_gen rhs in
			let rhs = ctxt.at_name in
			let fld = "sgl_"^cc_type2str cc.cc_typ^"_"^(Ly.mf2str f) in
			( match fld_exp, ctxt.at_exp with
				| true, true ->
						"("^fld^" = (tat_"^cc_type2str rt^")"^rhs^")"
				| true, false ->
						"("^fld^" = (tat_"^cc_type2str rt^")*"^rhs^")"
				| false, true ->
						"("^fld^" = (pat_"^cc_type2str rt^")&"^rhs^")"
				| false, false ->
						"("^fld^" = (pat_"^cc_type2str rt^")"^rhs^")"
			)
	
	| _ -> Err.intern "assgn2exp_flds:3"

and cc_param apl =
	lst_sep2str (fun ap -> (cc_gen ap).at_name) "," apl

and cc_expr xe =
	(cc_gen xe).at_name

and cc_gen xe =
	let ctxt = cc_call cc.cc_this_ctxt xe in
	{ ctxt with at_name = cc_name ctxt }

and cc_call context xe =
	let some x = some x "cc_call"
	and lbl = Ly.lbl2str xe.elbl in
	match xe.expr with
	
	| Call(mf, None) when mf = id_timing ->
			mk_ctxt "TIMING" t_time
	
	| Call(mf, None) when mf = id_op__i ->
			mk_ctxt "__i" t_int
	| Call(mf, None) when mf = id_op__j ->
			mk_ctxt "__j" t_int
	| Call(mf, None) when mf = id_op__k ->
			mk_ctxt "__k" t_int
	| Call(mf, None) ->
			cc_field context xe.etyp mf
	
	| Dot({ expr = DeltaT },{ expr = Call(o, Some []) }) when o = id_op_value ->
			cc.cc_has_timing <- true;
			let ie = get_sysprop se.conf_class id_exc_deltat in
			mk_ctxt ("("^test (Sc.sgn2str Sc.alpha)
					^"?(mkExc("^i2s ie^
					",0),(se_Double)0.0):((se_Double)TIMING/1.0E6)") t_time
	
	| Dot({ expr = DotDot(sv, xs, offset) },{ expr = Call(op, Some [ap]) }) when op = id_op_get ->
			let typ = some xe.etyp in
			let sv = Sc.sgn2str sv in
			let name =
				let buf_size = dims2value(sig2buf xs) in
				if buf_size < 2 then (
					sv
				) else if buf_size = 2 && not(is_matrix_typ typ) then (
					sv^"[0]"
				) else (
					sv^ ".b.a[(__b="^sv^".pt+"^cc_expr ap^
					(if offset > 0 then "+"^i2s offset else "")
					^","^"__b>"^i2s (buf_size - 1)^"?__b-"^i2s buf_size^":__b)]"
				)
			in
			mk_ctxt name typ
	
	(* dealing with the all matrix *)
	| Dot(e,{ expr = Call(mf1, Some []) }) when mf1 = id_op_all ->
			mk_ctxt (cc_expr e) (some e.etyp)
	
	(* dealing with the diagonal matrix *)
	| Dot({ expr = Dot(e,{ expr = Call(mf1, Some []) }); etyp = t },
	{ expr = Call(mf2, Some [ap1; ap2]) })
	when mf1 = id_op_diagonal && mf2 = id_op_get ->
			( match some t with
				| Array(t, d1, d2)
				when dim2value d1 > 1 && dim2value d2 > 1 ->
						let name = "(("^cc_expr ap1^"=="^cc_expr ap2^")?("
							^cc_expr e ^"):(0.0))"
						in
						mk_ctxt name t
				| _ -> Err.intern "id_op_diagonal & mf2 = id_op_get:matrix"
			)
	
	| Dot(e1, e2) ->
			cc_call (cc_call context e1) e2
	
	| Call(op, Some [p]) when op = id_op_equal || op = id_op_not_equal ->
			let lhs = context
			and rhs = cc_gen p
			and opr = if op = id_op_equal then " == " else " != " in
			let cmp = if (is_basic_type_equal lhs.at_type rhs.at_type) then (
					if lhs.at_type = t_string then (
						"("^(if op = id_op_equal then "" else "!")^
						"f_string_eq ("^lhs.at_name^","^rhs.at_name^"))"
					) else if lhs.at_type = t_bool then (
						if op = id_op_equal then (
							"( !("^lhs.at_name^" || "^rhs.at_name^") || ("^
							lhs.at_name^" && "^rhs.at_name^") )"
						) else (
							"( (!"^lhs.at_name^" && "^rhs.at_name^") || ("^
							lhs.at_name^" && !"^rhs.at_name^") )"
						)
					) else (
						"("^lhs.at_name^opr^rhs.at_name^")"
					)
				) else (
					"((pat_object)"^cc_name lhs^opr
					^"(pat_object)"^rhs.at_name^")"
				)
			in
			mk_ctxt cmp t_bool
	
	| Call(op, Some [ap]) when op = id_op_get ->
			cc_array1_get xe.elbl context ap
	
	| Call(op, Some[ap1; ap2]) when op = id_op_get ->
			cc_array2_get xe.elbl context ap1 ap2
	
	| Call(op, Some apl)
	when (is_matrix_typ context.at_type || is_matrix_typ (some xe.etyp))
	&& is_matrix_op op ->
			let op = if op = id_op_pointmult then id_op_mult else op
			(* A really BAD HACK; needed to deal with vector multiplication
			can be eliminated if overloading is implemented *)
			and ctyp = match some xe.etyp with
				| Array(t, _, _) -> t
				| _ -> Err.intern ("cc_call:matrix_op"^Ly.mf2str op)
			in
			let cid = type2id ctyp in
			( match (apl, primop2str op) with
				| [ap], Some s ->
						let name = "("^context.at_name^s^cc_expr ap^")" in
						mk_ctxt name ctyp
				| [], Some s ->
						let name = "("^s^context.at_name^")" in
						mk_ctxt name ctyp
				| _ ->
						let name = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str op)^
							"("^cc_name context^
							(if apl = [] then "" else ",")^cc_param apl^")" in
						mk_ctxt name ctyp
			)
	
	| Call(op, Some []) when op = id_op_length ->
			cc_length context xe "1" "len"
	
	| Call(op, Some []) when op = id_op_rows ->
			cc_length context xe "2" "len1"
	
	| Call(op, Some []) when op = id_op_cols ->
			cc_length context xe "2" "len2"
	
	| Call(mf, Some apl) ->
			let vt = context.at_type in
			let (dcl, ctyp) =
				try ( get_decl_after_tc (type2id vt) mf (List.length apl), vt)
				with _ ->
						if (is_literal_type context.at_type) then (
							match apl with
							| [p] ->
									let ptyp = some p.etyp in
									( get_decl_after_tc (type2id ptyp) mf (List.length apl)
										, ptyp
									)
							| _ -> Err.intern ("cc_call:invlit:"^Ly.mf2str mf)
						) else (
							Err.intern ("cc_call:nolit:"^Ly.mf2str mf)
						)
			in
			let cid = type2id ctyp in
			if (is_method_or_constructor dcl) && not( is_rct_method dcl )
			then ()
			else Err.intern "cc_call:nomthd";
			if (is_literal_type ctyp) then (
				if apl != [] then Err.intern "cc_call:lits";
				mk_ctxt context.at_name (some xe.etyp)
			) else if ctyp = t_time then (
				let n = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^"("^context.at_name^
					(if apl = [] then "" else ",")^cc_param apl^")" in
				mk_ctxt n (some xe.etyp)
			) else if (is_primitive_typ ctyp) then (
				( match (apl, primop2str mf) with
					| [ap], Some s ->
							let n = "("^context.at_name^s^cc_expr ap^")" in
							mk_ctxt n (some xe.etyp)
					| [], Some s ->
							let n = "("^s^context.at_name^")" in
							mk_ctxt n (some xe.etyp)
					| _ ->
							let n = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^"("^
								cc_name context^(if apl = [] then "" else ",")^
								cc_param apl^")" in
							mk_ctxt n (some xe.etyp)
				)
			) else if dcl.scope = Class then (
				let name = "f_"^(Ly.c2str dcl.origin)^"_"^(Ly.mf2str mf)^(apl2str apl)
					^"("^(if apl = [] then "" else cc_param apl)^")" in
				mk_ctxt name (some xe.etyp)
			) else if dcl.scope = Single then (
				let name = "f_"^(Ly.c2str dcl.origin)^"_"^(Ly.mf2str mf)^(apl2str apl)
					^"((pat_"^cc_type2str context.at_type^")"^
					(if context.at_exp then "&" else "")^context.at_name^
					(if apl = [] then " " else ",")^cc_param apl^")" in
				mk_ctxt name (some xe.etyp)
			) else (
				let len = List.length apl
				and c1 = "_"^(Ly.mf2str mf)^(apl2str apl)^"((pat_"
				and c2 = (if apl = [] then "" else ",")^cc_param apl^")" in
				let suf imp this = c1^imp^")("^this^")"^c2 in
				let name = dynamic_bind xe.elbl context mf len suf in
				mk_ctxt name (some xe.etyp)
			)
	
	| ClassCall(cid, mf, None) ->
			let dcl = try let dcl = get_decl_after_tc cid mf (- 1) in
					if is_field dcl
					then dcl
					else Err.intern "cc_call"
				with Not_found -> Err.intern "cc_call" in
			(* ------- static fields ---------------------------------------- *)
			let can_exp = can_expand dcl in
			if is_static dcl then (
				let typ = some xe.etyp in
				let typ = subst_map typ cc.cc_v2t in
				let fld =
					match native2name dcl with
					| Some s -> s
					| None ->
							(if dcl.scope = Class then "sta_" else "sgl_")
							^(Ly.c2str dcl.origin)^"_"^(Ly.mf2str mf) in
				(* mk_ctxt fld (some expr.etyp) XXX *)
				{ at_name = fld;
					at_type = typ;
					at_exp = can_exp;
				}
			) else (
				Err.intern "cc_call:static final"
			)
	
	| ClassCall(cid, mf, Some apl) ->
			let dcl =
				try let dcl = get_decl_after_tc cid mf (List.length apl) in
					if is_field dcl
					then Err.intern "cc_call"
					else dcl
				with Not_found -> Err.intern "cc_call"
			in
			if (is_method_or_constructor dcl) && not( is_rct_method dcl ) then (
				let name = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^
					(apl2str apl)^" ("^cc_param apl^")" in
				mk_ctxt name (some xe.etyp)
			) else (
				Err.intern "cc_call"
			)
	| New(typ, apl) ->
			( match (typ, apl) with
				| (Array(t, DimLen 1, d),[_])
				| (Array(t, d, DimLen 1),[_]) when dim2value d > 0 ->
						let name = "((pat_vector)ARRAYM1("^i2s(dim2value d)^
							",sizeof("^cc_typedef t^")))"
						in
						mk_ctxt name (some xe.etyp)
				| (Array(t, DimLen 1, _),[e])
				| (Array(t, _, DimLen 1),[e]) ->
						let tmpvar = alloc_tmpvar () in
						let tmpstr = tmpvar2int tmpvar in
						let size = i2s (get_sysprop se.conf_class id_exc_arraysize) in
						let name =
							
							"(pat_array1)("^tmpstr^"="^(cc_expr e)^","^
							tmpstr^"<0 && mkExc("^size^","^lbl^"), "^
							"ARRAYM1("^tmpstr^",sizeof("^(cc_typedef t)^")))" in
						free_tmpvar tmpvar;
						mk_ctxt name (some xe.etyp)
				| (Array(t, d1, d2),[_; _]) when dim2value d1 > 0 && dim2value d2 > 0 ->
				(* see above *)
						let name = "((pat_matrix)ARRAYM2("^i2s (dim2value d1)^","
							^i2s (dim2value d2)^",sizeof("^(cc_typedef t)^")))" in
						mk_ctxt name (some xe.etyp)
				| (Array(t, _, _),[e1; e2]) ->
						let tmpvar1 = alloc_tmpvar () in
						let tmpvar2 = alloc_tmpvar () in
						let tmpstr1 = tmpvar2int tmpvar1 in
						let tmpstr2 = tmpvar2int tmpvar2 in
						let size = i2s (get_sysprop se.conf_class id_exc_arraysize) in
						let name =
							"(pat_array2)("^tmpstr1^"="^(cc_expr e1)^", "^
							tmpstr2^"="^(cc_expr e2)^", "^
							tmpstr1^"<0 && mkExc("^size^","^lbl^"), "^
							tmpstr2^"<0 && mkExc("^size^","^lbl^"), "^
							"ARRAYM2("^tmpstr1^","^tmpstr2^
							",sizeof("^(cc_typedef t)^")))" in
						free_tmpvar tmpvar2;
						free_tmpvar tmpvar1;
						mk_ctxt name (some xe.etyp)
				| _ ->
						let cid = cc_type2str (subst_map typ cc.cc_v2t)
						and fid = Ly.c2str (type2id typ) in
						let name = "((pat_"^cid^")f_"^cid^"_"^fid^(apl2str apl)
							^" (i_"^cid^" (n_"^cid^"())"^
							(lst_sep2BstrB "," "" cc_expr "," apl)^"))" in
						mk_ctxt name typ
			)
	
	| Super(fm, apl) ->
			let len = List.length apl in
			let dcll = Hashtbl.find (i_ast (type2id cc.cc_typ)).symtab (fm, len) in
			let dcll = match dcll with
				| [] -> Err.intern "cc_call:super"
				| _:: t -> t in
			let dcl = list_first_match (fun d -> not( is_abstract d )) dcll in
			let rec iter_supertype typ =
				let typ = get_supertype typ in
				if (type2id typ) = dcl.origin
				then typ
				else iter_supertype typ
			in
			let typ = cc_type2str (iter_supertype cc.cc_typ) in
			let name = "f_"^typ^"_"^(Ly.mf2str fm)^(apl2str apl)^
				" ("^cc_param apl^")" in
			mk_ctxt name (some xe.etyp)
	
	| Cast(to_typ, e) ->
			let ctxt = cc_gen e in
			let subl = subtypes (some xe.etyp) ac.ac_sub in
			if ctxt.at_exp && List.mem ctxt.at_type subl then (
				mk_ctxt ctxt.at_name to_typ
			) else (
				let tmpvar = alloc_tmpvar () in
				let tmpptr = tmpvar2ptr (Some tmpvar) in
				let t = tmpptr^"->objhead.cid" in
				let ctxt = if List.mem to_typ subl then ( (* down cast? *)
						let name = "("^tmpptr^"=(pat_Object)"^ctxt.at_name^","^
							(dynamic_cast t subl)^" ? "^tmpptr^" : Null)" in
						mk_ctxt name (some xe.etyp)
					) else ( (* up cast or identity cast *)
						ctxt
					)
				in
				free_tmpvar tmpvar;
				ctxt
			)
	
	| This ->
			mk_ctxt (this_name ()) context.at_type
	
	| ThisConstr(apl) ->
			let typ = cc_type2str cc.cc_typ in
			let name = "f_"^typ^"_"^typ^apl2str apl^" ("^this_name()^
				(lst_sep2BstrB "," "" cc_expr "," apl)^")" in
			mk_ctxt name Null
	
	| SuperConstr(apl) ->
			let super = get_supertype cc.cc_typ in
			let scid = type2id super in
			let super = cc_type2str super in
			let name = "f_"^super^"_"^Ly.c2str scid^apl2str apl^
				" ("^"(pat_"^super^") "^this_name()^
				(lst_sep2BstrB "," "" cc_expr "," apl)^")" in
			mk_ctxt name Null
	
	| NullObj ->
			mk_ctxt "Null" Null
	
	| IncrDecr(mf, o) ->
			let f = "("^cc_name (cc_field context xe.etyp mf)^")" in
			let n = if o = id_op_prefix_incr then "(++"^f^")" else
				if o = id_op_prefix_decr then "(--"^f^")" else
				if o = id_op_postfix_incr then "("^f^"++)" else
				if o = id_op_postfix_decr then "("^f^"--)" else
					Err.intern "cc_call:++--" in
			mk_ctxt n (some xe.etyp)
	
	| Assign(f, idx, kind, rhs) ->
			let cid = type2id context.at_type
			and rt = subst_map (some xe.etyp) cc.cc_v2t in
			let prim = is_primitive_typ rt
			and expansion =
				try let dcl = get_decl_after_tc cid f (- 1) in
					if can_expand dcl
					then Some dcl
					else None
				with _ -> None
			and asg =
				if kind = id_op_assign then (
					" = "
				) else (
					match primop2str kind with
					| Some s -> " "^s^"= "
					| None -> Err.intern "cc_call:primop2str"
				) in
			( match expansion, idx with
				| Some dcl, NoArr when not (prim) ->
				(* assumption: assignment done in constructor or at decl,
				only to initialize [[NOT TO CREATE]] expanded objects *)
						if kind = id_op_assign
						then ()
						else Err.intern "cc_call: init_exp_kind";
						let fld_exp = (cc_field context xe.etyp f).at_exp in
						mk_ctxt (assgn2exp_flds ~fld_exp: fld_exp f rhs dcl) rt
				| _, NoArr ->
				(* no arrays, primitive or not expandable fields *)
						let fld = (cc_field context xe.etyp f).at_name in
						let cast = if prim then "" else "(pat_"^cc_type2str rt^")" in
						mk_ctxt ("("^fld^asg^cast^(cc_gen rhs).at_name^")") rt
				| _, ADim1 i ->
				(* 1 dim array *)
						( match some xe.etyp with
							| Array(t, DimLen 1, d)
							| Array(t, d, DimLen 1) when dim2value d > 0 ->
									let vtyp = cc_typedef t in
									let ctxt = cc_field context xe.etyp f in
									let fld = ctxt.at_name
									and idx = (cc_gen i).at_name
									and rhs = (cc_gen rhs).at_name in
									let name =
										if ctxt.at_exp
										then fld^".a["^idx^"]"^asg^rhs
										else "(("^vtyp^"*)&("^fld^"->a))["^idx^"]"^asg^rhs
									
									in
									mk_ctxt name rt
							| Array(t, DimLen 1, _)
							| Array(t, _, DimLen 1) ->
									let tmpvar1 = alloc_tmpvar ()
									and tmpvar2 = alloc_tmpvar () in
									let tmpptr = tmpvar2ptr (Some tmpvar1)
									and tmpx = tmpvar2int tmpvar2
									and ctxt = cc_field context xe.etyp f in
									let fld = ctxt.at_name
									and idx = (cc_gen i).at_name
									and rhs = (cc_gen rhs).at_name in
									let fld = if ctxt.at_exp then "&"^fld else fld in
									let name =
										"({"^tmpptr^"=(pat_Object)"^fld^";"^tmpx^"="^idx^";"
										^"check_pointer("^tmpptr^","^lbl
										^")||check_index("^tmpptr^","^tmpx^","^lbl^");(("
										^cc_typedef t^"*)(&((pat_array1)"^tmpptr
										^")->a))["^tmpx^"]"^asg^rhs^";})"
									in
									free_tmpvar tmpvar2;
									free_tmpvar tmpvar1;
									mk_ctxt name rt
							| _ -> Err.intern "cc_call:arrayt1"
						)
				| _, ADim2(x, y) ->
				(* 2 dim array *)
						( match some xe.etyp with
							| Array(t, d1, d2) when dim2value d1 > 0 && dim2value d2 > 0 ->
									let ctxt = cc_field context xe.etyp f in
									let fld = ctxt.at_name
									and idx = (cc_gen x).at_name
									and idy = (cc_gen y).at_name
									and rhs = (cc_gen rhs).at_name
									and vtyp = cc_typedef t in
									let name =
										if ctxt.at_exp
										then fld^".a["^idx^"]["^idy^"]"^asg^rhs
										else "(("^vtyp^"*)&("^fld^"->a))["^
											idx^"]["^idy^"]"^asg^rhs
									in
									mk_ctxt name rt
							| Array(t, _, _) ->
							(* was: (if expansion=None then fld else "&"^fld) *)
									let tmpvar1 = alloc_tmpvar () in
									let tmpvar2 = alloc_tmpvar () in
									let tmpvar3 = alloc_tmpvar () in
									let tmpptr = tmpvar2ptr (Some tmpvar1) in
									let tmpi1 = tmpvar2int tmpvar2 in
									let tmpi2 = tmpvar2int tmpvar3 in
									let ctxt = cc_field context xe.etyp f in
									let fld = ctxt.at_name
									and idx = (cc_gen x).at_name
									and idy = (cc_gen y).at_name
									and rhs = (cc_gen rhs).at_name
									and vtyp = cc_typedef t in
									let fld = if ctxt.at_exp
										then "&"^fld
										else fld in
									let name =
										"({"^tmpptr^"=(pat_Object)"^fld^";"
										^tmpi1^"="^idx^";"^tmpi2^"="^idy^";"
										^"check_pointer("^tmpptr^","^lbl
										^");check_indices("^tmpptr^","^tmpi1^","^tmpi2^","
										^lbl^");(("^vtyp^"*)(&((pat_array2)"^tmpptr^")->a))["
										^tmpi1^"*((se_Int)((pat_array2)"
										^tmpptr^")->len2)+"^tmpi2^"]"^asg^rhs^";})"
									in
									free_tmpvar tmpvar3;
									free_tmpvar tmpvar2;
									free_tmpvar tmpvar1;
									mk_ctxt name rt
							| _ -> Err.intern "cc_call:array21"
						)
			)
	
	| If(tl) ->
			let rec theni =
				function
				| [] ->
						""
				| [Else(_,[ExprStmt(_, s)])] ->
						cc_expr s
				| Then(_, e,[ExprStmt(_, s)]):: tl ->
						"(("^(cc_expr e)^") ? "^
						(cc_expr s)^" : "^(theni tl)^")"
				| _ -> Err.intern "cc_call:then"
			in
			mk_ctxt (theni tl) (some xe.etyp)
	
	| Literal c ->
			mk_ctxt (cc_constant ~lbl: xe.elbl c) (some xe.etyp)
	
	| ArrayLiteral (Dim1 a) ->
			let typ = cc_type2str cc.cc_typ in
			( match some xe.etyp with
				| Array(t, DimLen 1, d)
				| Array(t, d, DimLen 1) as ct when dim2value d > 0 ->
						{ at_name = typ^"_al"^(i2s a.an1);
							at_type = ct;
							at_exp = true;
						}
				| _ ->
						mk_ctxt("()&"^typ^"_al"^(i2s a.an1)) (some xe.etyp)
			)
	
	| ArrayLiteral (Dim2 a) ->
			let typ = cc_type2str cc.cc_typ in
			( match some xe.etyp with
				| Array(t, d1, d2) as ct when dim2value d1 > 0 && dim2value d2 > 0 ->
						{ at_name = typ^"_al"^(i2s a.an2);
							at_type = ct;
							at_exp = true;
						}
				| _ ->
						mk_ctxt("(pat_array2)&"^typ^"_al"^(i2s a.an2)) (some xe.etyp)
			)
	
	| DeltaT ->
			cc.cc_has_timing <- true;
			let ie = get_sysprop se.conf_class id_exc_deltat in
			mk_ctxt ("("^test (Sc.sgn2str Sc.alpha)
					^"?(mkExc("^i2s ie^
					",0),(se_Double)0.0):(((se_Double)TIMING)/1.0E6))") t_double
	
	| Instant ->
			mk_ctxt (runtime_engine AppE "instant") t_int
	
	| Present(s) ->
			mk_ctxt (test (Sc.sgn2str s)) (some xe.etyp)
	
	| Timestamp(s) ->
			cc.cc_has_timing <- true;
			mk_ctxt ("("^test (Sc.sgn2str s)^")?((se_Time)0):("^
					(Sc.sgn2str s)^"__t)") (some xe.etyp)
	
	| SigVal(_, xs, offset) ->
			let s = Sc.sgn2val (sig2sgn xs) in
			let typ = some xe.etyp in
			let is_mat = is_matrix_typ (sig2valtyp xs) in
			let sv = Sc.sgn2str s in
			let sv = if typ = t_bool then test sv else
				if is_primitive_typ typ && not( typ = t_string )
				then sv
				else sv in
			let buf_size = dims2value(sig2buf xs) in
			let name =
				if buf_size < 2 then (
					sv
				) else if buf_size = 2 && not is_mat then (
					sv^"["^i2s offset^"]"
				) else (
					sv^".b.a[(__b="^sv^".pt"^
					(if offset > 0 then "+"^i2s offset else "")
					^","^"__b>"^i2s (buf_size - 1)^"?__b-"^i2s buf_size^":__b)]"
				) in
			{ at_name = name;
				at_type = typ;
				at_exp = true;
			}
	
	| Value(s) ->
			let typ = some xe.etyp in
			let sv = Sc.sgn2str s in
			let name = if typ = t_bool && not(Sc.is_var s)
				then test sv
				else
				if is_primitive_typ typ && not( typ = t_string )
				|| is_matrix_typ typ
				then sv
				else "((pat_"^cc_type2str typ^")&("^sv^")))"
			in
			{ at_name = name;
				at_type = typ;
				at_exp = true;
			}
	
	| DotDot(_, _, _) -> Err.intern "cc_call:DotDot"
	| Slice _ -> Err.intern "cc_call:Slice"
	| Var _ -> Err.intern "cc_call:Var"
	
	| Offset(index, offset) ->
			let typ = some xe.etyp in
			let name = (cc_call context index).at_name^"+"
				^(cc_call context offset).at_name
			in
			mk_ctxt name typ

and mk_vector_ctxt context t v =
	let name =
		if context.at_exp
		then context.at_name^".a["^v^"]"
		else let vtyp = cc_typedef t in
			"(("^vtyp^"*)&("^cc_name context^"->a))["^v^"]"
	in
	mk_ctxt name t

and mk_matrix_ctxt context t v1 v2 =
	let name =
		if context.at_exp
		then context.at_name^".a["^v1^"]["^v2^"]"
		else let vtyp = cc_typedef t in
			"(("^vtyp^"*)&("^cc_name context^"->a))["^v1^"]["^v2^"]"
	in
	mk_ctxt name t

and mk_array1_ctxt lbl context t ap =
	let vtyp = cc_typedef t in
	let lbl = Ly.lbl2str lbl in
	let tmpvar1 = alloc_tmpvar () in
	let tmpvar2 = alloc_tmpvar () in
	let tmpptr = tmpvar2ptr (Some tmpvar1) in
	let tmpi = tmpvar2int tmpvar2 in
	let name = "({"^tmpptr^"=(pat_Object)"^cc_name context^";"
		^tmpi^"="^cc_expr ap
		^";check_pointer("^tmpptr^","^lbl
		^");check_index("^tmpptr^","^tmpi^","^lbl^");"^
		"(("^vtyp^"*)(&((pat_array1)"^tmpptr^ ")->a))["^tmpi^"]"^";})"
	in
	free_tmpvar tmpvar2;
	free_tmpvar tmpvar1;
	mk_ctxt name t

and cc_array1_get lbl context ap =
	match context.at_type, ap.expr with
	| Array(t, DimLen 1, d), Call(mf, None)
	| Array(t, d, DimLen 1), Call(mf, None) when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
			mk_vector_ctxt context t (cc_expr ap)
	| Array(t, DimLen 1, d), Literal(_, v, _)
	| Array(t, d, DimLen 1), Literal(_, v, _) when dim2value d > 0 ->
			if 0 <= s2i v && s2i v <= (dim2value d) - 1 then (
				mk_vector_ctxt context t v
			) else (
				Err.msg(Err.Index(type2id cc.cc_typ, ap.elbl))
			)
	| Array(t, DimLen 1, d), Offset _
	| Array(t, d, DimLen 1), Offset _ when dim2value d > 0 ->
			mk_vector_ctxt context t (cc_call context ap).at_name
	| Array(t, DimLen 1, d), Dot({ expr = Call(mf, None) }, _)
	| Array(t, d, DimLen 1), Dot({ expr = Call(mf, None) }, _) when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
			mk_vector_ctxt context t (cc_expr ap)
	| Array(t, DimLen 1, d), _
	| Array(t, d, DimLen 1), _ when dim2value d > 0 ->
			mk_vector_ctxt context t (cc_expr ap)
	| Array(t, DimLen 1, _), _
	| Array(t, _, DimLen 1), _ ->
			mk_array1_ctxt lbl context t ap
	| _ -> Err.intern "cc_array1_get"

and cc_array2_get lbl context ap1 ap2 =
	match context.at_type, ap1.expr, ap2.expr with
	| Array(t, DimLen 1, _), _, Call(mf, None) when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
			mk_vector_ctxt context t (cc_expr ap2)
	| Array(t, _, DimLen 1), Call(mf, None), _ when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
			mk_vector_ctxt context t (cc_expr ap1)
	| Array(t, _, d2), Call(mf1, None), Call(mf2, None)
	when (
		(mf1 = id_op__i || mf1 = id_op__j || mf1 = id_op__k)
		&& (mf2 = id_op__i || mf2 = id_op__j || mf2 = id_op__k)
	) ->
			mk_matrix_ctxt context t (cc_expr ap1) (cc_expr ap2)
	| Array(t, d, DimLen 1) as ct, Literal(_, v, _), _ when dim2value d > 0 ->
			let n = dim2value d in
			let i = s2i v in
			if 0 <= i && i <= n - 1 then (
				mk_vector_ctxt context t v
			) else (
				Err.msg(Err.Index(type2id ct, ap1.elbl))
			)
	| Array(t, DimLen 1, d) as ct, _, Literal(_, v, _) when dim2value d > 0 ->
			let n = dim2value d in
			let i = s2i v in
			if 0 <= i && i <= n - 1 then (
				mk_vector_ctxt context t v
			) else (
				Err.msg(Err.Index(type2id ct, ap1.elbl))
			)
	| Array(t, DimLen 1, d), _, Offset _ when dim2value d > 0 ->
			let v = (cc_call context ap2).at_name in
			mk_vector_ctxt context t v
	| Array(t, d, DimLen 1), Offset _, _
	when dim2value d > 0
	-> let v = (cc_call context ap1).at_name in
			mk_vector_ctxt context t v
	| Array(t, d1, d2) as ct, Literal(_, v1, _), Literal(_, v2, _) when dim2value d1 > 0 && dim2value d2 > 0 ->
			let n1 = dim2value d1
			and n2 = dim2value d2 in
			let i1 = s2i v1 and i2 = s2i v2 in
			if 0 <= i1 && i1 <= n1 - 1 then (
				if 0 <= i2 && i2 <= n2 - 1 then (
					mk_matrix_ctxt context t v1 v2
				) else (
					Err.msg(Err.Index(type2id ct, ap2.elbl))
				)
			) else (
				Err.msg(Err.Index(type2id ct, ap1.elbl))
			)
	| Array(t, d1, d2), Offset _, _ when dim2value d1 > 0 && dim2value d2 > 0 ->
			let v1 = (cc_call context ap1).at_name
			and v2 = (cc_call context ap2).at_name in
			mk_matrix_ctxt context t v1 v2
	| Array(t, d1, d2), _, Offset _ when dim2value d1 > 0 && dim2value d2 > 0 ->
			let v1 = (cc_call context ap1).at_name
			and v2 = (cc_call context ap2).at_name in
			mk_matrix_ctxt context t v1 v2
	| Array(t, d1, d2), _, _ when dim2value d1 > 0 && dim2value d2 > 0 ->
			let v1 = (cc_call context ap1).at_name
			and v2 = (cc_call context ap2).at_name in
			mk_matrix_ctxt context t v1 v2
	| Array(t, DimLen 1, _), _, _ ->
			mk_array1_ctxt lbl context t ap2
	| Array(t, _, DimLen 1), _, _ ->
			mk_array1_ctxt lbl context t ap1
	| Array(t, _, _), _, _ ->
			let vtyp = cc_typedef t in
			let tmpvar1 = alloc_tmpvar () in
			let tmpvar2 = alloc_tmpvar () in
			let tmpvar3 = alloc_tmpvar () in
			let tmpptr = tmpvar2ptr (Some tmpvar1) in
			let tmpi1 = tmpvar2int tmpvar2 in
			let tmpi2 = tmpvar2int tmpvar3 in
			let lbl = Ly.lbl2str lbl in
			let name = "({"^tmpptr^"=(pat_Object)"^cc_name context^";"
				^tmpi1^"="^ (cc_expr ap1)^";"
				^tmpi2^"="^ (cc_expr ap2)
				^";check_pointer("^tmpptr^","^lbl
				^");check_indices("^tmpptr^","^tmpi1^","^tmpi2^","
				^lbl^");"^"(("^vtyp^"*)&((pat_array2)"^tmpptr^
				")->a)["^tmpi1^"*((se_Int)((pat_array2)"^
				tmpptr^")->len2)+"^ tmpi2^"];})"
			in
			free_tmpvar tmpvar3;
			free_tmpvar tmpvar2;
			free_tmpvar tmpvar1;
			mk_ctxt name t
	
	| _ -> Err.intern "cc_array2_get"

(* -------------------------------------------------------------------------
declaration for callback fields, always expanded,
>>> assumption, that reactive classes CANNOT be extended <<<
------------------------------------------------------------------------- *)
let cc_callback_signal (typ, _, _) =
	let enc_cid = type2id typ in
	let sel _ _ dcl =
		if is_signal dcl && dcl.origin = enc_cid
		then ( Some dcl )
		else ( None )
	in
	let sff = i_fromsymtab sel enc_cid in
	
	let rec callback_signal dcl =
		let sdc = match dcl.entry with
			| SigDecl s -> s
			| _ -> Err.intern "cc_callback_field" in
		let i, _ = sdecl2initkind sdc in
		match i with
		| None ->
				() (* a not visible signal *)
		| Some i ->
				if is_newexpr i then (
					let name = (if dcl.scope = Class then " sta_" else " sgl_")
						^Ly.c2str dcl.origin
						^"_"^"cb__"^(Ly.mf2str dcl.name) in
					let typ = some i.etyp "cc_callback_field" in
					let typ = cc_type2str (subst_map typ cc.cc_v2t) in
					if debug_level DbgField then (
						let ps = P.primary_ps in
						ps "\n[[FieldExpanded-"; ps name; ps "]]"
					);
					ps "static tat_"; ps typ; ps " "; ps name; ps ";\n"
				) else (
				(* evaluated like ordinary expressions ... *)
				)
	in
	List.iter callback_signal sff

(* -------------------------------------------------------------------------
generate code for statements
------------------------------------------------------------------------- *)
let rec c_code_stmtl tab stmtl =
	List.iter (c_code_stmt tab) stmtl

and c_code_stmt tab stmt =
	match stmt with
	| ExprStmt(_, e) ->
			if (is_if e)
			then ( c_code_if tab e.expr )
			else ( pT tab; ps (cc_expr e); ps ";\n" )
	| Break (_, None) ->
			ps "break;\n"
	| Continue (_, None) ->
			ps "continue;\n"
	| Switch (_, sw) ->
			c_code_switch tab sw
	| While(_, cond, loop) ->
			pT tab; ps "while ("; ps (cc_expr cond); ps ")\n";
			pT tab; ps "{\n";
			c_code_stmtl (tab + 1) loop;
			pT tab; ps "};\n"
	| Return(_, None) ->
			if is_constructor cc.cc_decl then (
				pT tab; ps "return p__this;\n"
			) else if (cc.cc_has_rtn_lbl) then (
				pT tab; ps "goto return_label;\n"
			) else (
				pT tab; ps "return;\n"
			)
	| Return(_, Some e) ->
			let e = cc_expr e in
			if (cc.cc_has_rtn_lbl) then (
				pT tab; ps "__result = "; ps e; ps ";\n";
				pT tab; ps "goto return_label;\n"
			) else (
				pT tab; ps "return "; ps e; ps ";\n"
			)
	| LetStmt(_, lt) ->
			pT tab; ps "{\n";
			pT (tab + 1); ps (cc_field_local lt.letent); ps " = ";
			( match lt.letexpr with
				| None -> cc_initval (type2id lt.letent.p_type)
				| Some e -> ps (cc_expr e)
			); ps ";\n";
			c_code_stmtl (tab + 1) lt.letil;
			pT tab; ps "}\n"
	| AssertStmt(_, al) ->
			List.iter (c_assert tab) al
	| Throw(l, e) ->
			let cid = type2id se.conf_class in
			let thw = int64_2_int (eval_const_expr cid e) in
			pT tab; ps "mkExc("; pi thw; ps ","; ps (Ly.lbl2str l); ps ");\n"
	| DoStmt(_, cond, loop) ->
			pT tab; ps "do\n";
			pT tab; ps "{\n";
			c_code_stmtl (tab + 1) loop;
			ps "\n";
			pT tab; ps "} while ("; ps (cc_expr cond); ps ");\n"
	| ForStmt(_, f) ->
			pT tab; ps "for (\n";
			( match f.forinit with
				| None -> ()
				| Some e -> ps (cc_expr e)
			); ps ";";
			( match f.fortest with
				| None -> ()
				| Some e -> ps (cc_expr e)
			); ps ";";
			( match f.forupd with
				| None -> ()
				| Some e -> ps (cc_expr e)
			);
			ps ")\n";
			pT tab; ps "{\n";
			c_code_stmtl (tab + 1) f.forstmtl;
			pT tab; ps "};\n"
	| Nothing(_) ->
			()
	| s ->
			let sp = Err.cid_lbl2str (type2id cc.cc_typ) (Err.stmt2lbl s) in
			Err.msg (Err.NYI ("code generator not yet implemented for \
						stmt at "^sp))

and is_if =
	function
	| { expr = If(_) } -> true
	| _ -> false

and c_code_if tab =
	function
	| If([Else(_, sl)]) ->
			c_code_stmtl tab sl
	| If(tl) ->
			c_then_iter tab tl
	| _ -> Err.intern "c_code_if"

and c_then_iter tab =
	function
	| [] -> ()
	| [Else(_, sl)] ->
			pT tab; ps "{\n";
			c_code_stmtl (tab + 1) sl;
			pT tab; ps "}\n"
	| Then(_, e, sl):: tl ->
			pT tab; ps "if ("; ps (cc_expr e); ps ")\n";
			pT tab; ps "{\n";
			c_code_stmtl (tab + 1) sl;
			pT tab; ps "}";
			if tl = []
			then ( ps ";\n" )
			else ( ps "\n";
				pT tab; ps "else\n";
				c_then_iter tab tl )
	| Else _:: _ -> Err.intern "c_then_iter"

and c_assert tab =
	function
	| Assertion(expr, b, exc) ->
			pT tab; ps "if ("; if b then () else ps " !(";
			ps (cc_expr expr); if b then () else ps ") ";
			ps ") mkExc(";
			pi (int64_2_int (eval_const_expr (type2id cc.cc_typ) exc));
			ps ","; ps (Ly.lbl2str expr.elbl); ps ");\n"
	| _ -> Err.intern "c_assert"

and c_code_switch tab sw =
	let cc_casev =
		function
		| CaseLiteral c ->
				ps (cc_constant c)
		| CaseId i ->
				ps (cc_expr { etyp = sw.swexpr.etyp; elbl = sw.swexpr.elbl; expr = Call(i, None) }) in
	let cc_case =
		function
		| { sfrom = c1; sto = None; sstmtl = il } ->
				pT (tab + 1); ps "case "; cc_casev c1; ps ":\n";
				List.iter (c_code_stmt (tab + 2)) il
		| _ -> Err.msg (Err.NYI "code generator for case range")
	in
	pT tab; ps "switch ("; ps (cc_expr sw.swexpr); ps ")\n";
	pT tab; ps "{\n";
	List.iter cc_case sw.swcase;
	if sw.swdflt = [] then (
	) else (
		pT (tab + 1); ps "default: ";
		List.iter (c_code_stmt (tab + 2)) sw.swdflt
	);
	pT tab; ps "};\n"

(* ========================================================================= *)
let callback_init ~called_from_decl dcl =
	if gen_for_targetsys () then (
		let sdcl = match dcl.entry with
			| SigDecl s -> s
			| _ -> Err.intern "callback_init" in
		let init, at_decl = sdecl2initkind sdcl in
		if ( called_from_decl && at_decl ) ||
		( not(called_from_decl) && not(at_decl) )
		then (
			match init with
			| None -> () (* signal not visible -> no callback *)
			| Some init ->
					if is_newexpr init then (
						let name = Ly.mf2id ("cb__"^Ly.mf2str dcl.name)
						in
						ps (assgn2exp_flds name init dcl); ps ";\n"
					) else (
					(* expandable field used; is initialized *)
					)
		) else (
		(* this call is not responsible *)
		)
	) else (
	(* only needed for target system generation *)
	)

let bs_init tab scope dcl =
	if (is_staticfinal dcl && is_primitive_typ dcl.signature.rt) ||
	(dcl.scope != scope)
	then (
	(* nothing to do *)
	) else if (is_signal dcl) && (scope = Instance || scope = Single) then (
		pT tab; callback_init ~called_from_decl: true dcl
	) else (
		try
			let init_e = get_initexpr dcl in
			let init_a = { expr = Assign(dcl.name, NoArr, id_op_assign, init_e);
				etyp = Some dcl.signature.rt; elbl = init_e.elbl
			} in
			pT tab;
			if can_expand dcl && not(is_primitive_typ dcl.signature.rt) then (
				ps (assgn2exp_flds dcl.name init_e dcl)
			) else (
				ps (cc_expr init_a)
			);
			ps ";\n"
		with Not_found ->
				()
	)

let cc_static_init (typ, _, _) =
	let cid = type2id typ in
	if is_array_typ typ || (cid = id_string) then (
	(* ok *)
	) else (
		let ast = i_ast cid in
		let v2t = prepare_subst typ in
		set_cc_context typ "&conf_obj" v2t false;
		List.iter (bs_init 1 Class) ast.declseq
	)

