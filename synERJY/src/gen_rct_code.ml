open Ly
open Ast
open Util
open P
open Util_print
open Util_gen
open Util_gen_c
open Gen_application

(* ------------------------------------------------------------------------
print a reactive expression as a comment
------------------------------------------------------------------------- *)
let rexpr2ps_any_cg lhs rhs =
	ps "/* "; ps lhs; ps " = "; print_rctexp rhs; ps " */\n"

(* -------------------------------------------------------------------------
GENERATE CODE for expressions
- version ExprWordSty uses one variable for each mem or wire
- version ExprBitSty uses 1 bit for each mem or wire
------------------------------------------------------------------------- *)
let rec rexpr2str_expr_cg = function
	| S x when x = Sc.beta ->
			rexpr2str_expr_cg (NNot (S Sc.alpha))
	| S x ->
			( match !boolgen with
				| ExprBitSty -> test (Sc.sgn2str x)
				| ExprWordSty -> Sc.sgn2str x
				| JmpBitStyle -> Err.intern "rexpr2str_expr_cg"
			)
	| CC x -> Err.intern "rexpr2str_expr_cg"
	| TT -> "1"
	| FF -> "0"
	| NNot x -> "!("^(rexpr2str_expr_cg x)^")"
	| AAnd el -> "("^(lst_sep2str rexpr2str_expr_cg " && " el)^")"
	| OOr el -> "("^(lst_sep2str rexpr2str_expr_cg " || " el)^")"

(* -------------------------------------------------------------------------
GENERATE CODE for expressions as conditional jumps (Shannon - decomposition)
this style fits well to micro - controller
------------------------------------------------------------------------- *)
let sgn2bitps s =
	let (w, b) = bit_get s in
	ps w; ps ","; pi b

let jmp_label = ref 1

let jmp_nxt () =
	let l = !jmp_label in
	jmp_label := l + 1;
	l

let jmp_nxt () = "j"^(i2s (jmp_nxt ()))

let jmp_reset () = jmp_label := 1

let rec jmp_and next_branch ta fa = function
	| [] -> Err.intern "jmp_and"
	| [e] -> jmp_equ next_branch ta fa e
	| e:: el ->
			let undecided = jmp_nxt () in
			jmp_equ true undecided fa e;
			pT 1; ps "LABEL("; ps undecided; ps ");\n";
			jmp_and next_branch ta fa el

and jmp_or next_branch ta fa = function
	| [] -> Err.intern "jmp_or"
	| [e] -> jmp_equ next_branch ta fa e
	| e:: el ->
			let undecided = jmp_nxt () in
			jmp_equ false ta undecided e;
			pT 1; ps "LABEL("; ps undecided; ps ");\n";
			jmp_or next_branch ta fa el

and jmp_equ next_branch ta fa = function
	| S x when x = Sc.beta ->
			let x = Sc.alpha in
			if next_branch then (
				pT 1; ps "IF_FALSE("; sgn2bitps(Sc.sgn2str x); ps ",";
				ps ta; ps");\n"
			) else (
				pT 1; ps "IF_TRUE("; sgn2bitps(Sc.sgn2str x); ps ",";
				ps fa; ps");\n"
			)
	| S x ->
			if next_branch then (
				pT 1; ps "IF_FALSE("; sgn2bitps(Sc.sgn2str x);
				ps ","; ps fa; ps");\n"
			) else (
				pT 1; ps "IF_TRUE("; sgn2bitps(Sc.sgn2str x);
				ps ","; ps ta; ps");\n"
			)
	| CC x ->
			Err.intern "jmp_equ next_branch"
	| TT ->
			if next_branch
			then ()
			else ( pT 1; ps "GOTO("; ps ta; ps ");\n" )
	| FF ->
			if next_branch
			then ( pT 1; ps "GOTO("; ps fa; ps ");\n" )
			else ()
	| NNot x -> jmp_equ (not next_branch) fa ta x
	| AAnd xl -> jmp_and next_branch ta fa xl
	| OOr xl -> jmp_or next_branch ta fa xl

let jmp_eqn_sgn e action =
	match e with
	| FF -> ()
	| TT -> action ()
	| _ ->
			let ta = jmp_nxt ()
			and fa = jmp_nxt () in
			jmp_equ true ta fa e;
			pT 1; ps "LABEL("; ps ta; ps "); /* set to true */\n";
			action ();
			pT 1; ps "LABEL("; ps fa; ps "); /* stay false */\n"

let mk_rctengine_generator_reset () =
	match !boolgen with
	| ExprBitSty -> bit_reset ()
	| JmpBitStyle -> bit_reset (); jmp_reset ()
	| ExprWordSty -> ()

(* -------------------------------------------------------------------------
DEFINE wires for bool result value for DATA - IFS and for the await - COUNTER
------------------------------------------------------------------------- *)
let rec mk_if_and_counter_h = function
	| ActBool(_, b, _, _) -> alloc_wire b
	| ActTimeCond(_, b, _, _, _) -> alloc_wire b
	| ActBlck(cc) -> List.iter (fun(_, c) -> mk_if_and_counter_h c) cc
	| _ -> ()

let mk_if_and_counter d =
	match d.sc_act with
	| None -> ()
	| Some a -> mk_if_and_counter_h a

(* -------------------------------------------------------------------------
DEFINE the vars, that hold the values for visible SIGNALS of any kind.
Values are:
- < SIGNAME > : bool value for being present / absent
- < SIGNAME > __t : timestamp data
------------------------------------------------------------------------- *)
let mk_ext_sign_decl xs =
	let simul = gen_for_simulation () in
	let s = sig2sgn xs in
	let sn = Sc.sgn2str s in
	let sv = Sc.sgn2valstr s in
	let input = is_input_sig xs in
	if input then (
		alloc_wire s;
		if simul then (
			ps "\nEDEF("; ps sn; ps "__n);"
		)
	) else (
	(* for outputs: vars are declared elsewhere *)
	);
	let typ = sig2valtyp xs in
	if typ = Null then (
	(* pure signal *)
	) else (
		let has_mem = simul &&
			(is_input_sig xs || is_delayed_typ (sig2typ xs)) in
		if typ = t_bool then (
			if has_mem
			then ( ps "\nEDEF("; ps sv; ps "__n);" )
			else ()
		) else (
			let t =
				match typ with
				| Array(t, DimLen 1, DimLen n)
				| Array(t, DimLen n, DimLen 1) ->
						let t = cc_typedef t in
						"\nstruct { ARRAYLEN len;"^t^" a["^i2s n^"]; }"
				| Array(t, DimLen n1, DimLen n2) ->
						let t = cc_typedef t in
						"\nstruct { ARRAYLEN len1;ARRAYLEN len2;"^t
						^" a["^i2s n1^"]["^i2s n2^"]; } "
				| _ -> cc_typedef_with_no_builtin_prefix typ "tat_"
			in
			if has_mem then (ps "\n"; ps t; ps " "; ps sv; ps "__n;");
			let buf_size = dims2value(sig2buf xs) in
			if buf_size < 2 then (
				ps "\n"; ps t; ps " "; ps sv; ps ";"
			) else if buf_size = 2 && not(is_matrix_typ typ) then (
				ps "\n"; ps t; ps " "; ps sv; ps "["; pi buf_size; ps "];"
			) else (
				ps "\nstruct { int pt;int i;struct { ARRAYLEN len;";
				ps t; ps " a"; ps "["; pi buf_size; ps "]; } b; } "; ps sv; ps ";"
			)
		)
	)

let mk_ext_timestamp xs =
	if xs.trs_sdecl.sig_tmstmp then (
		let sname = Sc.sgn2str (sig2sgn xs) in
		ps "\nse_Time "; ps sname; ps "__t = -TIMING;";
	)

(* -------------------------------------------------------------------------
Functions for setting context
------------------------------------------------------------------------- *)
let rec calls2str cl =
	match cl with
	| [] -> ""
	| [Call(mf, None), _] when mf = id_conf ->
			"conf_obj"
	| [Call(mf, None), _] ->
			Ly.mf2str mf
	| (Call(mf, None), _):: (Call(mf', None), typ):: cl' ->
			if mf' = id_conf then (
				"sta_"^Ly.c2str (type2id typ)^"_"^Ly.mf2str mf
			) else (
				let cid = type2id typ in
				match (i_ast cid).status with
				| Singleton _ ->
						"sgl_"^Ly.c2str cid^"_"^Ly.mf2str mf
				| _ ->
						calls2str ((Call(mf', None), typ):: cl')^"."^Ly.mf2str mf
			)
	| _ -> assert false

let cc_obj2str ro =
	calls2str (List.rev ro.trs_name)

let rec this_in_CC_context_h sname =
	match sname with
	| [] ->
			assert false
	| [Call(mf, None), typ] when mf = id_conf ->
			let ctyp = cc_type2str typ in
			( match (i_ast (type2id typ)).status with
				| Singleton _ -> "conf_obj", true
				| _ -> "((pat_"^ctyp^")&(conf_obj))", false
			)
	| [Call(mf, None), _] ->
			assert false
	| (Call(mf, None), typ):: tl ->
			( try
				let ro' =
					List.find
						( fun ro -> ro.trs_name = List.rev tl
						) ac.ac_rctobj_tree
				in
				let dcl = get_decl_after_tc (type2id ro'.trs_type) mf (- 1) in
				let prf = if dcl.scope = Class then "sta_" else "sgl_" in
				if is_static dcl then (
					if can_expand dcl
					then prf^c2str dcl.origin^"_"^mf2str mf, true
					else "&"^prf^c2str dcl.origin^"_"^mf2str mf, false
				) else (
					let nm, exp = this_in_CC_context_h tl in
					if exp then (
						let ctyp = cc_type2str typ in
						"((pat_"^ctyp^")&("^nm^"."^mf2str mf^"))", false
					) else (
						let ctyp = cc_type2str typ in
						"(((pat_"^ctyp^")&("^nm^")->"^mf2str mf^"))", false
					)
				)
			with _ -> assert false
			)
	| _ -> assert false

let this_in_CC_context ro =
	let rv = List.rev ro.trs_name in
	this_in_CC_context_h rv

let gen_exp_in_CC_context ro xe =
	let rotyp = ro.trs_type in
	let this, exp = this_in_CC_context ro in
	set_cc_context rotyp this (prepare_subst rotyp) exp;
	cc_call cc.cc_this_ctxt xe

type cc_dim_t = Dim_i | Dim_j | Dim_ij | Dim_ji | Dim_ik | Dim_kj

let cc_mk_call ind =
	cc_mk_e t_int (Call(ind, None))

let gen_exp_in_array_context ro xe index =
	let rotyp = ro.trs_type in
	let this, exp = this_in_CC_context ro in
	set_cc_context rotyp this (prepare_subst rotyp) exp;
	let xe = match index with
		| Dim_i -> cc_call_add_get [0] [0, cc_mk_call id_op__i] xe
		| Dim_j -> cc_call_add_get [0] [0, cc_mk_call id_op__j] xe
		| Dim_ij ->
				cc_call_add_get [0; 1] [0, cc_mk_call id_op__i;
					1, cc_mk_call id_op__j] xe
		| Dim_ji ->
				cc_call_add_get [0; 1] [0, cc_mk_call id_op__j;
					1, cc_mk_call id_op__i] xe
		| Dim_ik ->
				cc_call_add_get [0; 1] [0, cc_mk_call id_op__i;
					1, cc_mk_call id_op__k] xe
		| Dim_kj ->
				cc_call_add_get [0; 1] [0, cc_mk_call id_op__k;
					1, cc_mk_call id_op__j] xe
	in
	cc_call cc.cc_this_ctxt xe

(* -------------------------------------------------------------------------
DEFINE the vars, that hold the values for MEMORIES.
Values are:
- < SIGVALUE > : actual value
- < SIGNAME > __n : value for the next instant
------------------------------------------------------------------------- *)
let mk_int_var_decl s t =
	match t with
	| Null -> ()  (* pure signals are declared as wires *)
	| t when t = t_bool  (* boolean signals are declared as wires *) ->
			if Sc.is_mem s then alloc_mem s else
			if Sc.is_pre s then alloc_mem s else
			if Sc.is_aux s then alloc_wire s else
			if Sc.is_var s then (
				ps "\n"; ps (cc_typedef t); ps " "; print_sgn s; ps ";"
			)
	| Array(typ, DimLen 1, DimLen n)
	| Array(typ, DimLen n, DimLen 1) ->
			let sv =
				if Sc.is_lcl s
				then Sc.sgn2valstr s
				else Sc.sgn2str s in
			let typedef =
				try cc_builtintypedef (type2id typ)
				with Not_found -> "se_Object"
			in
			ps "\nstruct { ARRAYLEN len;"; ps typedef;
			ps " a["; ps (i2s n); ps "]; } "; ps sv;
			if Sc.is_mem s then (
				ps ","; ps sv; ps "__n"
			); ps ";"
	| Array(typ, DimLen n1, DimLen n2) ->
			let sv =
				if Sc.is_lcl s
				then Sc.sgn2valstr s
				else Sc.sgn2str s in
			let typedef =
				try cc_builtintypedef (type2id typ)
				with Not_found -> "se_Object"
			in
			ps "\nstruct { ARRAYLEN len1;ARRAYLEN len2;";
			ps typedef; ps " a["; pi n1; ps "]["; pi n2; ps "]; } ";
			ps sv; ps ","; ps sv; ps "__n;"
	| t ->
			if Sc.is_mem s || Sc.is_pre s then (
				let sn = Sc.sgn2str s in
				ps "\n"; ps (cc_typedef_with_no_builtin_prefix t "tat_");
				ps " "; ps sn; ps ","; ps sn; ps "__n;"
			) else if Sc.is_aux s then (
				let sn = Sc.sgn2str s in
				ps "\n"; ps (cc_typedef t); ps " ";
				ps sn; ps ";";
			) else if Sc.is_lcl s then (
				let sv = Sc.sgn2valstr s in
				ps "\n"; ps (cc_typedef t); ps " ";
				ps sv; ps ";"
			) else if Sc.is_var s then (
				ps "\n"; ps (cc_typedef t); ps " "; print_sgn s; ps ";"
			) else (
				Err.intern "mk_int_decl"
			)

let mk_int_decl =
	function
	| LclFld (s, x) -> mk_int_var_decl s x.p_type
	| LclSig xs -> mk_ext_sign_decl xs
	| IntSig (s, t, n) -> mk_int_var_decl s t

let mk_init_val tab sv typ =
	match typ with
	| Null	-> ()
	| t when t = t_bool	-> ()
	| t when is_primitive_typ t	->
			let cid = type2id t in
			pT tab; ps sv; ps " = "; cc_initval cid; ps ";\n";
	| Array(t, DimLen 1, DimLen n)
	| Array(t, DimLen n, DimLen 1) ->
			pT tab; ps sv; ps ".len = "; pi n; ps ";\n";
			let cid = type2id t in
			pT tab; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
			pT tab; ps "{\n";
			pT (tab + 1); ps sv; ps ".a[__i] = "; cc_initval cid; ps ";\n";
			pT tab; ps "};\n"
	| Array(t, DimLen n1, DimLen n2) ->
			pT tab; ps sv; ps ".len1 = "; pi n1; ps ";\n";
			pT tab; ps sv; ps ".len2 = "; pi n2; ps ";\n";
			let cid = type2id t in
			pT tab; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
			pT tab; ps "{\n";
			pT (tab + 1); ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
			pT (tab + 1); ps "{\n";
			pT (tab + 2); ps "  "; ps sv; ps ".a[__i][__j] = "; cc_initval cid; ps ";\n";
			pT (tab + 1); ps "};";
			pT tab; ps "};\n"
	| _ -> ()

let mk_init_buffer tab sv typ buf_size =
	if buf_size < 2 then (
		mk_init_val tab sv typ
	) else if buf_size = 2 && not(is_matrix_typ typ) then (
		(* ps "for (__i="; pi (buf_size - 1); ps ";__i>=0;__i--) {\n";
		ps sv; ps "[__i] = "; cc_initval cid; ps ";\n";
		ps "};\n" *)
		mk_init_val tab (sv^"[0]") typ;
		mk_init_val tab (sv^"[1]") typ
	) else (
		(* one run index needed *)
		pT tab; ps sv; ps ".pt = "; pi (buf_size - 1); ps ";\n";
		pT tab; ps sv; ps ".b.len = "; pi buf_size; ps ";\n";
		pT tab; ps "for (__k="; pi (buf_size - 1); ps ";__k>=0;__k--)\n";
		pT tab; ps "{\n";
		mk_init_val (tab + 1) (sv^".b.a[__k]") typ;
		pT tab; ps "};\n"
	)

let mk_sig_init n xs =
	let s = sig2sgn xs in
	let typ = sig2valtyp xs
	and sv = Sc.sgn2valstr s in
	let buf_size = dims2value(sig2buf xs) in
	mk_init_buffer n sv typ buf_size

let mk_init_int_sig n = function
	| LclFld (s, x) ->
			mk_init_val n (Sc.sgn2str s) x.p_type
	| LclSig xs ->
			mk_sig_init n xs
	| IntSig(sv, typ, buf_size) ->
			let sv = Sc.sgn2str sv in
			mk_init_buffer n sv typ buf_size

(* -------------------------------------------------------------------------
Handle emit of an output signal for SIMULATION systems.
call OUTPUT FUNCTION (supplied by the programmer) for TARGET systems.
------------------------------------------------------------------------- *)
let rec mk_fmt_parm_group cid mem f =
	let typ = (get_decl_after_tc cid f (- 1)).signature.rt in
	mk_fmt_parm true (mem^(mf2str f)) typ

and mk_fmt_parm in_recursion mem typ =
	let cid = type2id typ in
	if cid = id_bool then (
		"("^mem^" ? \"true\" : \"false\")"
	) else if cid = id_time then (
		"time2str("^mem^")"
	) else (
		try
			let (typ, _, _, _, _) = builtin_info cid in
			"("^typ^") "^mem
		with Not_found ->
				if in_recursion
				then Err.intern "mk_fmt_parm"
				else ();
				let flds = get_fields typ ac.ac_classes in
				lst_sep2str (mk_fmt_parm_group cid (mem^".")) "," flds
	)

let mk_fmt_parm sn typ =
	let parm = mk_fmt_parm false sn typ in
	if parm = ""
	then ""
	else (","^parm)

let rec mk_fmt_item typ =
	let cid = type2id typ in
	if cid = id_bool || cid = id_time
	then "%s"
	else (
		try
			let (_, _, _, fmt, _) = builtin_info cid in fmt
		with Not_found ->
				mk_fmt_item_group typ
	)

and mk_fmt_item_group typ =
	lst_sep2str ( fun f -> let d = get_decl_after_tc (type2id typ) f (- 1) in
					mf2str d.name^"("^mk_fmt_item d.signature.rt^")"
		) "," (get_fields typ ac.ac_classes)

let write_to_stdout xs =
	let name = sig2str xs
	and s = sig2sgn xs in
	let typ = sig2valtyp xs in
	let sv = Sc.sgn2valstr s in
	let buf_size = dims2value(sig2buf xs) in
	let sv =
		if buf_size < 2
		then sv
		else if buf_size = 2 && not(is_matrix_typ typ) then sv^"[0]"
		else
			sv^".b.a["^sv^".pt]"
	in
	match typ with
	| Null ->
			pT 2; ps "fprintf (stdout,\"-"; ps name; ps " ("; ps ")\\n\");\n"
	| Simple _ ->
			let sv = if typ = t_bool then test sv else sv in
			let fmt = mk_fmt_item typ
			and apl = mk_fmt_parm sv typ in
			pT 2; ps "fprintf (stdout,\"-"; ps name; ps " (";
			ps fmt; ps ")\\n\""; ps apl; ps ");\n"
	| Array(t, DimLen 1, DimLen n)
	| Array(t, DimLen n, DimLen 1) ->
			let sv = if typ = t_bool then test sv else sv in
			let fmt = mk_fmt_item t
			and apl = mk_fmt_parm sv t in
			pT 2; ps "fprintf (stdout,\"-"; ps name; ps " \");\n";
			if n > 0 then (
				pT 2; ps "fprintf (stdout,\"({\");\n";
				pT 2; ps "fprintf (stdout,\""; ps fmt; ps "\"";
				ps apl; ps ".a[0]"; ps ");\n";
				pT 2; ps "for (__i = 1; __i < "; pi n; ps "; __i++)\n";
				pT 2; ps "{\n";
				pT 3; ps "fprintf (stdout,\",\");\n";
				pT 3; ps "fprintf (stdout,\""; ps fmt; ps "\"";
				ps apl; ps ".a[__i]"; ps ");\n";
				pT 2; ps "};\n";
				
				pT 2; ps "fprintf (stdout,\"})\\n\");\n";
			) else (
				let ro = some xs.trs_ro " write_to_stdout:1" in
				let cid = type2id ro.trs_type in
				Err.msg(Err.SigOutArray(name, cid, xs.trs_sdecl.sig_pos))
			)
	| Array(t, DimLen n1, DimLen n2) ->
			let sv = if typ = t_bool then test sv else sv in
			let fmt = mk_fmt_item t
			and apl = mk_fmt_parm sv t in
			if n1 > 0 && n2 > 0 then (
				pT 2; ps "fprintf (stdout,\"-"; ps name; ps " \");\n";
				pT 2; ps "fprintf (stdout,\"({\");\n";
				pT 2; ps "fprintf (stdout,\"{\");\n";
				pT 2; ps "fprintf (stdout,\""; ps fmt; ps "\"";
				ps apl; ps ".a[0][0]"; ps ");\n";
				pT 2; ps "for (__j = 1; __j < "; pi n2; ps "; __j++)\n";
				pT 2; ps "{\n";
				pT 3; ps "fprintf (stdout,\","; ps fmt; ps "\"";
				ps apl; ps ".a[0][__j]"; ps ");\n";
				pT 2; ps "};\n";
				pT 2; ps "fprintf (stdout,\"}\");\n";
				pT 2; ps "for (__i = 1; __i < "; pi n1; ps "; __i++)\n";
				pT 2; ps "{\n";
				pT 3; ps "fprintf (stdout,\",{\");\n";
				pT 3; ps "fprintf (stdout,\""; ps fmt; ps "\"";
				ps apl; ps ".a[__i][0]"; ps ");\n";
				pT 3; ps "for (__j = 1; __j < "; pi n2; ps "; __j++)\n";
				pT 3; ps "{\n";
				pT 4; ps "fprintf (stdout,\","; ps fmt; ps "\"";
				ps apl; ps ".a[__i][__j]"; ps ");\n";
				pT 3; ps "};\n";
				pT 3; ps "fprintf (stdout,\"}\");\n";
				pT 2; ps "};\n";
				
				pT 2; ps "fprintf (stdout,\"})\\n\");\n";
			) else (
				let ro = some xs.trs_ro " write_to_stdout:2" in
				let cid = type2id ro.trs_type in
				Err.msg(Err.SigOutArray(name, cid, xs.trs_sdecl.sig_pos))
			)
	| Array(_, DimLen 1, _)
	| Array(_, _, DimLen 1) ->
			( match xs.trs_sdecl.sig_init with
				| None -> ()
				| Some e ->
						( match e.expr with
							| (New(_,[_]))
							-> let ro = some xs.trs_ro " write_to_stdout:3" in
									let cid = type2id ro.trs_type in
									Err.msg(Err.SigOut(name, cid, xs.trs_sdecl.sig_pos))
							| _ -> ()
						)
			)
	| Array(_, _, _) ->
			( match xs.trs_sdecl.sig_init with
				| None -> ()
				| Some e ->
						( match e.expr with
							| (New(_,[_])) ->
									let ro = some xs.trs_ro " write_to_stdout" in
									let cid = type2id ro.trs_type in
									Err.msg(Err.SigOut(name, cid, xs.trs_sdecl.sig_pos))
							| _ -> ()
						)
			)
	| t ->
			let fmt = mk_fmt_item typ
			and apl = mk_fmt_parm sv typ in
			pT 2; ps "fprintf (stdout,\"-"; ps name; ps " (";
			ps fmt; ps ")\\n\""; ps apl; ps ");\n"

let principal2callback xs =
	let ro = some xs.trs_ro "principal2callback" in
	let e, _ = sdecl2initkind xs.trs_sdecl in
	match e with
	| None -> Err.intern "principal2callback"
	| Some xe -> gen_exp_in_CC_context ro xe, xe

let mk_output_callback_target xs =
	let s = sig2sgn xs in
	let cbe, xe = principal2callback xs in
	let typ = sig2valtyp xs in
	let lbl = xs.trs_sdecl.sig_pos in
	let cid = type2id (some xe.etyp "mk_output_callback_target") in
	let call =
		if typ = Null then (
			let dcl = get_decl_after_tc cid id_op_put_val 0 in
			if dcl.scope = Class then (
				"f_"^(Ly.c2str dcl.origin)^"_put_val_0()"
			) else (
				let pv = "_put_val_0((pat_" in
				let suf imp this = pv^imp^")("^this^"))" in
				dynamic_bind lbl cbe id_op_put_val 0 suf
			)
		) else (
			let is_prim = is_primitive_typ typ in
			let is_mat = is_matrix_typ typ in
			let sv = Sc.sgn2valstr s in
			let dcl = get_decl_after_tc cid id_op_put_val 1 in
			let sv = if typ = t_bool then test sv else
				if is_prim && not(typ = t_string) then sv else
					("&"^sv) in
			let buf_size = dims2value(sig2buf xs) in
			let sv =
				if buf_size < 2 then sv else
				if buf_size = 2 && not is_mat then sv^"[0]" else
					sv^".b.a["^sv^".pt]" in
			if dcl.scope = Class then (
				"f_"^(Ly.c2str dcl.origin)^"_put_val_1("^sv^")"
			) else (
				let pv = "_put_val_1((pat_" in
				let suf imp this = pv^imp^")("^this^"),"^sv^")" in
				dynamic_bind lbl cbe id_op_put_val 1 suf
			)
		)
	in
	pT 2; ps call; ps ";\n"

let mk_output_callback xs =
	if is_input_sig xs then (
	(* nothing to do *)
	) else (
		let s = sig2sgn xs in
		let c =
			match !boolgen with
			| JmpBitStyle -> test (Sc.sgn2str s)
			| ExprBitSty -> test (Sc.sgn2str s)
			| ExprWordSty -> Sc.sgn2str s in
		if se.target_sys = Simulation then (
			pT 1; ps "if ("; ps c; ps ")\n  {\n";
			write_to_stdout xs;
			pT 1; ps "};\n"
		) else if gen_for_targetsys () then (
			if (is_output_sig xs) then (
				pT 1; ps "if ("; ps c; ps ")\n";
				pT 1; ps "{\n";
				mk_output_callback_target xs;
				pT 1; ps "};\n"
			) else (
			(* signal not visible for the environment *)
			)
		);
	)

(* -------------------------------------------------------------------------
GENERATE code for an DATA ACTION if the action - trigger ''lhs'' is true:
------------------------------------------------------------------------- *)
let mk_valcp_data tab typ is_reference ~lhs ~rhs =
	if typ = Null then (
		Err.intern "mk_valcp_data"
	) else if is_primitive_typ typ then (
		pT tab; ps lhs; ps " = "; ps rhs
	) else if is_array_typ typ then (
		match typ with
		| Array(t, DimLen 1, DimLen n)
		| Array(t, DimLen n, DimLen 1) ->
				pT tab; ps "ARRAYC1(&("; ps lhs; ps "),";
				if is_reference then (
				(* address was given *)
				) else (
					ps "&" (* record name was given *)
				);
				ps "("; ps rhs; ps "),"; ps (i2s n);
				ps ",sizeof("; ps (cc_typedef t); ps "))"
		| Array(t, DimLen n1, DimLen n2) ->
				pT tab; ps "ARRAYC2(&("; ps lhs; ps "),";
				if is_reference then (
				(* address was given *)
				) else (
					ps "&" (* record name was given *)
				);
				ps "("; ps rhs; ps "),"; ps (i2s n1); ps ",";
				ps (i2s n2); ps ",sizeof("; ps (cc_typedef t); ps "))"
		| _ -> Err.intern "mk_valcp_data"
	) else (
		pT tab; ps "memcpy((char *)&("; ps lhs; ps "),(char *)";
		if is_reference then (
		(* address was given *)
		) else (
			ps "&" (* record name was given *)
		);
		ps "("; ps rhs; ps "),sizeof(tat_"; psc (type2id typ); ps "))"
	);
	ps ";\n"

let mk_valcp tab typ is_reference ~lhs ~rhs =
	if typ = Null then (
		Err.intern "mk_valcp"
	) else if typ = t_bool then (
		copy_input_boolval tab ~lhs: lhs ~rhs: rhs
	) else (
		mk_valcp_data tab typ is_reference lhs rhs
	)

let mk_buffer_offset typ sv buf_size offset =
	if buf_size < 2 then sv else
	if buf_size = 2 && not(is_matrix_typ typ) then sv^"["^i2s offset^"]" else
		sv^ ".b.a[(__b="^sv^".pt+"^i2s offset
		^","^"__b>"^i2s (buf_size - 1)^"?__b-"^i2s buf_size^":__b)]"

let rec mk_action tab act =
	let some t = some t "mk_action" in
	( match act with
		| ActStmt(_, e, ro) ->
				let e = (gen_exp_in_CC_context ro e).at_name in
				pT tab; ps e; ps ";\n"
		| ActProd(_, buf, e1, e2, cc, ro) ->
				mk_scalar_product tab buf e1 e2 cc ro
		| ActSigInit(_, _, xs, ro) ->
				mk_sig_init tab xs
		| ActInit(_, sn, t, ro) ->
				mk_init_val tab (Sc.sgn2str sn) t
		| ActBlck(cc) -> mk_blck tab cc
		| ActEmit(_, _, _, _, Rct _, cc, _, ro) ->
				mk_blck tab cc
		| ActEmit(_, _, _, dl, Val e, cc, xs, ro) ->
				let sv = Sc.sgn2valstr (sig2sgn xs) in
				let sv = if is_delayed_typ (sig2typ xs) then sv^"__n" else sv in
				let typ = sig2valtyp xs in
				let buf_size = dims2value(sig2buf xs) in
				let sv =
					if buf_size < 2 then sv
					else if buf_size = 2 && not(is_matrix_typ typ) then sv^"[0]"
					else sv^".b.a["^sv^".pt]" in
				( match dl, typ with
					| [], Null ->
							mk_blck tab cc (* nothing to do *)
					| _, Null ->
							Err.intern "mk_call:ActEmit:1"
					(* vector asssigned to a vector signal *)
					| [], Array _ ->
							mk_emit_array tab typ sv e cc xs ro
					| [], _ ->
							mk_blck tab cc;
							let rhs = gen_exp_in_CC_context ro e in
							mk_valcp tab typ (not rhs.at_exp)
								~lhs: sv
								~rhs: rhs.at_name
					(* vector asssigned to a slice of a vector signal *)
					| [_], Array _
					| [_; _], Array _ ->
							mk_emit_array_slice tab dl typ sv e cc xs ro
					| _ ->
							Err.intern "mk_call:Emit:no_case"
				);
		| ActSigPres(_)
		| ActSigVal(_)
		| ActSigIn(_)
		| ActSigOut(_) ->
				()
		| ActPrm(_, s, e, _, ro) ->
				let sn = Sc.sgn2str s in
				let typ = some e.etyp in
				let rhs = gen_exp_in_CC_context ro e in
				mk_valcp_data tab typ (not rhs.at_exp)
					~lhs: sn
					~rhs: rhs.at_name
		
		| ActAssgn(_, s, e, cc, ro) ->
				let sn = Sc.sgn2str s in
				let typ = some e.etyp in
				let tgt = if Sc.is_mem s then "__n" else
					if Sc.is_lcl s
					or Sc.is_aux s
					or Sc.is_var s
					or Sc.is_pre s then "" else
						Err.intern "mk_action: ActAssgn"
				in
				( match typ with
					| Array _ ->
							mk_assgn_array tab typ (sn^tgt) e cc ro
					| _ ->
							mk_blck tab cc;
							let rhs = gen_exp_in_CC_context ro e in
							mk_valcp tab typ (not rhs.at_exp)
								~lhs: (sn^tgt)
								~rhs: rhs.at_name
				)
		| ActAssgnInBlck(_, s, e, ro) ->
				let sn = Sc.sgn2str s in
				let typ = some e.etyp in
				let tgt = if Sc.is_mem s then "__n" else
					if Sc.is_lcl s
					or Sc.is_aux s
					or Sc.is_var s
					or Sc.is_pre s then "" else
						Err.intern "mk_action: ActInt"
				in
				( match typ with
					| Array _ ->
							mk_assgn_array_in_blck tab typ (sn^tgt) e ro
					| _ ->
							let rhs = gen_exp_in_CC_context ro e in
							mk_valcp tab typ (not rhs.at_exp)
								~lhs: (sn^tgt)
								~rhs: rhs.at_name;
				)
		| ActBool (_, result, e, ro) ->
				let e = (gen_exp_in_CC_context ro e).at_name in
				set_from_expr tab ~lhs: (Sc.sgn2str result) ~rhs: e
		| ActTimeInit(_, s, ro) ->
				cc.cc_has_timing <- true;
				pT tab; print_sgn s; ps " = (se_Time)0;\n";
		| ActTimeAdd(_, s, ro) ->
				cc.cc_has_timing <- true;
				pT tab; print_sgn s; ps " = "; print_sgn s; ps "+TIMING;\n";
				let ie = get_sysprop se.conf_class id_exc_timestamp in
				pT tab; ps "if ("; print_sgn s; ps "<0) mkExc("; pi ie; ps ",0);\n"
		| ActTimeCond(_, result, s, e, ro) ->
				cc.cc_has_timing <- true;
				let e = (gen_exp_in_CC_context ro e).at_name in
				let e = "("^Sc.sgn2str s^" >= "^e^")" in
				set_from_expr tab ~lhs: (Sc.sgn2str result) ~rhs: e
		| ActDbg (l, ro) ->
				pT tab; ps "fprintf(stdout,\"at: "; ps (obj2str ro);
				ps " , "; lst_sep2ps (fun l -> ps (int_lbl2str l)) ":" l;
				ps "\\n\");\n"
	)

and mk_blck tab cc =
	List.iter
		( fun (e, cll) ->
					match !boolgen with
					| JmpBitStyle ->
							jmp_eqn_sgn e (fun _ -> mk_action tab cll)
					| ExprWordSty
					| ExprBitSty ->
							let c = rexpr2str_expr_cg e in
							if c = "0" then (
							(* nothing to do *)
							) else if c = "1" then (
								mk_action tab cll
							) else (
								pT tab; ps "if ("; ps c; ps ")\n";
								pT tab; ps "{\n";
								mk_action (tab + 1) cll;
								pT tab; ps "};\n"
							)
		) cc

and mk_scalar_product tab buf e1 e2 cc ro =
	mk_blck tab cc;
	let buf = Sc.sgn2str buf in
	let t1 = some e1.etyp "mk_scalar_product:1" in
	let t2 = some e2.etyp "mk_scalar_product:2" in
	( match t1, t2 with
		| Array(t', DimLen 1, DimLen n1), Array(_, DimLen n2, DimLen 1)
		| Array(t', DimLen 1, DimLen n1), Array(_, DimLen 1, DimLen n2)	->
				if n1 <> n2 then Err.intern "mk_action:Vector";
				let e1' = (gen_exp_in_array_context ro e1 Dim_i).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_i).at_name in
				pT tab; ps buf; ps "="; cc_initval (type2id t'); ps ";\n";
				pT tab; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps buf; ps "+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT tab; ps "};\n";
		| Array(t', DimLen 1, DimLen n1), Simple _
		| Array(t', DimLen n1, DimLen 1), Simple _
		| Simple _, Array(t', DimLen 1, DimLen n1)
		| Simple _, Array(t', DimLen n1, DimLen 1) ->
				let e1' = (gen_exp_in_array_context ro e1 Dim_i).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_i).at_name in
				pT tab; ps buf; ps "="; cc_initval (type2id t'); ps ";\n";
				pT tab; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps buf; ps "+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT tab; ps "};\n";
		| Array(t', DimLen n11, DimLen n12), Array(_, DimLen n1, DimLen 1) ->
				if n12 <> n1 then Err.intern "mk_action:Matrix.2";
				let e1' = (gen_exp_in_array_context ro e1 Dim_ij).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_j).at_name in
				pT tab; ps "for (__i="; pi (n11 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps buf; ps ".a[__i]="; cc_initval (type2id t'); ps ";\n";
				pT (tab + 1); ps "for (__j="; pi (n12 - 1); ps ";__j>=0;__j--)\n";
				pT (tab + 1); ps "{\n";
				pT (tab + 2); ps buf; ps ".a[__i]+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT (tab + 1); ps "};\n";
				pT tab; ps "};\n";
		| Array(_, DimLen 1, DimLen n1), Array(t', DimLen n21, DimLen n22) ->
				if n1 <> n21 then Err.intern "mk_action:Matrix.3";
				let e1' = (gen_exp_in_array_context ro e1 Dim_j).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_ji).at_name in
				pT tab; ps "for (__i="; pi (n22 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps buf; ps ".a[__i]="; cc_initval (type2id t'); ps ";\n";
				pT (tab + 1); ps "for (__j="; pi (n1 - 1); ps ";__j>=0;__j--)\n";
				pT (tab + 1); ps "{\n";
				pT (tab + 2); ps buf; ps ".a[__i]+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT (tab + 1); ps "};\n";
				pT tab; ps "};\n"
		| Array(t', DimLen n11, DimLen 1), Array(_, DimLen 1, DimLen n22) ->
				let e1' = (gen_exp_in_array_context ro e1 Dim_ik).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_kj).at_name in
				pT tab; ps "for (__i="; pi (n11 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps "for (__j="; pi (n22 - 1); ps ";__j>=0;__j--)\n";
				pT (tab + 1); ps "{\n";
				pT (tab + 2); ps buf; ps ".a[__i][__j]="; cc_initval (type2id t'); ps";\n";
				pT (tab + 2); ps buf; ps ".a[__i][__j]+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT (tab + 1); ps "};\n";
				pT tab; ps "};\n"
		| Array(t', DimLen n11, DimLen n12), Array(_, DimLen n21, DimLen n22) ->
				if n12 <> n21 then Err.intern "mk_action:Matrix.1";
				let e1' = (gen_exp_in_array_context ro e1 Dim_ik).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_kj).at_name in
				pT tab; ps "for (__i="; pi (n11 - 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps "for (__j="; pi (n22 - 1); ps ";__j>=0;__j--)\n";
				pT (tab + 1); ps "{\n";
				pT (tab + 2); ps buf; ps ".a[__i][__j]="; cc_initval (type2id t'); ps";\n";
				pT (tab + 2); ps "for (__k="; pi (n12 - 1); ps ";__k>=0;__k--)\n";
				pT (tab + 2); ps "{\n";
				pT (tab + 3); ps buf; ps ".a[__i][__j]+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT (tab + 2); ps "};\n";
				pT (tab + 1); ps "};\n";
				pT tab; ps "};\n"
		| Array(t', DimLen n1, DimLen n2), Simple _
		| Simple _, Array(t', DimLen n1, DimLen n2) ->
				let e1' = (gen_exp_in_array_context ro e1 Dim_ij).at_name in
				let e2' = (gen_exp_in_array_context ro e2 Dim_ij).at_name in
				pT tab; ps buf; ps ".a[__i]="; cc_initval (type2id t'); ps ";\n";
				pT tab; ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps buf; ps ".a[__i]+="; ps e1'; ps "*"; ps e2'; ps ";\n";
				pT tab; ps "};\n";
		| _ -> Err.intern ("mk_action:ActScal: "^
						Err.type2str t1^"  "^Err.type2str t2)
	)

and mk_emit_array tab typ sv e cc xs ro =
	match typ with
	| Array(t, DimLen 1, DimLen n)
	| Array(t, DimLen n, DimLen 1) ->
			( match e.expr with
				| Value(sv') when cc = [] ->
						let sv' = Sc.sgn2str sv' in
						pT tab; ps "ARRAYC1(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n); ps ",sizeof("; ps (cc_typedef t); ps "));\n"
				| SigVal(sv', xs, offset) when cc = [] ->
						let buf_size = dims2value(sig2buf xs) in
						let sv' = Sc.sgn2str sv' in
						let sv' = mk_buffer_offset t sv' buf_size offset in
						pT tab; ps "ARRAYC1(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n); ps ",sizeof("; ps (cc_typedef t); ps "));\n"
				| _ ->
						pT tab; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
						pT tab; ps "{\n";
						mk_blck n cc;
						pT tab; ps sv; ps ".a[__i] = ";
						ps (gen_exp_in_array_context ro e Dim_i).at_name; ps ";\n";
						pT tab; ps "};\n"
			)
	(* matrix asssigned to a matrix signal *)
	| Array(t, DimLen n1, DimLen n2) ->
			( match e.expr with
				| Value(sv') ->
						let sv' = Sc.sgn2str sv' in
						pT tab; ps "ARRAYC2(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n1); ps ","; ps (i2s n2); ps ",sizeof(";
						ps (cc_typedef t); ps "));\n"
				| SigVal(sv', xs, offset) when cc = [] ->
						let buf_size = dims2value(sig2buf xs) in
						let sv' = Sc.sgn2str sv' in
						let sv' = mk_buffer_offset t sv' buf_size offset in
						pT tab; ps "ARRAYC2(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n1); ps ","; ps (i2s n2); ps ",sizeof(";
						ps (cc_typedef t); ps "));\n"
				| _ ->
						pT tab; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
						pT tab; ps "{\n";
						pT (tab + 1); ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
						pT (tab + 1); ps "{\n";
						mk_blck (tab + 2) cc;
						pT (tab + 2); ps sv; ps ".a[__i][__j] = ";
						ps (gen_exp_in_array_context ro e Dim_ij).at_name; ps ";\n";
						pT (tab + 1); ps "}; \n";
						pT tab; ps "   };\n"
			)
	| _ -> Err.intern ("mk_emit_array: "^Err.type2str typ)

and mk_emit_array_slice tab dl typ sv e cc xs ro =
	match dl, typ with
	| [d], Array(t, DimLen 1, DimLen n)
	| [d], Array(t, DimLen n, DimLen 1) ->
			let l, u = mk_index (React2sya.ro2cid ro) n d in
			if l = u then (
				pT tab; ps sv; ps ".a["; pi l; ps "] = ";
				ps (gen_exp_in_CC_context ro e).at_name; ps ";\n"
			) else (
				pT tab; ps "for (__i="; pi (u - l + 1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				mk_blck (tab + 1) cc;
				pT (tab + 1); ps sv; ps ".a[__i+"; pi l; ps "] = ";
				ps (gen_exp_in_array_context ro e Dim_i).at_name; ps ";\n";
				pT tab; ps "};\n";
			)
	(* matrix asssigned to a slice of a matrix signal *)
	| [d1; d2], Array(t, DimLen n1, DimLen n2) ->
			let l1, u1 = mk_index (React2sya.ro2cid ro) n1 d1
			and l2, u2 = mk_index (React2sya.ro2cid ro) n2 d2 in
			if l1 = u1 && l2 = u2 then (
				pT tab; ps ".a["; pi l2; ps "]["; pi l1; ps "] = ";
				ps (gen_exp_in_CC_context ro e).at_name; ps ";\n"
			) else if l1 = u1 then (
				pT tab; ps "for (__j="; pi (u2 - l2); ps ";__j>=0;__j--)\n";
				pT tab; ps "{\n";
				mk_blck (tab + 1) cc;
				pT (tab + 1); ps sv; ps ".a["; pi (l1); ps "][__j+"; pi l2; ps "] = ";
				ps (gen_exp_in_array_context ro e Dim_j).at_name; ps ";\n";
				pT tab; ps "};;\n";
			) else if l2 = u2 then (
				pT tab; ps "for (__i="; pi (u1 - l1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				mk_blck (tab + 1) cc;
				pT (tab + 1); ps sv; ps ".a[__i+"; pi (l1); ps "]["; pi l2; ps "] = ";
				ps (gen_exp_in_array_context ro e Dim_i).at_name; ps ";\n";
				pT tab; ps "};;\n";
			) else (
				pT tab; ps "for (__i="; pi (u1 - l1); ps ";__i>=0;__i--)\n";
				pT tab; ps "{\n";
				pT (tab + 1); ps "for (__j="; pi (u2 - l2); ps ";__j>=0;__j--)\n";
				pT (tab + 1); ps "{\n";
				mk_blck (tab + 2) cc;
				pT (tab + 2); ps sv; ps ".a[__i+"; pi (l1); ps "][__j+";
				pi l2; ps "] = ";
				ps (gen_exp_in_array_context ro e Dim_ij).at_name; ps ";\n";
				pT (tab + 2); ps "};\n";
				pT tab; ps "};\n";
			)
	| _ -> Err.intern ("mk_assgn_array_slice: "^Err.type2str typ)

and mk_assgn_array tab typ sv e cc ro =
	match typ with
	| Array(t, DimLen 1, DimLen n)
	| Array(t, DimLen n, DimLen 1) ->
			( match e.expr with
				| Value(sv')
				| SigVal(sv', _, _) when cc = [] ->
						let sv' = Sc.sgn2str sv' in
						pT tab; ps "ARRAYC1(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n); ps ",sizeof("; ps (cc_typedef t); ps "));\n"
				| _ ->
						pT tab; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
						pT tab; ps "{\n";
						mk_blck (tab + 1) cc;
						pT (tab + 1); ps sv; ps ".a[__i] = ";
						ps (gen_exp_in_array_context ro e Dim_i).at_name; ps ";\n";
						pT tab; ps "};\n"
			)
	| Array(t, DimLen n1, DimLen n2) ->
			( match e.expr with
				| Value(sv')
				| SigVal(sv', _, _) when cc = [] ->
						let sv' = Sc.sgn2str sv' in
						pT tab; ps "ARRAYC2(&"; ps sv; ps ",&"; ps sv'; ps ",";
						ps (i2s n1);
						ps ","; ps (i2s n2); ps ",sizeof("; ps (cc_typedef t);
						ps "));\n"
				| _ ->
						pT tab; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
						pT tab; ps "{\n";
						pT (tab + 1); ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
						pT (tab + 1); ps "{\n";
						mk_blck (tab + 2) cc;
						pT (tab + 2); ps sv; ps ".a[__i][__j] = ";
						ps (gen_exp_in_array_context ro e Dim_ij).at_name; ps ";\n";
						pT (tab + 1); ps "};\n";
						pT tab; ps "};\n"
			)
	| _ -> Err.intern ("mk_assgn_array: "^Err.type2str typ)

and mk_assgn_array_in_blck tab typ sv e ro =
	match typ with
	| Array(t, DimLen 1, DimLen n)
	| Array(t, DimLen n, DimLen 1) ->
			pT tab; ps sv; ps ".a[__i] = ";
			ps (gen_exp_in_array_context ro e Dim_i).at_name;
			ps ";\n";
	| Array(t, DimLen n1, DimLen n2) ->
			pT tab; ps sv; ps ".a[__i][__j] = ";
			ps (gen_exp_in_array_context ro e Dim_ij).at_name;
			ps ";\n";
	| Array(t, _, _) ->
			Err.intern "mk_action:ActInt:Matrix"
	| _ -> Err.intern ("mk_assgn_array_in_blck: "^Err.type2str typ)

(* -------------------------------------------------------------------------
GENERATE CODE for computing the value of a SIGNAL
!!! clock at the time being always is ''true'' (== tick)
------------------------------------------------------------------------- *)
let mk_eqn_sgn d =
	( match !boolgen with
		| JmpBitStyle ->
				rexpr2ps_any_cg (Sc.sgn2str d.sc_sgn) d.sc_def;
				let act =
					match d.sc_act with
					| None ->
							let s = Sc.sgn2str d.sc_sgn in
							let s =
								match d.sc_knd with
								| Mem -> s^"__n"
								| _ -> s
							in
							fun () ->
									ps "  SET_TRUE(";
									sgn2bitps s ; ps ");\n"
					| Some a ->
							fun () -> mk_action 1 a
				in
				jmp_eqn_sgn d.sc_def act
		| ExprBitSty ->
				rexpr2ps_any_cg (Sc.sgn2str d.sc_sgn) d.sc_def;
				( match d.sc_act with
					| None ->
							ps "  "; ps (rexpr2str_expr_cg d.sc_def);
							ps " && ";
							ps "SET_TRUE(";
							let s = Sc.sgn2str d.sc_sgn in
							let s =
								match d.sc_knd with
								| Mem -> s^"__n"
								| _ -> s
							in
							sgn2bitps s; ps ");\n"
					| Some a ->
							let c = rexpr2str_expr_cg d.sc_def in
							if c = "0" then (
							(* do nothing *)
							) else if c = "1" then (
								mk_action 1 a
							) else (
								pT 1; ps "if ("; ps c; ps ")\n";
								pT 1; ps "{\n";
								mk_action 2 a;
								pT 1; ps "};\n"
							)
				)
		| ExprWordSty ->
				( match d.sc_act with
					| None ->
							let s = Sc.sgn2str d.sc_sgn in
							let s =
								match d.sc_knd with
								| Mem ->
										if Sc.is_reg d.sc_sgn
										then s
										else s^"__n"
								| _ -> s
							in
							ps "  "; ps s; ps " = ";
							ps (rexpr2str_expr_cg d.sc_def); ps ";\n"
					| Some a ->
							let c = rexpr2str_expr_cg d.sc_def in
							if c = "0" then (
							(* do nothing *)
							) else if c = "1" then (
								mk_action 1 a
							) else (
								ps "  if ("; ps c; ps ")\n";
								ps "  {\n";
								mk_action 2 a;
								ps "  };\n"
							)
				)
	);
	( match d.sc_sig with
		| None -> ()
		| Some xs ->
				let sn = Sc.sgn2str d.sc_sgn in
				let sn = match d.sc_knd with
					| Mem -> sn^"__n"
					| _ -> sn
				in
				if (d.sc_def = TT) then (
					(* signal is always present @s = 0 *)
					if xs.trs_sdecl.sig_tmstmp then (
						pT 1; ps sn; ps "__t = (se_Time)0;\n"
					)
				) else if (d.sc_def = FF) then (
				(* signal is never present @s = 0 *)
				) else (
					if xs.trs_sdecl.sig_tmstmp then (
						pT 1; ps "if ("; ps (test sn); ps ")\n";
						pT 1; ps "{\n";
						pT 2; ps sn; ps "__t = -TIMING;\n";
						pT 1; ps "}\n";
						pT 1; ps "else\n";
						pT 1; ps "{\n";
						pT 2; ps sn; ps "__t = "; ps sn; ps "__t+TIMING;\n";
						let ie = get_sysprop se.conf_class id_exc_timestamp in
						pT 2; ps"if ("; ps sn; ps "__t<0) mkExc("; pi ie; ps ",0);\n";
						pT 1; ps "};\n";
					)
				)
	)

let mk_dbg_sgn d =
	if d.sc_def = (S Sc.alpha) then d.sc_def <- S Sc.alpha';
	mk_eqn_sgn d

let mk_eqn_sgn_ext_ff d = (* structurally equal to mk_eqn_sgn *)
	if d.sc_def = FF && Sc.is_ext d.sc_sgn then (
		match !boolgen with
		| JmpBitStyle
		| ExprBitSty -> () (* all values are initialized with 0 *)
		| ExprWordSty -> ps (Sc.sgn2str d.sc_sgn); ps " = 0;"
	) else ()

(* -------------------------------------------------------------------------
GENERATE CODE for copying the NEW REGISTER / MEMORY VALUES
------------------------------------------------------------------------- *)
let bs_copy_val typ s =
	let sn = Sc.sgn2str s in
	if typ = Null then (
		Err.intern "bs_copy_val"
	) else if typ = t_bool then (
		wire2mem s
	) else if is_primitive_typ typ then (
		ps sn; ps " = "; ps (sn^"__n;\n")
	) else if is_array_typ typ then (
		match typ with
		| Array(t, DimLen 1, DimLen n)
		| Array(t, DimLen n, DimLen 1) ->
				pT 1; ps "ARRAYC1(&"; ps sn; ps ",&"; ps sn; ps "__n,";
				ps (i2s n); ps ",sizeof("; ps (cc_typedef t); ps "));\n"
		| Array(t, DimLen n1, DimLen n2) ->
				pT 1; ps "ARRAYC2(&"; ps sn; ps ",&"; ps sn; ps "__n,";
				ps (i2s n1); ps ","; ps (i2s n2); ps ",sizeof(";
				ps (cc_typedef t); ps "));\n"
		| _ ->
				Err.intern "bs_copy_val"
	) else (
		pT 1; ps "memcpy((char *)&"; ps sn; ps ",(char *)&";
		ps (sn^"__n"); ps ",sizeof(tat_"; psc (type2id typ); ps "));\n"
	)

let bs_copy_mem d =
	match d.sc_act with
	| None -> wire2mem d.sc_sgn
	| Some a ->
			( match a with
				| ActEmit(_, _, _, _, Val e, _, _, _) when e.expr = NullObj ->
						()
				| ActEmit(_, _, _, _, _, _, xs, _) ->
						bs_copy_val (sig2valtyp xs) (Sc.sgn2val (sig2sgn xs))
				| ActAssgn(_, s, e, _, _) ->
						let typ = some e.etyp "bs_copy_mem" in
						if d.sc_knd <> Nxt && typ = t_bool
						then bs_copy_val typ s
				| _ -> ()
			)

let copy_buffer sv typ buf_size =
	if buf_size < 2 then (
	(* do nothing; no value stored *)
	) else if buf_size = 2 && not(is_matrix_typ typ) then (
		mk_valcp 1 typ false ~lhs: (sv^"[1]") ~rhs: (sv^"[0]")
	) else (
		pT 1; ps "if (--"; ps sv; ps ".pt < 0) { ";
		ps sv; ps ".pt = "; pi (buf_size - 1); ps "; };\n";
		mk_valcp 1 typ false
			~lhs: (sv^".b.a["^sv^".pt]")
			~rhs: (sv^".b.a[(__b="^sv^".pt+1,__b>"^i2s (buf_size - 1)^"?0:__b)]")
	)

let copy_sig_buffer xs =
	if is_output_sig xs || is_local_sig xs then (
		let sv = Sc.sgn2valstr (sig2sgn xs) in
		let sv = if is_delayed_typ (sig2typ xs) then sv^"__n" else sv in
		let typ = sig2valtyp xs in
		let buf_size = dims2value(sig2buf xs) in
		copy_buffer sv typ buf_size
	)

let copy_int_sig_buffer = function
	| LclFld (s, x) ->
			()
	| LclSig xs ->
			copy_sig_buffer xs
	| IntSig(sv, typ, buf_size) ->
			copy_buffer (Sc.sgn2str sv) typ buf_size

let mk_copy_mem sa =
	List.iter bs_copy_mem sa.sa_mems;
	List.iter copy_sig_buffer sa.sa_sigs;
	List.iter copy_int_sig_buffer sa.sa_dcls;
	match !boolgen with
	| JmpBitStyle
	| ExprBitSty ->
			for i = 0 to mem_bits.wordnum do
				pT 1; ps mem_bits.prefix; pi i; ps " = ";
				ps mem_bits.prefix; pi i; ps "__n;\n"
			done
	| ExprWordSty ->
			pT 1; ps (Sc.sgn2str sa.sa_alpha); ps " = 0;\n"

(* -------------------------------------------------------------------------
GENERATE CODE to READ input signal values at the begin of an instant
------------------------------------------------------------------------- *)
let mk_input_callback_target xs =
	if is_input_sig xs then (
		let s = sig2sgn xs in
		let sn = Sc.sgn2str s in
		let typ = sig2valtyp xs in
		let sv = Sc.sgn2valstr s in
		(* input callback for target, like mk_output_callback_target *)
		let cbe, xe = principal2callback xs in
		let lbl = xs.trs_sdecl.sig_pos in
		let cid = type2id (some xe.etyp "mk_input_callback_target") in
		let dcl = get_decl_after_tc cid id_op_new_val 0 in
		let call =
			if dcl.scope = Class then (
				"f_"^(Ly.c2str dcl.origin)^"_new_val_0( void )"
			) else (
				let nv = "_new_val_0((pat_" in
				let suf imp this = nv^imp^")("^this^"))" in
				dynamic_bind lbl cbe id_op_new_val 0 suf
			)
		in
		set_from_expr 2 ~lhs: sn ~rhs: call;
		if typ = Null && not(xs.trs_sdecl.sig_tmstmp) then (
		(* no timestamp, no data *)
		) else (
			pT 1; ps "if ("; ps (test sn); ps ")";
			pT 1; ps "{\n  ";
			if xs.trs_sdecl.sig_tmstmp then (
				pT 2; ps sn; ps "__t = -TIMING;\n"
			);
			if typ = Null then (
			(* no data for pure signals *)
			) else (
				let dcl = get_decl_after_tc cid id_op_get_val 0 in
				let call =
					if dcl.scope = Class then (
						"f_"^(Ly.c2str dcl.origin)^"_get_val_0( void )"
					) else (
						let gv = "_get_val_0((pat_" in
						let suf imp this = gv^imp^")("^this^"))" in
						dynamic_bind lbl cbe id_op_get_val 0 suf
					) in
				mk_valcp 1 typ true ~lhs: sv ~rhs: call;
				let buf_size = dims2value(sig2buf xs) in
				if buf_size < 2 then (
				(* nothing to do; no values buffered *)
				) else if buf_size = 2 && not(is_matrix_typ typ) then (
					mk_valcp 1 typ true ~lhs: (sv^"[0]") ~rhs: (sv^"[1]")
				) else (
					pT 2; ps "if (--"; ps sv; ps ".pt > 0)\n";
					pT 2; ps "{\n";
					pT 3; ps sv; ps ".pt = "; pi (buf_size - 1); ps "; };\n";
					pT 3; ps sv; ps ".b.a["; ps sv; ps ".pt] = ";
					ps sv; ps ";\n"
				)
			);
			if xs.trs_sdecl.sig_tmstmp then (
				pT 2; ps "}\n"; (* cleared when instant starts *)
				pT 2; ps "else\n";
				pT 2; ps "{\n";
				pT 3; ps sn; ps "__t="; ps sn; ps "__t+TIMING;\n";
				let ie = get_sysprop se.conf_class id_exc_timestamp in
				pT 3; ps "if ("; ps sn; ps "__t<0) mkExc("; pi ie; ps ",0);\n";
			);
			pT 2; ps "};\n"
		)
	) else (
	)

(* -------------------------------------------------------------------------
GENERATE CODE for SIMULATION systems. Names and types of values for all
input / output signals are written to stdout. This is used by the simulator.
------------------------------------------------------------------------- *)
let mk_env_sig xs =
	let valt = match sig2valtyp xs with
		| Null -> " "
		| typ -> "<"^(Err.type2str typ)^"> "
	in
	pT 1; ps "fprintf(stdout,\"";
	if is_input_sig xs then (
		ps "$sensor"; ps valt; ps " "
	) else if is_output_sig xs then (
		ps "$signal"; ps valt;
	) else (
		ps "$local"; ps valt; ps " "
	);
	ps (sig2str xs);
	ps "\\n\");\n"

(* ------------------------------------------------------------------------- *)
let cp_sim_input xs =
	if is_input_sig xs then (
		let s = sig2sgn xs in
		let sn = Sc.sgn2str s in
		let sv = Sc.sgn2valstr s in
		let typ = sig2valtyp xs in
		let buf_size = dims2value(sig2buf xs) in
		(* shift the buffer *)
		set_from_expr 1 ~lhs: sn ~rhs: ("has_new && "^sn^"__n");
		pT 1; ps sn; ps "__n = 0;\n";
		if buf_size < 2 then (
		(* nothing to do; no values buffered. Note that no new
		* value is given to the input ..n , hence
		* we can update by this value *)
		) else if buf_size = 2 && not(is_matrix_typ typ) then (
			mk_valcp 1 typ false ~lhs: (sv^"[1]") ~rhs: (sv^"[0]")
		) else (
			pT 1; ps "if (--"; ps sv; ps ".pt < 0)\n";
			pT 1; ps "{\n";
			pT 2; ps sv; ps ".pt = "; pi (buf_size - 1); ps ";\n";
			pT 1; ps "};\n";
			mk_valcp 1 typ false ~lhs: (sv^".b.a["^sv^".pt]") ~rhs: (sv^"__n")
		);
		pT 1; ps "if ("; ps (test sn); ps")\n";
		pT 1; ps "{\n";
		if xs.trs_sdecl.sig_tmstmp then (
			pT 2; ps sn; ps "__t = -TIMING;\n";
		);
		if typ = Null then (
		(* no data for pure signals *)
		) else (
			(* update *)
			if buf_size < 2 then (
				mk_valcp 2 typ false ~lhs: sv
					~rhs: (sv^"__n")
			) else if buf_size = 2 && not(is_matrix_typ typ) then (
				mk_valcp 2 typ false ~lhs: (sv^"[0]")
					~rhs: (sv^"__n")
			) else (
			(* nothing to be done; circular buffer updated at
			* every instant. *)
			);
		);
		if se.target_sys = Simulation then (
			write_to_stdout xs
		);
		
		let ie = get_sysprop se.conf_class id_exc_timestamp in
		( match !boolgen with
			| ExprBitSty
			| JmpBitStyle ->
					if xs.trs_sdecl.sig_tmstmp then (
						pT 1; ps "}\n"; (* cleared when instant starts *)
						pT 1; ps "else\n";
						pT 1; ps "{\n";
						pT 2; ps sn; ps "__t="; ps sn; ps "__t+TIMING;\n";
						pT 2; ps "if ("; ps sn; ps "__t<0) mkExc("; pi ie; ps ",0);\n";
					);
					pT 1; ps "};\n\n"
			| ExprWordSty ->
					pT 1; ps "}\n";
					pT 1; ps "else\n";
					pT 1; ps "{\n";
					pT 2; ps sn; ps " = 0;\n";
					if xs.trs_sdecl.sig_tmstmp then (
						pT 2; ps sn; ps "__t="; ps sn; ps "__t+TIMING;\n";
						pT 2; ps "if ("; ps sn; ps "__t<0) mkExc("; pi ie; ps ",0);\n";
					);
					pT 1; ps "};\n\n"
		)
	) else ()

let scan_simulator_input xs =
	if is_input_sig xs then (
		let dotn = sig2str xs in
		let typ = sig2valtyp xs in
		let s = sig2sgn xs in
		let sn = Sc.sgn2str s in
		let sv = Sc.sgn2valstr s in
		pT 1; ps "}\n";
		pT 1; ps "else if ( scan_String(\"+"; ps dotn; ps "\") )\n";
		pT 1; ps "{\n";
		pT 2; ps "scan_parOpen();\n";
		pT 2; ps (sn^"__n"); ps " = 1;\n";
		( match typ with
			| Null ->
					() (* no value *)
			| Simple _ ->
					pT 2; ps sv; ps "__n = ";
					ps (get_scan_rtn (type2id typ)); ps "();\n"
			| Array(t, DimLen 1, DimLen n)
			| Array(t, DimLen n, DimLen 1) ->
					let s = get_scan_rtn (type2id t) in
					pT 2; ps "scan_curlyOpen();\n";
					for i = 0 to n - 2 do
						pT 2; ps sv; ps "__n.a["; pi i; ps "] = "; ps s; ps "();\n";
						pT 2; ps "scan_comma();\n";
					done;
					pT 2; ps sv; ps "__n.a["; pi (n - 1); ps"] = "; ps s; ps "();\n";
					pT 2; ps "scan_curlyClose();\n"
			| Array(t, DimLen n1, DimLen n2) ->
					let s = get_scan_rtn (type2id t) in
					pT 2; ps "scan_curlyOpen();\n";
					for i = 0 to n1 - 2 do
						pT 2; ps "scan_curlyOpen();\n";
						for j = 0 to n2 - 2 do
							pT 2; ps sv; ps "__n.a["; pi i; ps "]["; pi j;
							ps "] = "; ps s; ps "();\n";
							pT 2; ps "scan_comma();\n";
						done;
						pT 2; ps sv; ps "__n.a["; pi i; ps "]["; pi (n2 - 1);
						ps"] = "; ps s; ps "();\n";
						pT 2; ps "scan_curlyClose();\n";
						pT 2; ps "scan_comma();\n";
					done;
					pT 2; ps "scan_curlyOpen();\n";
					for j = 0 to n2 - 2 do
						pT 2; ps sv; ps "__n.a["; pi (n1 - 1); ps "]["; pi j;
						ps "] = "; ps s; ps "();\n";
						pT 2; ps "scan_comma();\n";
					done;
					pT 2; ps sv; ps "__n.a["; pi (n1 - 1); ps "]["; pi (n2 - 1);
					ps"] = "; ps s; ps "();\n";
					pT 2; ps "scan_curlyClose();\n";
					pT 2; ps "scan_curlyClose();\n"
			| Array _ ->
					Err.intern "scan_simulator_input"
			| _ ->
					let cid = type2id typ
					and flds = get_fields typ ac.ac_classes
					and pref = sv^"__n." in
					let sc f =
						let t = (get_decl_after_tc cid f (- 1)).signature.rt in
						pT 2; ps "if ( scan_String(\""; psmf f; ps "\") )\n";
						pT 2; ps "{\n";
						pT 2; ps "scan_parOpen();\n";
						
						let s = get_scan_rtn (type2id t) in
						pT 3; ps pref; psmf f; ps " = "; ps s; ps "();\n";
						pT 3; ps "scan_parClose();\n";
						pT 2; ps "}\n";
						pT 2; ps "else\n";
						pT 2; ps "{\n";
						pT 3; ps "scan_abort (\"invalid field detected\");\n";
						pT 2; ps "};\n";
					in
					lst_sep2ps sc "  scan_comma();\n" flds
		);
		pT 2; ps "scan_parClose();\n"
	) else ()

(* -------------------------------------------------------------------------
info for the simulator
------------------------------------------------------------------------- *)
let extract_fields cid sel fl =
	let rec extract_fields = function
		| [] -> []
		| f:: fl ->
				let el = extract_fields fl in
				let dcl = get_decl_after_tc cid f (- 1) in
				if sel dcl
				then (f, dcl):: el
				else el
	in
	try
		extract_fields fl
	with Not_found ->
			Err.intern "extract_fields"

let cc_field_decl_simul enc_typ (fid, dcl) =
	let v2t = prepare_subst enc_typ in
	let rt = subst_map dcl.signature.rt v2t in
	let prt = Err.type2str rt in
	let enc = cc_type2str enc_typ in
	set_cc_context enc_typ "Null" v2t false;
	pT 1; ps "fprintf(stdout,\"";
	if (is_primitive_typ rt)
	then ( ps "$exp "; ps prt )
	else if can_expand dcl
	then ( ps "$exp "; ps (Err.type2str (get_expandtyp dcl)) )
	else ( ps "$ref "; ps prt );
	ps " "; psmf fid; ps " %d\\n\",\n";
	pT 2; ps "(int)(&(((pat_";
	ps enc; ps ")0)->"; psmf fid; ps ")) - (int)(&(((pat_";
	ps enc; ps ")0)->header)));\n"

let file_name_tbl = Hashtbl.create 3
let fna_counter = ref 0

let fna_add fn =
	try Hashtbl.find file_name_tbl fn
	with Not_found ->
			fna_counter := !fna_counter + 1;
			Hashtbl.add file_name_tbl fn !fna_counter;
			!fna_counter

let fna_clear () =
	fna_counter := 0;
	Hashtbl.clear file_name_tbl

let fna_try_find fn =
	try Hashtbl.find file_name_tbl fn
	with Not_found ->
			Err.intern ("fna_try_find: "^fn)

let fna_iter f =
	Hashtbl.iter f file_name_tbl

let cc_class_pos_simul ast =
	ps " (";
	( match Ly.lbl2srcp ast.src_pos ast.lbl2sp with
		| TextPos(fid, go, fl, fc, tl, tc) ->
				let fn = ast.src_file in
				if fn = "" then () else (
					let fid = fna_add fn in
					pi fid; ps ","; pi go; ps ","; pi fl; ps ",";
					pi fc; ps ","; pi tl; ps ","; pi tc
				)
		| _ -> ()
	);
	ps ")"

let cc_constr_decl_simul ast =
	let c = rctclass2constr ast in
	pT 1; ps "fprintf(stdout,\"$constr "; ps (lbl2str c.csrcp); ps "\\n\");\n"

let cc_rctstmt2tags ast s =
	let rec cc_rctstmt2tags = function
		| ExprStmt(lbl,{ expr = If(sl) }) ->
				List.iter cc_rctthen2tags sl
		| ExprStmt (lbl,{ expr = Call(mf, Some el) }) ->
				( try let dcls = Hashtbl.find ast.symtab (mf, List.length el) in
					( match dcls with
						| dcl :: _ ->
								( match dcl.entry with
									| Method m ->
											( match m.method_kind with
												| RctMethod _ ->
														pT 1; ps "fprintf(stdout,\"$mtag (";
														ps (lbl2str lbl);
														ps ",";
														ps (lbl2str m.msrcp);
														ps ")\\n\");\n"
												| _ -> ()
											)
									| _ -> ()
								)
						| _ -> ()
					)
				with Not_found -> ()
				)
		| ExprStmt _ ->
				()
		| GraphicStM (lbl, g) ->
				let fid = fna_add g.gstm_file in
				pT 1; ps "fprintf(stdout,\"$gtag (";
				ps (lbl2str lbl);
				ps ",";
				pi fid;
				ps ")\\n\");\n"
		| Cancel(lbl, strong, next, sl, csl) ->
				List.iter cc_rctstmt2tags sl;
				List.iter cc_rctthen2tags csl
		| Schedule _ -> ()
		| Nothing _ -> ()
		| Sustain _ -> ()
		| Halt _ -> ()
		| Next _ -> ()
		| Emit _ -> ()
		| FlowContext _ -> ()
		| Par(_, sll) -> List.iter cc_rctstmtl2tags sll
		| RctLoop(_, sl) -> cc_rctstmtl2tags sl
		| Await(_, _, csl) -> List.iter cc_rctthen2tags csl
		| Activate(_, _, sl, _) -> cc_rctstmtl2tags sl
		| NextState(_, st) -> ()
		| TextStM (_, a) -> cc_rctautomaton2tags a
		| LetStmt(_, ld) -> cc_rctstmtl2tags ld.letil
		| _ -> Err.intern "rctstmttags"
	and cc_rctstmtl2tags sl = List.iter cc_rctstmt2tags sl
	and cc_rctthen2tags = function
		| Then(_, xe, sl) -> cc_rctstmtl2tags sl
		| Else(_, sl) -> cc_rctstmtl2tags sl
	and cc_rctautomaton2tags a =
		List.iter cc_rctstate2tags a.a_states
	and cc_rctstate2tags st =
		cc_rctstmtl2tags st.sdo;
		cc_rctstmtl2tags st.sentry;
		cc_rctstmtl2tags st.sduring;
		cc_rctstmtl2tags st.sexit;
		List.iter cc_rctthen2tags st.strans
	in
	cc_rctstmt2tags s

let cc_tags_simul ast =
	let sel_mthd _ = function
		| dcl:: _ ->
				( match dcl.entry with
					| Method m ->
							( match m.method_kind with
								| RctMethod _ ->
										( match m.mbody with
											| TextStmtL sl -> List.iter (cc_rctstmt2tags ast) sl
											| _ -> ()
										)
								| _ -> ()
							)
					| Constructor c ->
							( match c.cactive with
								| Some sl -> List.iter (cc_rctstmt2tags ast) sl
								| None -> ()
							)
					| _ -> ()
				)
		| _ -> ()
	in
	Hashtbl.iter sel_mthd ast.symtab

let cc_label_pos_simul lbl sp =
	match sp with
	| NoSrcPos	->
			()
	| HaltPos(fid, go, fl, fc, tl, tc) ->
			pT 1; ps "fprintf(stdout,\""; ps (lbl2str lbl); ps " : ";
			ps "("; pi (fna_try_find fid); ps ","; pi go; ps ","; pi fl; ps ",";
			pi fc; ps ","; pi tl; ps ","; pi tc; ps "):1";
			ps "\\n\");\n"
	| EmitPos(fid, go, fl, fc, tl, tc) ->
			pT 1; ps "fprintf(stdout,\""; ps (lbl2str lbl); ps " : ";
			ps "("; pi (fna_try_find fid); ps ","; pi go; ps ","; pi fl; ps ",";
			pi fc; ps ","; pi tl; ps ","; pi tc; ps "):2";
			ps "\\n\");\n"
	| TextPos(fid, go, fl, fc, tl, tc) ->
			pT 1; ps "fprintf(stdout,\""; ps (lbl2str lbl); ps " : ";
			ps "("; pi (fna_try_find fid); ps ","; pi go; ps ","; pi fl; ps ",";
			pi fc; ps ","; pi tl; ps ","; pi tc; ps "):0";
			ps "\\n\");\n"
	| GraphicPos(fid, go) ->
			pT 1; ps "fprintf(stdout,\""; ps (lbl2str lbl); ps " : ";
			ps "("; pi (fna_try_find fid); ps ","; pi go; ps ")";
			ps "\\n\");\n"

let cc_class_decl_simul (typ, cno, ats) =
	let cid = type2id typ in
	let ast = i_ast cid in
	let seli dcl = (dcl.scope = Instance) in
	pT 1; ps "fprintf(stdout,\"class "; ps (Err.type2str typ);
	cc_class_pos_simul ast;
	ps " {\\n\");\n";
	List.iter (cc_field_decl_simul typ) (extract_fields cid seli ats);
	( match ast.classkind with
		| RctClass
		| ConfClass ->
				cc_constr_decl_simul ast;
				cc_tags_simul ast
		| _ -> ()
	);
	(* XXX BETTER PRINT CNO, TOO
	ps "fprintf(stdout,\"}\\n static{\\n\");\n";
	List.iter (cc_field_decl_simul typ) (extract_fields cid sels ats);
	ps "fprintf(stdout,\"}\\n fpos{\\n\");\n";
	XXX *)
	Ly.lbl2srcp_iter ast.lbl2sp cc_label_pos_simul;
	pT 1; ps "fprintf(stdout,\"}\\n\");\n"

(* =========================================================================
generate data definitions and actions for the reactive part of the appl.
for SIMULATION and TARGET
========================================================================= *)
let pr_co s = print_detailed_comment "\n/* " s " */"

let mk_rctengine_decls sa =
	mk_rctengine_generator_reset ();
	print_detailed_comment "/* " "memory for ALPHA" " */";
	alloc_mem Sc.alpha;
	if se.target_sys = Simulation then alloc_wire Sc.alpha';
	pr_co "wires for internal and outputs";
	List.iter (fun s -> alloc_wire s) sa.sa_wires;
	pr_co "memory";
	List.iter (fun s -> alloc_mem s) sa.sa_regs;
	pr_co "wires for the result of data-expr's in ifs";
	List.iter mk_if_and_counter sa.sa_eqns;
	List.iter mk_if_and_counter sa.sa_mems;
	pr_co "wires for input; data for valued input/output signals";
	List.iter mk_ext_sign_decl sa.sa_sigs;
	pr_co "data for valued internal signals and memories";
	List.iter mk_int_decl sa.sa_dcls;
	pr_co "run indices for vectors and matrices";
	ps "\nint __i,__j,__k,__b;\n";
	pr_co "time buffer for signals, that need timestamps";
	List.iter mk_ext_timestamp sa.sa_sigs;
	finish_alloc ()

let gen_reactive_part_for_c conf_cid = function
	| None -> ()
	| Some sa ->
	
			let alpha = Sc.sgn2str Sc.alpha in
			let (t, s, v) = field2literal conf_cid id_timing in
			if t != t_time then Err.intern "grpiC:time";
			let timing = int64_2_int (some v "grpiC:tval") in
			cc.cc_has_timing <- false;
			mk_rctengine_decls sa;
			
			(* generate function responsible to execute ONE REACTIVE STEP [INSTANT] *)
			( match se.target_sys with
				| Simulation ->
						pn(); print_comment "read input signals buffered by Lib/sim_reader.c";
						ps "\nvoid scan_signal_input (void)\n{\n";
						pT 1; ps "if (0)\n";
						pT 1; ps "{\n";
						List.iter scan_simulator_input sa.sa_sigs;
						pT 1; ps "}";
						pT 1; ps "else";
						pT 1; ps "{\n";
						pT 2; ps "scan_abort (\"invalid signal detected\");\n";
						pT 1; ps "}\n";
						ps "}\n\n";
						
						print_comment "copy pending input signals to the reactive machine";
						ps "\nvoid cp_input_signal (se_Bool has_new)\n{\n";
						List.iter cp_sim_input sa.sa_sigs;
						ps "}\n";
						print_comment "function to execute ONE REACTIVE STEP [1 INSTANT]";
						ps (runtime_engine Decl "instant");
						pT 1; ps "exception = setjmp( exception_env );\n";
						pT 1; ps "if (exception != 0)\n";
						pT 1; ps "{\n";
						pT 2; ps "fprintf(stdout,\"--exc(%d) %s\\n\", (se_Int)exception,\
						time2str(__deltat));\n";
						pT 2; ps "fflush(stdout);\n";
						pT 2; ps "return;\n";
						pT 1; ps "};\n\n";
						wire_reset ();
						( match !boolgen with
							| JmpBitStyle
							| ExprBitSty ->
									pT 1; ps "if ("; ps (test alpha);
									ps ") SET_TRUE("; sgn2bitps (Sc.sgn2str Sc.alpha'); ps ");\n"
							| ExprWordSty	->
									pT 1; ps (Sc.sgn2str Sc.alpha'); ps " = "; ps alpha; ps ";\n";
						);
						pT 1; ps "prepare_sim_step ();\n";
						print_comment "set the timestamp for the instant";
						pT 1; ps "tp_set_timer();\n";
				
				| Host
				| Makefile
				| Platform _ ->
						print_comment "function to execute ONE REACTIVE STEP [1 INSTANT]";
						ps (runtime_engine Decl "instant");
						pT 1; ps "SE_EXCEPTION_CATCH\n";
						wire_reset ();
						List.iter mk_input_callback_target sa.sa_sigs;
				
				| _ -> Err.intern "gen_reactive_part_for_c:1"
			);
			print_comment "signal equations";
			List.iter mk_eqn_sgn sa.sa_eqns;
			print_comment "output signals";
			List.iter mk_output_callback sa.sa_sigs;
			print_comment "memory equations";
			List.iter mk_eqn_sgn sa.sa_mems;
			print_comment "copy new mem vals to mem & set alpha";
			mk_copy_mem sa;
			if se.target_sys = Simulation then (
				if timing = 0 then
					print_comment "get the delta time for the instant";
				pT 1; ps "tp_reset_timer();\n";
				print_comment "debug equations";
				List.iter mk_dbg_sgn sa.sa_dbgs
			);
			
			( match se.target_sys with
				| Simulation ->
				(* check, whether the last instant did exceed the timing interval *)
						print_comment "finish the instant";
						if timing = 0 then (
							(* dense instant execs, no violation *)
							pT 1; ps "fprintf(stdout,\"--i %s\\n\",time2str(__deltat));\n";
							pT 1; ps "fflush(stdout);\n";
							pT 1; ps "return 0;\n}\n"
						) else (
							let ie = get_sysprop se.conf_class id_exc_instant in
							pT 1; ps "if (__deltat >= TIMING)\n";
							pT 1; ps "{\n";
							pT 2; ps "fprintf(stdout,\"--exc("; pi ie; ps ") %s\\n\",\
							time2str(__deltat));\n";
							pT 2; ps "fflush(stdout);\n";
							pT 2; ps "return "; pi ie; ps ";\n";
							pT 1; ps "}\n";
							pT 1; ps "else\n";
							pT 1; ps "{\n";
							pT 2; ps "fprintf(stdout,\"--i %s\\n\",time2str(__deltat));\n";
							pT 2; ps "fflush(stdout);\n";
							pT 2; ps "return 0;\n";
							pT 1; ps "};\n";
							ps "}\n"
						)
				| Host
				| Platform _ ->
				(* check, whether the last instant did exceed the interval defined by
				the timing constant. If not, do a suitable sleep *)
						if timing = 0 then (
							(* dense instant execs, no violation, no sleep  *)
							if cc.cc_has_timing then (
								pT 1; ps "tp_reset_timer();\n"
							) else (
							(* no timer needed *)
							)
						) else (
							let ie = get_sysprop se.conf_class id_exc_instant in
							pT 1; ps "tp_sleep_until("; pi timing; ps ","; pi ie; ps ");\n";
						);
						pT 1; ps "return 0;\n";
						ps "}\n";
				| _ ->
						Err.intern "gen_reactive_part_for_c:end"
			);
			
			(* generate function responsible to INITIALIZE the RUNTIME ENGINE *)
			print_comment "initialize the runtime engine";
			ps (runtime_engine Decl "init");
			pT 1; ps "SE_EXCEPTION_INIT\n";
			ps "/* clear memory, set alpha */\n";
			rctengine_reset 1 sa;
			ps "/* eqns for external signals always false */\n";
			List.iter mk_eqn_sgn_ext_ff sa.sa_eqns;
			
			let conf_str = c2str (type2id se.conf_class) in
			if t != t_time then Err.intern "mk_c:timing";
			if se.target_sys = Simulation then (
				( match ac.ac_sca with
					| None -> ()
					| Some sa ->
							ps "/* signal info for the simulator */\n";
							List.iter mk_env_sig sa.sa_sigs
				);
				ps "/* class info for the simulator */\n";
				fna_clear();
				List.iter cc_class_decl_simul ac.ac_classes;
				pT 1; ps "fprintf(stdout,\"timing "; pi timing;
				ps "\\n$filenames {\\n\");\n";
				fna_iter
					( fun fn fid ->
								pT 1; ps "fprintf(stdout,\""; pi fid; ps " \\\"";
								ps (String.escaped fn); ps "\\\"\\n\");\n")
				;
				pT 1; ps "fprintf(stdout,\"}\\n$configuration %d ";
				ps (c2str (type2id se.conf_class));
				ps "\\n\",(int)&conf_obj);fflush(stdout);\n";
			) else (
			(* target doesn'n need simulator infos *)
			);
			if ac.ac_gc = [] then (
			(* no memory management *)
			) else (
				ps "\n/* initialization of memory management */\n";
				pT 1; ps "{ int alloc__length[] = {";
				lst_sep2ps ( fun t -> ps "sizeof(tat_"; ps (cc_type2str t); ps ")" )
					"," ac.ac_gc;
				ps ",0};\n";
				pT 2; ps "gc_init(&conf_obj,sizeof(tat_"; ps conf_str;
				ps "),alloc__length);\n";
				pT 1; ps "}\n"
			);
			ps "\n/* initialization (if any) of static data */\n";
			List.iter cc_static_init ac.ac_classes;
			pT 1; ps "i_"; ps conf_str; ps " (&conf_obj);\n";
			if se.target_sys = Simulation then (
				pT 1; ps "sim_init("; pi timing; ps ");\n";
				pT 1; ps "seThread_create(&sim_rdr,&sim_reader);\n";
			) else (
			(* target doesn'n need simulator infos *)
			);
			pT 1; ps "f_"; ps conf_str; ps "_"; ps conf_str; ps "_0(&conf_obj);\n";
			ps "/* initialisation of signal declarations */\n";
			List.iter (mk_sig_init 1) sa.sa_sigs;
			ps "\n/* initialisation of internal declarations */\n";
			List.iter (mk_init_int_sig 1) sa.sa_dcls;
			ps "\n/* initialisation of the timer */\n";
			pT 1; ps "tp_set_timer();\n";
			ps "}\n"

(* =========================================================================
deal with parameters for SIMULINK and SCICOS
========================================================================= *)let parameters = ref [];;

let mk_parameters () =
	let ccid = type2id se.conf_class in
	let cast = i_ast ccid in
	if cast.classkind != ConfClass then (
		Err.intern "mk_parameters"
	) else (
		let sel id n dcl =
			if is_field dcl && dcl.origin = ccid
			then ( Some dcl )
			else ( None ) in
		let counter = ref 0 in
		parameters := [];
		List.iter
			( fun dcl ->
						if dcl.parameter then (
							parameters := (!counter, dcl)::!parameters;
							counter := !counter + 1
						);
			) (List.rev (i_fromsymtab sel ccid));
		parameters := List.rev !parameters
	)

(* =========================================================================
generate data definitions and actions for the reactive part of the appl.
for SIMULINK
========================================================================= *)
let typ2simulink_typid typ =
	match typ with
	| Simple id when id = id_double -> "SS_DOUBLE"
	| Simple id when id = id_float -> "SS_SINGLE"
	| Simple id when id = id_int8 -> "SS_INT8"
	| Simple id when id = id_char -> "SS_UINT8"
	| Simple id when id = id_short -> "SS_INT16"
	| Simple id when id = id_uint16 -> "SS_UINT16"
	| Simple id when id = id_int -> "SS_INT32"
	| Simple id when id = id_uint32 -> "SS_UINT32"
	| Simple id when id = id_long -> "SS_INT64"
	| Simple id when id = id_uint64 -> "SS_UINT64"
	| Simple id when id = id_bool -> "SS_BOOLEAN"
	| _ -> Err.msg (Err.SimulinkParamType typ)

let check_parameter_simulink (no, dcl) =
	( match dcl.entry with
		| Field fd ->
				let typ = dcl.signature.rt in
				( match typ with
					| Simple _ ->
							pT 1; ps " if (ssGetDTypeIdFromMxArray(\
							ssGetSFcnParam(simulinkS,";pi no;ps")) != ";
							ps (typ2simulink_typid typ); ps "\n";
							ps "     || mxGetNumberOfElements(ssGetSFcnParam(simulinkS,";
							pi no; ps")) != 1)\n";
							pT 1; ps "{\n";
							pT 2; ps "ssSetErrorStatus(simulinkS,";
							ps "\"parameter '"; ps (mf2str dcl.name); ps "' ";
							ps "must be a scalar of type ";
							ps (Err.type2str typ); ps "\");\n";
							pT 2; ps "return;\n";
							pT 1; ps "}\n";
					| Array(t, DimLen 1, DimLen n)
					| Array(t, DimLen n, DimLen 1) ->
							pT 1; ps "if (ssGetDTypeIdFromMxArray(\
							ssGetSFcnParam(simulinkS,";pi no;ps")) != ";
							ps (typ2simulink_typid t); ps "\n";
							pT 2; ps "|| mxGetNumberOfElements(ssGetSFcnParam(simulinkS,";
							pi no; ps")) != "; pi n; ps ")\n";
							pT 1; ps "{\n";
							pT 2; ps "ssSetErrorStatus(simulinkS,";
							ps "\"parameter '"; ps (mf2str dcl.name); ps "' ";
							ps "must be a vector value of type ";
							ps (Err.type2str typ); ps "\");\n";
							pT 2; ps "return;\n";
							pT 1; ps "}\n";
					| _ -> Err.msg (Err.SimulinkParamType typ)
					
				);
		| _ -> ()
	)

let set_parameter_size_simulink (no, dcl) =
	let typ = dcl.signature.rt in
	pT 1; ps "ssRegDlgParamAsRunTimeParam(simulinkS,"; pi no; ps ","; pi no;
	ps",\""; ps (mf2str dcl.name); ps"\",";
	( match typ with
		| Simple _ ->
				ps (typ2simulink_typid typ)
		| Array(t, DimLen 1, DimLen n)
		| Array(t, DimLen n, DimLen 1) ->
				ps (typ2simulink_typid t)
		| _ -> Err.msg (Err.SimulinkParamType typ)
	); ps");\n"

let mk_simulinkno xsl =
	let rec bs_simulinkno ~input ~output = function
		| [] ->
				(input, output)
		| xs:: xsl ->
				let input', output' =
					if is_input_sig xs then (
						xs.simulinkno <- input;
						input + 1, output
					) else if is_output_sig xs then (
						xs.simulinkno <- output;
						input, output + 1
					) else (
						input, output
					)
				in
				bs_simulinkno ~input: input' ~output: output' xsl
	in
	bs_simulinkno ~input: 0 ~output: 0 xsl

let scan_simulink_input xs =
	if is_input_sig xs then (
		let typ = sig2valtyp xs in
		let s = sig2sgn xs in
		let sn = Sc.sgn2str s in
		let sv = Sc.sgn2valstr s
		and name = sig2str xs in
		if not (xs.trs_sdecl.sig_clock.expr = v_true.expr)
		then Err.msg (Err.SfunctionClock name);
		pT 1; ps (sn^"__n"); ps " = 1;\n"; (* always present *)
		pT 1; ps "{\n";
		( match typ with
			| Null ->
					pT 2; ps sv; ps "__n = * ((boolean_T *)\
					* ssGetInputPortSignalPtrs(simulinkS, ";
					pi xs.simulinkno; ps "));\n"
			| Simple _ when typ = t_double ->
					pT 2; ps sv; ps "__n = * ((real_T *)\
					* ssGetInputPortRealSignalPtrs(simulinkS, ";
					pi xs.simulinkno; ps "));\n"
			| Simple _ ->
					let cid = type2id typ in
					pT 2; ps sv; ps "__n = *("; ps (cc_builtintypedef cid);
					ps " *)*ssGetInputPortSignalPtrs(simulinkS,";
					pi xs.simulinkno; ps ");\n"
			| Array(t, DimLen 1, DimLen n)
			| Array(t, DimLen n, DimLen 1) when t = t_float || t = t_double ->
					let cid = type2id t in
					if t = t_float then (
						pT 2; ps "InputPtrsType __u = \
						ssGetInputPortSignalPtrs(simulinkS, ";
						pi xs.simulinkno; ps ");\n"
					) else (
						pT 2; ps "InputRealPtrsType __u = \
						ssGetInputPortRealSignalPtrs(simulinkS, ";
						pi xs.simulinkno; ps ");\n"
					);
					pT 2; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
					pT 2; ps "{\n";
					pT 3; ps sv; ps "__n.a[__i] = *("; ps (cc_builtintypedef cid);
					ps "*)__u[__i];\n";
					pT 2; ps "};\n";
			| _ ->
					Err.msg (Err.NYI "for S - functions the type of a signal value \
							must be primitive or a vector of floats \
							or doubles.")
		);
		pT 1; ps "};\n"
	) else (
	(* only input signals deserve work ... *)
	)

let mk_output_callback_simulink xs =
	let name = sig2str xs
	and s = sig2sgn xs in
	let typ = sig2valtyp xs in
	let sn = Sc.sgn2str s in
	let sv = Sc.sgn2valstr s in
	if not (xs.trs_sdecl.sig_clock.expr = v_true.expr)
	then Err.msg (Err.SfunctionClock name);
	match typ with
	| Null ->
			pT 2; ps "  *((boolean_T *)ssGetOutputPortSignal(simulinkS,";
			pi xs.simulinkno; ps ")) = (boolean_T)"; ps (test sn); ps ";\n"
	| Simple _ when typ = t_double || typ = t_float ->
			pT 2; ps "  *((real_T *)ssGetOutputPortRealSignal(simulinkS,";
			pi xs.simulinkno; ps ")) = (real_T)"; ps (test sv);
			ps ";\n"
	| Simple _ ->
			let typ = cc_builtintypedef (type2id typ) in
			pT 2; ps " *(("; ps typ; ps " *)ssGetOutputPortSignal(simulinkS,";
			pi xs.simulinkno; ps ")) = "; ps sv; ps ";\n"
	| Array(t, DimLen 1, DimLen _)
	| Array(t, DimLen _, DimLen 1)
	when t = t_float || t = t_double ->
			let i = xs.simulinkno in
			pT 1; ps "{\n";
			pT 2; ps "real_T *__y = ssGetOutputPortRealSignal(simulinkS,";
			pi i; ps");\n";
			pT 2; ps "int_T  __ny = ssGetOutputPortWidth(simulinkS,";
			pi i; ps");\n";
			pT 2; ps "for (__i = __ny; __i >=0; __i--)\n";
			pT 2; ps "{\n";
			pT 3; ps "__y[__i] = "; ps sv; ps ".a[__i];\n";
			pT 2; ps "};\n";
			pT 1; ps "};\n"
	| _ ->
			Err.msg (Err.NYI "for S - functions the type of a signal value \
					must be primitive or a vector of floats \
					or doubles.")

let gen_reactive_part_for_simulink conf_cid = function
	| None -> ()
	| Some sa ->
			boolgen := ExprWordSty;
			mk_rctengine_decls sa;
			
			let input_no, output_no = mk_simulinkno sa.sa_sigs in
			
			(* generate functions responsible to execute ONE REACTIVE STEP [INSTANT] *)
			let inputs = List.filter is_input_sig sa.sa_sigs in
			let feedthroughs_in, others_in =
				List.partition
					( fun xs -> List.mem (sig2sgn xs) sa.sa_obsv
					) inputs in
			
			print_comment "ps read input signals buffered by SIMULINK";
			ps "\nvoid scan_feedthrough_input_signal (SimStruct *simulinkS)\n{\n";
			List.iter scan_simulink_input feedthroughs_in;
			ps "}\n";
			ps "\nvoid scan_update_input_signal (SimStruct *simulinkS)\n{\n";
			List.iter scan_simulink_input others_in;
			ps "}\n";
			print_comment "copy pending input signals to the reactive machine";
			ps "\nvoid cp_feedthrough_input_signal (boolean_T has_new)\n{\n";
			List.iter cp_sim_input feedthroughs_in;
			ps "}\n";
			ps "\nvoid cp_update_input_signal (boolean_T has_new)\n{\n";
			List.iter cp_sim_input others_in;
			ps "}\n";
			
			let outputs = List.filter is_output_sig sa.sa_sigs in
			let feedthroughs_out, others_out =
				List.partition
					( fun xs -> List.mem (sig2sgn xs) sa.sa_obsv
					) outputs in
			let eqn_out, eqn_upd =
				List.partition
					( fun d -> List.exists (fun s -> d.sc_sgn = s) sa.sa_obsv
					) sa.sa_eqns in
			print_comment "function to generate outputs [of 1 INSTANT]";
			ps (runtime_engine Decl "output");
			pT 1; ps "exception = setjmp( exception_env );\n";
			pT 1; ps "if (exception != 0)\n";
			pT 1; ps "{\n";
			pT 2; ps "static char __msg[256];\n";
			pT 2; ps "sprintf(__msg,\"synERJY exception: %d\",exception);\n";
			pT 2; ps "ssSetErrorStatus(simulinkS,__msg);\n";
			pT 2; ps "return exception;\n";
			pT 1; ps "};\n\n";
			wire_reset ();
			pT 1; ps "scan_feedthrough_input_signal (simulinkS);\n";
			pT 1; ps "cp_feedthrough_input_signal (True);\n\n";
			
			print_comment "signal equations";
			List.iter mk_eqn_sgn eqn_out;
			print_comment "output signals";
			List.iter mk_output_callback_simulink feedthroughs_out;
			ps "}\n";
			
			print_comment "function to update the state [1 INSTANT]";
			ps (runtime_engine Decl "update");
			pT 1; ps "exception = setjmp( exception_env );\n";
			pT 1; ps "if (exception != 0)\n";
			pT 1; ps "{\n";
			pT 2; ps "static char __msg[256];\n";
			pT 2; ps "sprintf(__msg,\"synERJY exception: %d\",exception);\n";
			pT 2; ps "ssSetErrorStatus(simulinkS,__msg);\n";
			pT 2; ps "return exception;\n";
			pT 1; ps "};\n\n";
			pT 1; ps "scan_update_input_signal (simulinkS);\n";
			pT 1; ps "cp_update_input_signal (True);\n\n";
			print_comment "signal equations";
			List.iter mk_eqn_sgn eqn_upd;
			print_comment "output signals";
			List.iter mk_output_callback_simulink others_out;
			print_comment "memory equations";
			List.iter mk_eqn_sgn sa.sa_mems;
			print_comment "copy new mem vals to mem & set alpha";
			mk_copy_mem sa;
			ps "}\n";
			
			(* generate function responsible to INITIALIZE the RUNTIME ENGINE *)
			print_comment "initialize the runtime engine";
			ps (runtime_engine Decl "init");
			pT 1; ps "target_init();\n";
			pT 1; ps "SE_EXCEPTION_INIT\n";
			ps "/* clear memory, set alpha */\n";
			rctengine_reset 1 sa;
			ps "/* eqns for external signals always false */\n";
			List.iter mk_eqn_sgn_ext_ff sa.sa_eqns;
			
			let conf_str = c2str (type2id se.conf_class)
			and (t, _, v) = field2literal conf_cid id_timing in
			let v = int64_2_int (some v "mk_c:int64") in
			if t != t_time then Err.intern "mk_c:timing";
			if ac.ac_gc = [] then (
			(* no memory management *)
			) else (
				ps "\n/* initialization of memory management */\n";
				pT 1; ps "{ int alloc__length[] = {";
				lst_sep2ps (fun t -> ps "sizeof(tat_"; ps (cc_type2str t); ps ")")
					"," ac.ac_gc;
				ps ",0};\n";
				pT 2; ps "gc_init(&conf_obj,sizeof(tat_"; ps conf_str;
				ps "),alloc__length);\n";
				pT 1; ps "}\n"
			);
			ps "\n  /* initialization (if any) of static data */\n";
			List.iter cc_static_init ac.ac_classes;
			pT 1; ps "i_"; ps conf_str; ps " (&conf_obj);\n";
			
			pT 1; ps "f_"; ps conf_str; ps "_"; ps conf_str; ps "_0(&conf_obj);\n";
			ps "\n/* initialization of signal declarations */\n";
			List.iter (mk_sig_init 2) sa.sa_sigs;
			ps "\n/* initialization of internal declarations */\n";
			List.iter (mk_init_int_sig 2) sa.sa_dcls;
			pT 1; ps "}\n";
			
			print_comment "SIMULINK GLUE CODE";
			
			print_comment "check the parameters";
			mk_parameters();
			if !parameters <> [] then (
				ps "#define MDL_CHECK_PARAMETERS\n";
				ps "static void mdlCheckParameters(SimStruct *simulinkS)\n{\n";
				List.iter check_parameter_simulink !parameters;
				ps "}\n"
			);
			
			print_comment "declaration of the interface";
			ps "static void mdlInitializeSizes (SimStruct *simulinkS)\n{\n";
			if !parameters <> [] then (
				pT 1; ps "  ssSetNumSFcnParams(simulinkS, ";
				pi (List.length !parameters); ps ");\n";
				pT 1; ps " if (ssGetNumSFcnParams(simulinkS) \
				== ssGetSFcnParamsCount(simulinkS))\n";
				pT 1; ps "{\n";
				pT 2; ps "mdlCheckParameters(simulinkS);\n";
				pT 2; ps "if (ssGetErrorStatus(simulinkS) != NULL)\n";
				pT 2; ps "{\n";
				pT 3; ps "return;\n";
				pT 2; ps "}\n";
				pT 1; ps "}\n";
				pT 1; ps "else\n";
				pT 1; ps "{\n";
				pT 2; ps "return;\n";
				pT 1; ps "};\n";
			);
			pT 1; ps "ssSetNumContStates(simulinkS, 0);\n";
			pT 1; ps "ssSetNumDiscStates(simulinkS, 0);\n";
			pT 1; ps "if (!ssSetNumInputPorts(simulinkS, "; pi input_no;
			ps ")) return;\n";
			if inputs <> [] then (
				List.iter
					( fun xs ->
								let i = xs.simulinkno in
								( match sig2valtyp xs with
									| Simple _ ->
											pT 1; ps "ssSetInputPortWidth(simulinkS, "; pi i; ps ", 1);\n";
									| Array(_, DimLen 1, DimLen n)
									| Array(_, DimLen n, DimLen 1) ->
											pT 1; ps "ssSetInputPortWidth(simulinkS, "; pi i; ps ", ";
											pi n; ps ");\n";
									| _ ->
											Err.msg(Err.SimulinkInputType(type2id(sig2ro xs).trs_type, sig2id xs))
								);
								pT 1; ps "ssSetInputPortDataType(simulinkS,"; pi i; ps ",";
								let xt = sig2valtyp xs in
								let xc =
									match xt with
									| Simple _ -> type2id xt
									| Array(t, _, _) -> type2id t
									| _ -> Err.intern "Sfun:input type" in
								let (_, _, slt, _, _) = builtin_info xc in
								ps slt;
								ps ");\n";
								pT 1; ps "ssSetInputPortDirectFeedThrough(simulinkS,"; pi i;
								if List.mem xs feedthroughs_in
								then ps ",1);\n"
								else ps ",0);\n"
					) inputs
			) else (
			(* no input signals / ports, nothing to do *)
			);
			pT 1; ps "if (!ssSetNumOutputPorts(simulinkS, "; pi output_no;
			ps ")) return;\n";
			if outputs <> [] then (
				List.iter
					( fun xs ->
								let i = xs.simulinkno in
								( match sig2valtyp xs with
									| Simple _ ->
											pT 1; ps "ssSetOutputPortWidth(simulinkS, "; pi i;
											ps ", 1);\n";
									| Array(_, DimLen 1, DimLen n)
									| Array(_, DimLen n, DimLen 1) ->
											pT 1; ps "ssSetOutputPortWidth(simulinkS, ";
											pi i; ps ", "; pi n; ps ");\n";
									| _ ->
											Err.msg(Err.SimulinkOutputType(type2id(sig2ro xs).trs_type, sig2id xs))
								);
								pT 1; ps "ssSetOutputPortDataType(simulinkS,"; pi i; ps ",";
								let xt = sig2valtyp xs in
								let xc =
									match xt with
									| Simple _ -> type2id xt
									| Array(t, _, _) -> type2id t
									| _ -> Err.intern "Sfun:output type" in
								let (_, _, slt, _, _) = builtin_info xc in
								ps slt;
								ps ");\n"
					) outputs
			) else (
			(* no output signals / ports, nothing to do *)
			);
			let sample_time =
				if v = 0
				then "-1"
				else string_of_float ((float_of_int v) *.0.000001)
			in
			pT 1; ps "ssSetNumSampleTimes(simulinkS, 1);\n";
			pT 1; ps "ssSetNumRWork(simulinkS, 0);\n";
			pT 1; ps "ssSetNumIWork(simulinkS, 0);\n";
			pT 1; ps "ssSetNumPWork(simulinkS, 1);\n";
			pT 1; ps "ssSetNumModes(simulinkS, 0);\n";
			pT 1; ps "ssSetNumNonsampledZCs(simulinkS, 0);\n";
			pT 1; ps "ssSetOptions(simulinkS,0);\n";
			ps "}\n\n";
			
			ps "static void mdlInitializeSampleTimes(SimStruct *simulinkS)\n{\n";
			pT 1; ps "ssSetSampleTime(simulinkS, 0, "; ps sample_time; ps");\n";
			pT 1; ps "ssSetOffsetTime(simulinkS, 0, 0.0);\n";
			ps "}\n";
			if !parameters <> [] then (
				print_comment "set size of parameters";
				ps "#define MDL_SET_WORK_WIDTHS\n";
				ps "static void mdlSetWorkWidths(SimStruct *simulinkS)\n{\n";
				pT 1; ps "ssSetNumRunTimeParams(simulinkS, ";
				pi (List.length !parameters); ps ");\n";
				pT 1; ps "if (ssGetErrorStatus(simulinkS) != NULL) return;\n";
				List.iter set_parameter_size_simulink !parameters;
				ps "}\n";
				
				print_comment "process runtime parameters";
				ps "#define MDL_PROCESS_PARAMETERS\n";
				ps "static void mdlProcessParameters(SimStruct *simulinkS)\n{\n";
				List.iter
					( fun (no, _) ->
								pT 1; ps "ssUpdateDlgParamAsRunTimeParam(simulinkS,";
								pi no; ps ");\n";
					) !parameters;
				ps "}\n";
				
				print_comment "start-up routine";
				ps "#define MDL_START\n";
				ps "static void mdlStart(SimStruct *simulinkS)\n{\n";
				pT 1; ps "mdlCheckParameters(simulinkS);\n";
				pT 1; ps "mdlProcessParameters(simulinkS);\n";
				ps "}\n";
			);
			
			print_comment "initialisation";
			ps "#define MDL_INITIALIZE_CONDITIONS\n\
			static void mdlInitializeConditions(SimStruct * simulinkS)\n {\n";
			ps (runtime_engine Appl "init");
			List.iter
				( fun (_, dcl) ->
							let typ = dcl.signature.rt in
							match typ with
							| Simple _ ->
									pT 1; ps "(("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps " = *(("; ps (cc_typedef typ);
									ps " *)((ssGetRunTimeParamInfo(simulinkS,0))->data));\n"
							| Array(t, DimLen 1, DimLen n)
							| Array(t, DimLen n, DimLen 1) ->
									pT 1; ps "  for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
									pT 1; ps "{\n";
									pT 2; ps "((("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ").a[__i]";
									ps " = (("; ps (cc_typedef t);
									ps " *)((ssGetRunTimeParamInfo(simulinkS,0))->data))";
									ps "[__i]"; ps";\n";
									pT 1; ps "}\n"
							| _ -> Err.msg (Err.SimulinkParamType typ)
				) !parameters;
			ps "}\n";
			print_comment "generating output";
			ps "static void mdlOutputs(SimStruct *simulinkS, int_T tid)\n{\n";
			pT 1; ps (runtime_engine Appl "output");
			ps "}\n";
			print_comment "updating the state";
			ps "#define MDL_UPDATE\n";
			ps "static void mdlUpdate(SimStruct *simulinkS, int_T tid)\n{\n";
			pT 1; ps (runtime_engine Appl "update");
			ps "}\n";
			print_comment "termination";
			ps "static void mdlTerminate(SimStruct *simulinkS)\n{\n";
			pT 1; ps "UNUSED_ARG(simulinkS);\n";
			ps "}\n";
			print_comment "MEX or code generation?";
			ps "#ifdef MATLAB_MEX_FILE\n\
#include \"simulink.c\"\n\
#else\n\
#include \"cg_sfun.h\"\n\
#endif\n"

(* =========================================================================
generate data definitions and actions for the reactive part of the appl.
for Scicos
========================================================================= *)
let scan_scicos_input xs =
	if is_input_sig xs then (
		let typ = sig2valtyp xs in
		let s = sig2sgn xs in
		let sn = Sc.sgn2str s in
		let sv = Sc.sgn2valstr s
		and name = sig2str xs in
		if not (xs.trs_sdecl.sig_clock.expr = v_true.expr)
		then Err.msg (Err.SfunctionClock name);
		pT 1; ps (sn^"__n"); ps " = 1;\n"; (* always present *)
		( match typ with
			| Null ->
					pT 1; ps sn; ps "__n = *((se_Bool *)block->inptr[";
					pi xs.simulinkno; ps "]);\n"
			| Simple _ when typ = t_double ->
					pT 1; ps sv; ps "__n = *((se_Double *)block->inptr[";
					pi xs.simulinkno; ps "]);\n"
			| Simple _ ->
					let cid = type2id typ in
					pT 1; ps sv; ps "__n = *("; ps (cc_builtintypedef cid);
					ps " *)block->inptr[";
					pi xs.simulinkno; ps "]);\n"
			| Array(t, DimLen 1, DimLen n)
			| Array(t, DimLen n, DimLen 1) when t = t_float || t = t_double ->
					let cid = type2id t in
					pT 1; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
					pT 1; ps "{\n";
					pT 2; ps sv; ps "__n.a[__i] = *("; ps (cc_builtintypedef cid);
					ps "*)block->inptr["; pi xs.simulinkno; ps "][__i];\n";
					pT 1; ps "};\n";
			| _ -> Err.msg (Err.NYI "for S - functions the type of a signal value \
							must be primitive or a vector of floats \
							or doubles.")
		);
	) else (
	(* only input signals deserve work ... *)
	)

let mk_output_callback_scicos xs =
	let name = sig2str xs
	and s = sig2sgn xs in
	let typ = sig2valtyp xs in
	let sn = Sc.sgn2str s in
	let sv = Sc.sgn2valstr s in
	if not (xs.trs_sdecl.sig_clock.expr = v_true.expr)
	then Err.msg (Err.SfunctionClock name);
	( match typ with
		| Null ->
				pT 1; ps "  *((se_Double *)block->outptr["; pi xs.simulinkno;
				ps "]) = (se_Double)"; ps (test sn); ps ";\n"
		| Simple _ when typ = t_double || typ = t_float ->
				pT 1; ps "  *((se_Double *)block->outptr[";
				pi xs.simulinkno; ps "]) = (se_Double)"; ps (test sv);
				ps ";\n"
		| Simple _ ->
				let typ = cc_builtintypedef (type2id typ) in
				pT 1; ps "  *(("; ps typ; ps " *)block-outptr[";
				pi xs.simulinkno; ps "]) = "; ps sv; ps ";\n"
		| Array(t, DimLen 1, DimLen _)
		| Array(t, DimLen _, DimLen 1) when t = t_float || t = t_double ->
				let i = xs.simulinkno in
				pT 1; ps "{ se_Double *__y = block->outptr["; pi i; ps"];\n";
				pT 2; ps "int  __ny  = block->outsz["; pi i; ps"];\n";
				pT 2; ps "for (__i = __ny; __i >=0; __i--)\n";
				pT 2; ps "{\n";
				pT 3; ps "__y[__i] = "; ps sv; ps ".a[__i];\n";
				pT 2; ps "};\n";
				pT 1; ps "};\n"
		| _ ->
				Err.msg (Err.NYI "for scicos blocks, the type of a signal value \
						must be primitive or a vector of floats \
						or doubles.")
	)

let gen_reactive_part_for_scicos conf_cid = function
	| None -> ()
	| Some sa ->
			boolgen := ExprWordSty;
			mk_rctengine_decls sa;
			
			let _, _ = mk_simulinkno sa.sa_sigs in
			
			mk_parameters();
			(* generate functions responsible to execute ONE REACTIVE STEP [INSTANT] *)
			let inputs = List.filter is_input_sig sa.sa_sigs in
			pn(); print_comment "read input signals buffered by Scicos";
			ps "\nvoid scan_input_signal (scicos_block *block)\n{\n";
			List.iter scan_scicos_input inputs;
			ps "}\n";
			print_comment "copy pending input signals to the reactive machine";
			ps "\nvoid cp_input_signal (se_Bool has_new)\n{\n";
			List.iter cp_sim_input inputs;
			ps "}\n";
			
			let outputs = List.filter is_output_sig sa.sa_sigs in
			print_comment "function to generate outputs [of 1 INSTANT]";
			ps "se_Time realtm, deltat;\n\n";
			ps (runtime_engine Decl "instant");
			pT 1; ps "se_Time lasttm;\n\n";
			pT 1; ps "exception = setjmp( exception_env );\n";
			pT 1; ps "if (exception != 0)\n";
			pT 1; ps "{";
			pT 2; ps "sciprint(\"synERJY HAS RAISED AN EXCEPTION\");\n";
			pT 2; ps "sciprint(\"the exception raise  is '%d'\",exception);\n";
			pT 2; ps "set_block_error(-3);\n";
			pT 2; ps "return exception;\n";
			pT 1; ps "};\n";
			wire_reset ();
			let size = ref 0 in
			List.iter
				( fun (count, dcl) ->
							let typ = dcl.signature.rt in
							match typ with
							| Simple _ ->
									pT 1; ps "(("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps " = (("; ps (cc_typedef typ);
									ps " )block->rpar["; pi !size; ps "]);\n";
									size := !size + 1
							| Array(t, DimLen 1, DimLen n)
							| Array(t, DimLen n, DimLen 1) ->
									pT 1; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
									pT 1; ps "{\n";
									pT 2; ps "((("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ").a[__i]";
									ps " = (("; ps (cc_typedef t);
									ps " )block->rpar["; pi !size; ps "+__i]);\n";
									pT 1; ps "}\n";
									size := !size + n
							| Array(t, DimLen n1, DimLen n2) ->
									pT 1; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
									pT 1; ps "{\n";
									pT 2; ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
									pT 2; ps "{\n";
									pT 3; ps "((("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ").a[__i][__j]";
									ps " = (("; ps (cc_typedef t);
									ps " )block->rpar["; pi !size; ps "+__i*"; pi n2; ps "+__j]);\n";
									pT 2; ps "}\n";
									pT 1; ps "}\n";
									size := !size + n1 * n2
							| _ -> Err.msg (Err.SimulinkParamType typ)
				) !parameters;
			pT 1; ps "scan_input_signal (block);\n";
			pT 1; ps "cp_input_signal (True);\n";
			
			(* check, whether the last instant did exceed the interval defined by
			the timing constant. If not, do a suitable sleep *)
			let (t, s, v) = field2literal conf_cid id_timing in
			let v = int64_2_int (some v "grpiC:tval") in
			if t != t_time then Err.intern "grpiC:time";
			pT 1; ps "lasttm = realtm;\n";
			pT 1; ps "realtm = get_scicos_time();\n";
			if v = 0 then (
			(* dense instant execs, no violation, no sleep  *)
			) else (
				let ie = get_sysprop se.conf_class id_exc_instant in
				pT 1; ps "deltat = realtm - lasttm;\n";
				pT 1; ps "if (deltat >= "; pi v; ps ") mkExc("; pi ie; ps ",0);\n";
			);
			pT 1; ps "if (deltat <= (se_Time)0) { deltat = realtm; };\n";
			
			print_comment "signal equations";
			List.iter mk_eqn_sgn sa.sa_eqns;
			print_comment "output signals";
			List.iter mk_output_callback_scicos outputs;
			print_comment "memory equations";
			List.iter mk_eqn_sgn sa.sa_mems;
			print_comment "copy new mem vals to mem & set alpha";
			mk_copy_mem sa;
			ps "}\n";
			
			(* generate function responsible to INITIALIZE the RUNTIME ENGINE *)
			print_comment "initialize the runtime engine";
			ps (runtime_engine Decl "init");
			pT 1; ps "target_init();\n";
			pT 1; ps "SE_EXCEPTION_INIT\n";
			ps "/* clear memory, set alpha */\n";
			rctengine_reset 1 sa;
			ps "/* eqns for external signals always false */\n";
			List.iter mk_eqn_sgn_ext_ff sa.sa_eqns;
			
			let conf_str = c2str (type2id se.conf_class)
			and (t, _, v) = field2literal conf_cid id_timing in
			if t != t_time then Err.intern "mk_c:timing";
			if ac.ac_gc = [] then (
			(* no memory management *)
			) else (
				ps "\n/* initialization of memory management */\n";
				pT 1; ps "{ int alloc__length[] = {";
				lst_sep2ps (fun t -> ps "sizeof(tat_"; ps (cc_type2str t); ps ")")
					"," ac.ac_gc;
				ps ",0};\n";
				pT 2; ps "gc_init(&conf_obj,sizeof(tat_"; ps conf_str;
				ps "),alloc__length);\n";
				pT 1; ps "}\n"
			);
			ps "\n/* initialization (if any) of static data */\n";
			List.iter cc_static_init ac.ac_classes;
			pT 1; ps "i_"; ps conf_str; ps " (&conf_obj);\n";
			
			pT 1; ps "f_"; ps conf_str; ps "_"; ps conf_str; ps "_0(&conf_obj);\n";
			ps "\n/* initialization of signal declarations */\n";
			List.iter (mk_sig_init 2) sa.sa_sigs;
			ps "\n/* initialization of internal declarations */\n";
			List.iter (mk_init_int_sig 2) sa.sa_dcls;
			let size = ref 0 in
			List.iter
				( fun (count, dcl) ->
							let typ = dcl.signature.rt in
							match typ with
							| Simple _ ->
									pT 1; ps "block->rpar["; pi !size; ps "] = ((";
									ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ";\n";
									size := !size + 1
							| Array(t, DimLen 1, DimLen n)
							| Array(t, DimLen n, DimLen 1) ->
									pT 1; ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--)\n";
									pT 1; ps "{\n";
									pT 2; ps "block->rpar["; pi !size;
									ps "+__i] = \n           ";
									ps "((("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ").a[__i];\n";
									pT 1; ps "};\n";
									size := !size + n
							| Array(t, DimLen n1, DimLen n2) ->
									pT 1; ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--)\n";
									pT 1; ps "{\n";
									pT 2; ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--)\n";
									pT 2; ps "{\n";
									pT 3; ps "block->rpar["; pi !size; ps "+__i*";
									pi n2; ps "+__j] = \n           ";
									ps "((("; ps (cc_typedef se.conf_class); ps ") &";
									ps (mf2str id_conf); ps ")->"; ps (mf2str dcl.name);
									ps ").a[__i][__j];\n";
									pT 2; ps "}\n";
									pT 1; ps "  };\n";
									size := !size + n1 * n2
							| _ -> Err.msg (Err.SimulinkParamType typ)
				) !parameters;
			ps "}\n"
