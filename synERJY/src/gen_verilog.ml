open Ly
open Ast
open P
open Util
open Util_gen
open Util_print
open Gen_application

(* ==========================================================================
GENERATE Verilog Code for FPGAs
========================================================================== *)

module Verilog =

struct
	
	(* =========================================================================
	Value Types
	========================================================================= *)
	let class2range cid =
		if cid = id_bool then ""
		else if cid = id_byte then "[8:0]"
		else if cid = id_char then "[8:0]"
		else if cid = id_short then "[16:0]"
		else if cid = id_int then "[32:0]"
		else if cid = id_long then "[64:0]"
		else if cid = id_float then "[32:0]"
		else if cid = id_double then "[64:0]"
		
		else if cid = id_cuint7 then "[7:0]"
		else if cid = id_cuint15 then "[15:0]"
		else if cid = id_cuint31 then "[31:0]"
		else if cid = id_cuint63 then "[63:0]"
		else if cid = id_cint8 then "[8:0]"
		else if cid = id_cint16 then "[16:0]"
		else if cid = id_cint32 then "[32:0]"
		else if cid = id_cint64 then "[64:0]"
		else if cid = id_cuint8 then "[8:0]"
		else if cid = id_cuint16 then "[16:0]"
		else if cid = id_cuint32 then "[32:0]"
		else if cid = id_cuint64 then "[64:0]"
		
		else if cid = id_uint16 then "[16:0]"
		else if cid = id_uint32 then "[32:0]"
		else if cid = id_uint64 then "[64:0]"
		(*     else if cid = id_time    then "[64:0]"  *)
		else Err.msg (Err.NYI "type not supported in verilog code generation")
	
	let sig2str xs = match xs.trs_sname with
		| (Call(_, None), _):: cl -> calll2str "_" cl
		| _ -> assert false
	
	(* -------------------------------------------------------------------------
	Input and Output Signals for Modul Parameters
	------------------------------------------------------------------------- *)
	let input_sigs sa =
		List.fold_left
			( fun l xs ->
						if is_input_sig xs
						then (
							match sig2valtyp xs with
							| Null -> (sig2str xs):: l
							| _ -> let s = (sig2str xs) in
									s:: ("_"^s):: l
						)
						else l
			) [] sa.sa_sigs
	
	let output_sigs sa =
		List.fold_left
			( fun l xs ->
						if is_output_sig xs
						then (
							match sig2valtyp xs with
							| Null -> (sig2str xs):: l
							| _ -> let s = (sig2str xs) in
									s:: ("_"^s):: l
						)
						else l
			) [] sa.sa_sigs
	
	(* =========================================================================
	Input and Output Callbacks
	========================================================================= *)
	let mk_input_callback xs =
		if is_input_sig xs then (
			ps "\n\ninput   "; ps (sig2str xs); ps ";";
			ps "\nreg     "; print_sgn (sig2sgn xs); ps ";";
			( match sig2valtyp xs with
				| Null -> ()
				| Simple cid ->
						let range = class2range cid in
						ps "\ninput "; ps range; ps "  _";
						ps (sig2str xs); ps ";";
						ps "\nreg "; ps range; ps "    ";
						print_sgn (Sc.sgn2val(sig2sgn xs)); ps ";";
				| _ -> Err.intern "Verilog.mk_input_callback"
			);
			ps "\nalways @(posedge clck)";(*ps (sig2str xs);ps ")";*)
			ps "\nbegin";
			ps "\n  "; print_sgn (sig2sgn xs); ps " = ";
			ps (sig2str xs); ps ";";
			( match sig2valtyp xs with
				| Null -> ()
				| Simple cid ->
						ps "\n  if ("; ps (sig2str xs); ps ") ";
						ps "\n    "; print_sgn (Sc.sgn2val(sig2sgn xs));
						ps " = _"; ps (sig2str xs); ps ";";
				| _ -> Err.intern "Verilog.mk_input_callback"
			);
			ps "\nend\n"
		) else (
		)
	
	let mk_output_callback xs =
		if is_output_sig xs then (
			ps "\noutput  "; ps (sig2str xs); ps ";";
			ps "\nreg    "; ps (sig2str xs); ps "_r;";
			( match sig2valtyp xs with
				| Null -> ()
				| Simple cid ->
						let range = class2range cid in
						ps "\n\noutput "; ps range; ps "  _";
						ps (sig2str xs); ps ";";
						ps "\nreg "; ps range; ps "    ";
						print_sgn (Sc.sgn2val(sig2sgn xs)); ps ";";
				| _ -> Err.intern "Verilog.mk_output_callback"
			);
			
			ps "\nalways @(posedge "; print_sgn (sig2sgn xs); ps ")";
			ps "\nbegin";
			ps "\n  "; ps (sig2str xs); ps "_r = 1;";
			ps "\nend";
			
			ps "\nassign "; ps (sig2str xs);
			ps " = "; ps (sig2str xs); ps "_r;";
			( match sig2valtyp xs with
				| Null -> ()
				| Simple cid ->
						ps "\nassign _"; ps (sig2str xs);
						ps " = "; print_sgn (Sc.sgn2val(sig2sgn xs)); ps ";";
				| _ -> Err.intern "Verilog.mk_output_callback"
			);
		) else (
		)
	
	(* =========================================================================
	Variable Declarations
	========================================================================= *)
	let mk_ext_sign_decl xs =
		let err() = Err.intern "Verilog.mk_ext_sign_decl" in
		if is_input_sig xs || is_output_sig xs then (
		) else (
			( match sig2valtyp xs with
				| Null ->
						ps "\nwire    "; ps (sig2str xs);
				| Simple cid ->
						let range = class2range cid in
						ps "\nreg "; ps range; ps "    ";
						print_sgn (Sc.sgn2val(sig2sgn xs)); ps ";";
				| Array(t, DimLen 1, DimLen n)
				| Array(t, DimLen n, DimLen 1) ->
						( match t with
							| Simple cid ->
									let range = class2range cid in
									ps "\nreg "; ps range; ps "    ";
									print_sgn (Sc.sgn2val(sig2sgn xs));
									ps ("[0:"^i2s (n - 1)^"];");
									ps "\nreg    "; ps (sig2str xs); ps "_r;";
							| _ -> err();
						);
				| Array(t, DimLen n1, DimLen n2) ->
						( match t with
							| Simple cid ->
									let range = class2range cid in
									ps "\nreg "; ps range; ps "    ";
									print_sgn (Sc.sgn2val(sig2sgn xs));
									ps ("[0:"^i2s (n1 * n2 - 1)^"];");
							| _ -> err();
						);
				| _ -> Err.intern "Verilog.mk_ext_sign_decl";
			);
		)
	
	(* =========================================================================
	Wires
	========================================================================= *)
	let mk_wire d =
		match d.sc_act with
		| None -> ps "\nwire   "; ps (Sc.sgn2str d.sc_sgn); ps ";"
		| Some _ -> ()
	
	(* -------------------------------------------------------------------------
	Generation of reactive Expressions
	------------------------------------------------------------------------- *)
	let rec v_rctexpr = function
		| S x when x = Sc.beta -> v_rctexpr (NNot (S Sc.alpha))
		| S x -> Sc.sgn2str x
		| CC x -> Err.intern "v_rctexpr"
		| TT -> "1'b1"
		| FF -> "1'b0"
		| NNot x -> "! ("^(v_rctexpr x)^")"
		| AAnd el -> "("^(lst_sep2str v_rctexpr " & " el)^")"
		| OOr el -> "("^(lst_sep2str v_rctexpr " | " el)^")"
	
	(* -------------------------------------------------------------------------
	Generation of Expressions
	------------------------------------------------------------------------- *)
	let v_initval cid =
		try
			let (_, _, _, _, initval) = builtin_info cid in
			ps initval
		with Not_found ->
				ps "(pat_"; psc cid; ps ") 0"  (* TODO *)
	
	let v_typedef typ =
		cc_typedef_with_no_builtin_prefix typ "pat_"
	
	let v_constant (t, s, v) =
		if is_integral_typ t || t = t_time or
		le_type ~sub: t ~super: t_cint64 || le_type ~sub: t ~super: t_cuint64 then (
			Int64.to_string (some v "v_constant")
		) else if t = t_float || t = t_double then (
			s
		) else if t = t_bool then (
			if s ="true" then "1" else
			if s ="false" then "0" else
				Err.intern "v_constant"
		) else if t = t_string then (
			"\""^(String.escaped s)^"\""
		) else (
			Err.intern "v_constant"
		)
	
	let v_field context mf_typ mf =
		let mf_typ = some mf_typ "v_field" in
		let mf_typ = subst_map mf_typ cc.cc_v2t in
		let cid = type2id context.at_type in
		let prim = is_primitive_typ mf_typ in
		try
			let dcl = get_decl_after_tc cid mf (- 1) in
			(* ------- all cases are field accesses ------------------------- *)
			try
				if is_sensor_or_signal_typ mf_typ then
					mk_ctxt "Null" mf_typ
				else
					let can_exp = can_expand dcl in
					let fld = if dcl.scope = Class then (
							match native2name dcl with
							| Some s -> s
							| None -> "sta_"^(c2str dcl.origin)^"_"^
									(mf2str mf)
						) else (
							context.at_name^"->"^(mf2str mf)
						)
					in
					let fld = if can_exp && (not prim)
						then "(&("^fld^"))"
						else fld in
					{ at_name = fld;
						at_type = mf_typ;
						at_exp = can_exp;
					}
			with _ -> Err.intern "v_field"
		with _ ->
		(* ------- formal or local parameter -------------------------------- *)
				mk_ctxt (mf2str mf) mf_typ
	
	let rec v_expr expr =
		(v_gen expr).at_name
	
	and v_gen expr =
		v_call cc.cc_this_ctxt expr
	
	and v_call context expr =
		let some x = some x "v_call" in
		match expr.expr with
		| Dot({ expr = DeltaT },{ expr = Call(op, Some []) }) when op = id_op_value ->
				mk_ctxt "deltat" t_time
		
		| Dot({ expr = DotDot(sv, xs, 0) },{ expr = Call(op, Some [ap]) }) when op = id_op_get ->
				let typ = some expr.etyp in
				let sv = Sc.sgn2str sv in
				let tmpvar1 = alloc_tmpvar () in
				let name =
					"(__b="^sv^"__buf.pt-"^v_expr ap^",(("^v_typedef typ^
					"*)&((pat_array1)&"^sv^ "__buf.b)->a)[__b<0?((Int)((pat_array1)&"
					^sv^ "__buf.b)->len)+__b:__b])"
				in
				free_tmpvar tmpvar1;
				{ at_name = name;
					at_type = typ;
					at_exp = true;
				}
		
		| Dot(e1, e2) ->
				v_call (v_call context e1) e2
		
		| Call(op, Some [p]) when op = id_op_equal || op = id_op_not_equal ->
				let lhs = context
				and rhs = v_gen p
				and opr = if op = id_op_equal then " == " else " != " in
				let cmp = if (Util_gen.is_basic_type_equal lhs.at_type rhs.at_type) then (
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
						"((pat_object)"^lhs.at_name^opr^"(pat_object)"^rhs.at_name^")"
					)
				in
				Util_gen.mk_ctxt cmp t_bool
		
		| Call(mf, None) ->
				v_field context expr.etyp mf
		
		| Call(op, Some [ap]) when op = id_op_get ->
				( match context.at_type, ap.expr with
					| Array(t, DimLen 1, d), Call(mf, None)
					| Array(t, d, DimLen 1), Call(mf, None) when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
							let name = context.at_name^"["^v_expr ap^"]" in
							mk_ctxt name (some expr.etyp)
					| Array(t, DimLen 1, d), Literal(_, v, _)
					| Array(t, d, DimLen 1), Literal(_, v, _) when dim2value d > 0 ->
							if 0 <= s2i v && s2i v <= (dim2value d) - 1 then (
								let name = context.at_name^"["^v^"]"
								in
								mk_ctxt name (some expr.etyp)
							) else (
								Err.msg(Err.Index(type2id cc.cc_typ, ap.elbl))
							)
					| Array(t, DimLen 1, d), Dot({ expr = Call(mf, None) }, _)
					| Array(t, d, DimLen 1), Dot({ expr = Call(mf, None) }, _)
					when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
							let name = context.at_name^"["^v_expr ap^"]" in
							mk_ctxt name (some expr.etyp)
					| Array(t, DimLen 1, _), _
					| Array(t, _, DimLen 1), _ ->
							Err.msg(Err.VerilogIndex(type2id cc.cc_typ, ap.elbl))
					| _ -> Err.intern "v_call:get:1"
				)
		
		| Call(op, Some[ap1; ap2]) when op = id_op_get ->
				( match context.at_type, ap1.expr, ap2.expr with
					| Array(t, DimLen 1, _), _, Call(mf, None) when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
							let name = context.at_name^"["^v_expr ap2^"]" in
							mk_ctxt name (some expr.etyp)
					| Array(t, _, DimLen 1), Call(mf, None), _ when mf = id_op__i || mf = id_op__j || mf = id_op__k ->
							let name = context.at_name^"["^v_expr ap1^"]" in
							mk_ctxt name (some expr.etyp)
					| Array(t, _, d2), Call(mf1, None), Call(mf2, None)
					when (mf1 = id_op__i || mf1 = id_op__j || mf1 = id_op__k)
					&& (mf2 = id_op__i || mf2 = id_op__j || mf2 = id_op__k) ->
							let vtyp = v_typedef t
							and n2 = dim2value d2 in
							let name = "(("^vtyp^"*)&((pat_array2)"^
								"(pat_Object)"^context.at_name^
								")->a)["^v_expr ap1^"*"^i2s n2^"+"^v_expr ap2^"]"
							in
							mk_ctxt name (some expr.etyp)
					| Array(t, DimLen 1, d) as ct, _, Literal(_, v, _) when dim2value d > 0 ->
							let vtyp = v_typedef t in
							let n = dim2value d in
							let i = s2i v in
							if 0 <= i && i <= n - 1 then (
								let name = "(("^vtyp^"*)&((pat_array1)"^
									"(pat_Object)"^context.at_name^ ")->a)["^v^"]"
								in
								mk_ctxt name (some expr.etyp)
							) else (
								Err.msg(Err.Index(type2id ct, ap1.elbl))
							)
					| Array(t, d, DimLen 1) as ct, Literal(_, v, _), _ when dim2value d > 0 ->
							let vtyp = v_typedef t in
							let n = dim2value d in
							let i = s2i v in
							if 0 <= i && i <= n - 1 then (
								let name = "(("^vtyp^"*)&((pat_array1)"^
									"(pat_Object)"^context.at_name^ ")->a)["^v^"]"
								in
								mk_ctxt name (some expr.etyp)
							) else (
								Err.msg(Err.Index(type2id ct, ap1.elbl))
							)
					| Array(t, d1, d2) as ct, Literal(_, v1, _), Literal(_, v2, _)
					when dim2value d1 > 0 && dim2value d2 > 0 ->
							let vtyp = v_typedef t in
							let n1 = dim2value d1
							and n2 = dim2value d2 in
							let i1 = s2i v1 and i2 = s2i v2 in
							if 0 <= i1 && i1 <= n1 - 1 then (
								if 0 <= i2 && i2 <= n2 - 1 then (
									let name = "(("^vtyp^"*)&((pat_array2)"^
										"(pat_Object)"^context.at_name^
										")->a)["^v1^"*"^i2s n2^"+"^v2^"]"
									in
									mk_ctxt name (some expr.etyp)
								) else (
									Err.msg(Err.Index(type2id ct, ap2.elbl))
								)
							) else (
								Err.msg(Err.Index(type2id ct, ap1.elbl))
							)
					| Array(t, DimLen 1, _), _, _ ->
							let vtyp = v_typedef t in
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpptr = tmpvar2ptr (Some tmpvar1) in
							let tmpint = tmpvar2int tmpvar2 in
							let bds = i2s(get_sysprop se.conf_class id_exc_bounds) in
							let nll = i2s(get_sysprop se.conf_class id_exc_nullptr) in
							let lbl = Ly.lbl2str expr.elbl in
							let name =
								"("^tmpptr^"=(pat_Object)"^context.at_name^","^
								tmpint^"="^(v_expr ap2)^", "^
								tmpptr^"==Null && mkExc("^ nll^","^lbl^"), "^
								"("^tmpint^"<0||"^tmpint^">=((pat_array1)"^
								tmpptr^")->len) && mkExc("^bds^","^lbl^"), "^
								"("^vtyp^"*)&((pat_array1)"^tmpptr^
								")->a)["^tmpint^"]" in
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt name (some expr.etyp)
					| Array(t, _, DimLen 1), _, _ ->
							let vtyp = v_typedef t in
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpptr = tmpvar2ptr (Some tmpvar1) in
							let tmpint = tmpvar2int tmpvar2 in
							let bds = i2s(get_sysprop se.conf_class id_exc_bounds) in
							let nll = i2s(get_sysprop se.conf_class id_exc_nullptr) in
							let lbl = Ly.lbl2str expr.elbl in
							let name =
								"("^tmpptr^"=(pat_Object)"^context.at_name^","^
								tmpint^"="^(v_expr ap1)^", "^
								tmpptr^"==Null && mkExc("^ nll^","^lbl^"), "^
								"("^tmpint^"<0||"^tmpint^">=((pat_array1)"^
								tmpptr^")->len) && mkExc("^bds^","^lbl^"), "^
								"("^vtyp^"*)&((pat_array1)"^tmpptr^
								")->a)["^tmpint^"]" in
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt name (some expr.etyp)
					| Array(t, _, _), _, _ ->
							let vtyp = v_typedef t in
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpvar3 = alloc_tmpvar () in
							let tmpptr = tmpvar2ptr (Some tmpvar1) in
							let tmpi1 = tmpvar2int tmpvar2 in
							let tmpi2 = tmpvar2int tmpvar3 in
							let bds = i2s (get_sysprop se.conf_class id_exc_bounds) in
							let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
							let lbl = Ly.lbl2str expr.elbl in
							let name =
								"("^tmpptr^"=(pat_Object)"^context.at_name^","^
								tmpi1^"="^ (v_expr ap1)^", "^
								tmpi2^"="^ (v_expr ap2)^", "^
								tmpptr^"==Null && mkExc("^ nll^","^lbl^"), "^
								"("^tmpi1^"<0||"^tmpi1^">=(Int)((pat_array2)"^
								tmpptr^")->len1) && mkExc("^bds^","^lbl^"), "^
								"("^tmpi2^"<0||"^tmpi2^">=(Int)((pat_array2)"^
								tmpptr^")->len2) && mkExc("^bds^","^lbl^"), "^
								"("^vtyp^"*)&((pat_array2)"^tmpptr^
								")->a)["^tmpi1^"*(Int)(((pat_array2)"^tmpptr^
								")->len2)+"^ tmpi2^"]" in
							free_tmpvar tmpvar3;
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt name (some expr.etyp)
					
					| _ -> Err.intern "v_call:get:1"
				)
		
		| Call(op, Some apl)
		when (is_matrix_typ context.at_type || is_matrix_typ (some expr.etyp))
		&& is_matrix_op op ->
				let op = if op = id_op_pointmult then id_op_mult else op
				(* A really BAD HACK; needed to deal with vector multiplication
				can be eliminated if overloading is implemented *)
				and ctyp = match some expr.etyp with
					| Array(t, _, _) -> t
					| _ -> Err.intern ("v_call:matrix_op"^Ly.mf2str op)
				in
				let cid = type2id ctyp in
				( match (apl, primop2str op) with
					| [ap], Some s ->
							let n = "("^context.at_name^s^v_expr ap^")" in
							mk_ctxt n ctyp
					| [], Some s ->
							let n = "("^s^context.at_name^")" in
							mk_ctxt n ctyp
					| _ ->
							let n = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str op)^
								"("^context.at_name^
								(if apl = [] then "" else ",")^
								(lst_sep2str v_expr "," apl)^")" in
							mk_ctxt n ctyp
				)
		
		| Call(op, Some []) when op = id_op_length ->
				let tmpvar = alloc_tmpvar () in
				let tmpptr = tmpvar2ptr (Some tmpvar) in
				let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
				let lbl = Ly.lbl2str expr.elbl in
				let name =
					"("^tmpptr^"=(pat_Object)"^context.at_name^","^
					tmpptr^"==Null && mkExc("^nll^","^lbl^"), "^
					"(Int)((pat_array1)"^tmpptr^")->len)" in
				free_tmpvar tmpvar;
				mk_ctxt name (some expr.etyp)
		
		| Call(op, Some []) when op = id_op_rows ->
				let tmpvar = alloc_tmpvar () in
				let tmpptr = tmpvar2ptr (Some tmpvar) in
				let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
				let lbl = Ly.lbl2str expr.elbl in
				let name =
					"("^tmpptr^"=(pat_Object)"^context.at_name^","^
					tmpptr^"==Null && mkExc("^nll^","^lbl^"), "^
					"(Int)((pat_array2)"^tmpptr^")->len1)" in
				free_tmpvar tmpvar;
				mk_ctxt name (some expr.etyp)
		
		| Call(op, Some []) when op = id_op_cols ->
				let tmpvar = alloc_tmpvar () in
				let tmpptr = tmpvar2ptr (Some tmpvar) in
				let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
				let lbl = Ly.lbl2str expr.elbl in
				let name =
					"("^tmpptr^"=(pat_Object)"^context.at_name^","^
					tmpptr^"==Null && mkExc("^nll^","^lbl^"), "^
					"(Int)((pat_array2)"^tmpptr^")->len2)" in
				free_tmpvar tmpvar;
				mk_ctxt name (some expr.etyp)
		
		| Call(mf, Some apl) ->
				let vt = context.at_type in
				let (dcl, ctyp) =
					try ( get_decl_after_tc (type2id vt) mf (List.length apl), vt)
					with _ ->
							if (is_literal_type context.at_type) then (
								match apl with
								| [p] -> let ptyp = some p.etyp in
										( get_decl_after_tc (type2id ptyp) mf (List.length apl)
											, ptyp
										)
								| _ -> Err.intern ("v_call:invlit:"^mf2str mf)
							) else (
								Err.intern ("v_call:nolit:"^mf2str mf)
							)
				in
				let cid = type2id ctyp in
				if (is_method_or_constructor dcl) && not( is_rct_method dcl )
				then ()
				else Err.intern "v_call:nomthd";
				if (is_literal_type ctyp) then (
					if apl != [] then Err.intern "v_call:lits";
					Util_gen.mk_ctxt context.at_name (some expr.etyp)
				) else if ctyp = t_time then (
					let n = "f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^"("^context.at_name^
						(if apl = [] then "" else ",")^
						(lst_sep2str v_expr "," apl)^")" in
					mk_ctxt n (some expr.etyp)
				) else if (is_primitive_typ ctyp) then (
					( match (apl, Util_gen.primop2str mf) with
						| [ap], Some s ->
								let n = "("^context.at_name^s^v_expr ap^")" in
								Util_gen.mk_ctxt n (some expr.etyp)
						| [], Some s ->
								let n = "("^s^context.at_name^")" in
								Util_gen.mk_ctxt n (some expr.etyp)
						| _ ->
								let n =
									"f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^"("^context.at_name^
									(if apl = [] then "" else ",")^(lst_sep2str v_expr "," apl)^")"
								in
								mk_ctxt n (some expr.etyp)
					)
				) else (
					(* let len = List.length apl
					and c1 = "_"^(Ly.mf2str mf)^(apl2str apl)^"((pat_"
					and c2 = (if apl = [] then "" else ",")^
					(lst_sep2str v_expr "," apl)^")" in
					let suf imp this = c1^imp^")("^this^")"^c2 in *)
					let resulttyp = some expr.etyp in
					let name = ""(*dynamic_bind resulttyp expr.elbl context mf len suf*) in
					mk_ctxt name resulttyp
				)
		
		| ClassCall(cid, mf, None) ->
				let dcl = try
						let dcl = get_decl_after_tc cid mf (- 1) in
						if is_field dcl
						then dcl
						else Err.intern "v_call"
					with Not_found ->
							Err.intern "v_call"
				in
				(* ------- static fields ---------------------------------------- *)
				if dcl.scope = Class then
					let fld =
						match native2name dcl with
						| Some s -> s
						| None -> "sta_"^(Ly.c2str dcl.origin)^"_"^(Ly.mf2str mf)
					in
					let fld =
						if (can_expand dcl) && not(is_primitive_typ dcl.signature.rt)
						then "(&("^fld^"))"
						else fld
					in
					mk_ctxt fld (some expr.etyp)
				else
					Err.intern "v_call:static final"
		| ClassCall(cid, mf, Some apl) ->
				let dcl =
					try
						let dcl = get_decl_after_tc cid mf (List.length apl) in
						if is_field dcl
						then Err.intern "v_call"
						else dcl
					with Not_found ->
							Err.intern "v_call" in
				if (is_method_or_constructor dcl) && not( is_rct_method dcl ) then (
					let name =
						"f_"^(Ly.c2str cid)^"_"^(Ly.mf2str mf)^(apl2str apl)^" ("^(lst_sep2str v_expr "," apl)^")" in
					mk_ctxt name (some expr.etyp)
				) else (
					Err.intern "v_call"
				)
		| New(typ, apl) ->
				( match (typ, apl) with
					| (Array(t, DimLen 1, _),[e])
					| (Array(t, _, DimLen 1),[e]) ->
							let tmpvar = alloc_tmpvar () in
							let tmpstr = tmpvar2int tmpvar in
							let size = i2s (get_sysprop se.conf_class id_exc_arraysize) in
							let name =
								"(pat_array1)("^tmpstr^"="^(v_expr e)^", "^
								tmpstr^"<0 && mkExc("^size^","^Ly.lbl2str expr.elbl^"), "^
								"ARRAYM1("^tmpstr^",sizeof("^(v_typedef t)^")))"
							in
							free_tmpvar tmpvar;
							mk_ctxt name (some expr.etyp)
					| (Array(t, _, _),[e1; e2]) ->
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpstr1 = tmpvar2int tmpvar1 in
							let tmpstr2 = tmpvar2int tmpvar2 in
							let size = i2s (get_sysprop se.conf_class id_exc_arraysize) in
							let name =
								"(pat_array2)("^tmpstr1^"="^(v_expr e1)^", "^tmpstr2^"="^(v_expr e2)^", "^
								tmpstr1^"<0 && mkExc("^size^","^Ly.lbl2str expr.elbl^"), "^
								tmpstr2^"<0 && mkExc("^size^","^Ly.lbl2str expr.elbl^"), "^
								"ARRAYM2("^tmpstr1^","^tmpstr2^",sizeof("^(v_typedef t)^")))"
							in
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt name (some expr.etyp)
					| _ ->
							let cid = cc_type2str (subst_map typ cc.cc_v2t)
							and fid = Ly.c2str (type2id typ) in
							let name =
								"f_"^cid^"_"^fid^(apl2str apl)^" (i_"^cid^" (n_"^cid^"())"^
								(lst_sep2BstrB "," "" v_expr "," apl)^")"
							in
							mk_ctxt name typ
				)
		| Super(fm, apl) ->
				let len = List.length apl in
				let dcll = Hashtbl.find (i_ast (type2id cc.cc_typ)).symtab (fm, len) in
				let dcll =
					match dcll with
					| [] -> Err.intern "v_call:super"
					| _:: t -> t in
				let dcl = list_first_match (fun d -> not( is_abstract d )) dcll in
				let rec iter_supertype typ =
					let typ = get_supertype typ in
					if (type2id typ) = dcl.origin
					then typ
					else iter_supertype typ
				in
				let typ = cc_type2str (iter_supertype cc.cc_typ) in
				let name =
					"f_"^typ^"_"^(Ly.mf2str fm)^(apl2str apl)^
					" ("^(lst_sep2str v_expr "," apl)^")"
				in
				mk_ctxt name (some expr.etyp)
		
		| Cast(to_typ, e) ->
				let tmpvar = alloc_tmpvar () in
				let tmpptr = tmpvar2ptr (Some tmpvar) in
				let e = v_gen e in
				let t = tmpptr^"->objhead.cid" in
				let subl = Util_gen_c.subtypes (some expr.etyp) ac.ac_sub in
				let gen =
					if List.mem to_typ subl then ( (* down cast? *)
						let name =
							"("^tmpptr^"=(pat_Object)"^e.at_name^","^
							(Util_gen_c.dynamic_cast t subl)^" ? "^tmpptr^" : Null)" in
						mk_ctxt name (some expr.etyp)
					) else ( (* up cast or identity cast *)
						e
					)
				in
				free_tmpvar tmpvar;
				gen
		
		| This ->
				mk_ctxt (this_name ()) context.at_type
		
		| ThisConstr(apl) ->
				let typ = cc_type2str cc.cc_typ in
				let name =
					"f_"^typ^"_"^typ^apl2str apl^" ("^this_name()^
					(lst_sep2BstrB "," "" v_expr "," apl)^")"
				in
				mk_ctxt name Null
		
		| SuperConstr(apl) ->
				let super = get_supertype cc.cc_typ in
				let scid = type2id super in
				let super = cc_type2str super in
				let name =
					"f_"^super^"_"^Ly.c2str scid^apl2str apl^
					" ("^"(pat_"^super^") "^this_name()^
					(lst_sep2BstrB "," "" v_expr "," apl)^")"
				in
				mk_ctxt name Null
		
		| NullObj ->
				mk_ctxt "Null" Null
		
		| IncrDecr(f, o) ->
				let f = "("^(v_field context expr.etyp f).at_name^")" in
				let n = if o = id_op_prefix_incr then "(++"^f^")"
					else if o = id_op_prefix_decr then "(--"^f^")"
					else if o = id_op_postfix_incr then "("^f^"++)"
					else if o = id_op_postfix_decr then "("^f^"--)"
					else Err.intern "v_call:++--"
				in
				mk_ctxt n (some expr.etyp)
		
		| Assign(f, idx, kind, rhs) ->
				let cid = type2id context.at_type in
				let rt = subst_map (some expr.etyp) cc.cc_v2t in
				let prim = is_primitive_typ rt in
				let expansion =
					try
						let dcl = get_decl_after_tc cid f (- 1) in
						if can_expand dcl
						then Some dcl
						else None
					with _ -> None
				in
				( match expansion, idx with
					| Some dcl, NoArr when (not prim) ->
					(* assumption: assignment done in constructor or at decl,
					only to initialize [[NOT TO CREATE]] expanded objects *)
							if kind = id_op_assign
							then ()
							else Err.intern "v_call: init_exp_kind";
							mk_ctxt (Util_gen_c.assgn2exp_flds f rhs dcl) rt
					| _, NoArr ->
					(* no arrays, primitive or not expandable fields *)
							let rhs = v_gen rhs
							and asg = if kind = id_op_assign then (
									" = "
								) else (
									match primop2str kind with
									| Some s -> " "^s^"= "
									| None -> Err.intern "v_call:primop2str"
								)
							and fld = (v_field context expr.etyp f).at_name in
							let cast = if prim
								then ""
								else "(pat_"^cc_type2str rt^")" in
							mk_ctxt ("("^fld^asg^cast^rhs.at_name^")") rt
					| _, ADim1 i ->
					(* 1 dim array *)
							let rhs = v_gen rhs
							and asg = if kind = id_op_assign then (
									" = "
								) else (
									match primop2str kind with
									| Some s -> " "^s^"= "
									| None -> Err.intern "v_call:primop2str"
								)
							and fld = (v_field context expr.etyp f).at_name in
							let vlt = match some expr.etyp with
								| Array(t, DimLen 1, _)
								| Array(t, _, DimLen 1) ->
										v_typedef t
								| _ -> Err.intern "v_call:arrayt1"
							and idx = (v_gen i).at_name
							and fld = "(pat_array1)"^fld
							(* was: (if expansion=None then fld else "&"^fld) *)
							and bds = get_sysprop se.conf_class id_exc_bounds in
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpint = tmpvar2int tmpvar1 in
							let tmpptr = tmpvar2ptr (Some tmpvar2) in
							let lbl = Ly.lbl2str expr.elbl in
							let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
							let gen =
								"("^tmpint^"="^idx^","^tmpptr^"=(pat_Object)"^fld^",\n"^
								"("^tmpptr^"==Null && mkExc("^nll^","^lbl^")||"
								^tmpint^"<0||"^
								tmpint^">=((pat_array1)"^tmpptr^")->len) && "^
								"(mkExc("^i2s bds^","^Ly.lbl2str expr.elbl^"),0)"^
								",\n(("^vlt^"*)(&((pat_array1)"^tmpptr^
								")->a))["^tmpint^"]"^
								asg^rhs.at_name^")"
							in
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt gen rt
					
					| _, ADim2(x, y) -> (* 2 dim array *)
							let rhs = v_gen rhs
							and asg = if kind = id_op_assign then (
									" = "
								) else (
									match primop2str kind with
									| Some s -> " "^s^"= "
									| None -> Err.intern "v_call:primop2str"
								)
							and fld = (v_field context expr.etyp f).at_name in
							let vlt = match some expr.etyp with
								| Array(t, _, _) -> v_typedef t
								| _ -> Err.intern "v_call:arrayt2"
							and idx = (v_gen x).at_name
							and idy = (v_gen y).at_name
							and fld = "(pat_array2)"^fld
							(* was: (if expansion=None then fld else "&"^fld) *)
							and bds = get_sysprop se.conf_class id_exc_bounds in
							let tmpvar1 = alloc_tmpvar () in
							let tmpvar2 = alloc_tmpvar () in
							let tmpvar3 = alloc_tmpvar () in
							let tmpx = tmpvar2int tmpvar1 in
							let tmpy = tmpvar2int tmpvar2 in
							let tmpptr = tmpvar2ptr (Some tmpvar3) in
							let lbl = Ly.lbl2str expr.elbl in
							let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
							let gen =
								"("^tmpx^"="^idx^","^tmpy^"="^idy^",
								"^tmpptr^" = (pat_Object)"^fld^",\n"^
								"("^tmpptr^"==Null && mkExc("^nll^","^lbl^")||"
								^tmpx^"<0||"^tmpy^"<0||"^
								tmpx^">=(Int)((pat_array2)"^tmpptr^")->len1||"^
								tmpy^">=(Int)((pat_array2)"^tmpptr^")->len2) && "^
								"(mkExc("^i2s bds^","^Ly.lbl2str expr.elbl^"),0)"^
								",\n(("^vlt^"*)(&((pat_array2)"^tmpptr^
								")->a))["^tmpx^"*(Int)(((pat_array2)"^tmpptr^")->len2)+"^
								tmpy^"]"^asg^rhs.at_name^")"
							in
							free_tmpvar tmpvar3;
							free_tmpvar tmpvar2;
							free_tmpvar tmpvar1;
							mk_ctxt gen rt
				)
		
		| If(tl) ->
				let rec theni = function
					| [] -> ""
					| [Else(_,[ExprStmt(_, s)])] ->
							v_expr s
					| Then(_, e,[ExprStmt(_, s)]):: tl ->
							"(("^(v_expr e)^") ? "^(v_expr s)^" : "^(theni tl)^")"
					| _ -> Err.intern "v_call:then"
				in
				Util_gen.mk_ctxt (theni tl) (some expr.etyp)
		
		| Literal c -> Util_gen.mk_ctxt (v_constant c) (some expr.etyp)
		
		| ArrayLiteral (Dim1 a) ->
				mk_ctxt ("(pat_array1)&"^(cc_type2str cc.cc_typ)^"_al"^(i2s a.an1)) (some expr.etyp)
		
		| ArrayLiteral (Dim2 a) ->
				mk_ctxt ("(pat_array2)&"^(cc_type2str cc.cc_typ)^"_al"^(i2s a.an2)) (some expr.etyp)
		
		| DeltaT -> Util_gen.mk_ctxt "deltat" t_time
		
		| Instant -> mk_ctxt "" t_int
		
		| Present(s) -> Util_gen.mk_ctxt (Sc.sgn2str s) (some expr.etyp)
		
		| Timestamp(s) -> Util_gen.mk_ctxt ((Sc.sgn2str s)^"__t") (some expr.etyp)
		
		| SigVal(_, xs, 0) ->
				let s = Sc.sgn2val (sig2sgn xs) in
				let sv = Sc.sgn2str s in
				let typ = some expr.etyp in
				let name = sv in
				{ at_name = name;
					at_type = typ;
					at_exp = true;
				}
		
		| Value(s) ->
				let typ = some expr.etyp in
				let sv = Sc.sgn2str s in
				let name = sv in
				{ at_name = name;
					at_type = typ;
					at_exp = true;
				}
		
		| DotDot(_, _, _) -> Err.intern "v_call:DotDot"
		
		| Slice _ -> Err.intern "v_call:Slice"
		
		| Var _ -> Err.intern "v_call:Var"
		
		| _ -> Err.intern "v_call:Var"
	
	(* =========================================================================
	Arrays
	========================================================================= *)
	let v_gen_exp_in_array_context ro e index =
		let rotyp = ro.trs_type in
		let this, exp = Gen_rct_code.this_in_CC_context ro in
		Util_gen.set_cc_context rotyp this (prepare_subst rotyp) exp;
		let e =
			match index with
			| Gen_rct_code.Dim_i ->
					cc_call_add_get [0] [0, Gen_rct_code.cc_mk_call id_op__i] e
			| Gen_rct_code.Dim_j ->
					cc_call_add_get [0] [0, Gen_rct_code.cc_mk_call id_op__j] e
			| Gen_rct_code.Dim_ij ->
					cc_call_add_get [0; 1] [0, Gen_rct_code.cc_mk_call id_op__i;
						1, Gen_rct_code.cc_mk_call id_op__j] e
			| Gen_rct_code.Dim_ji ->
					cc_call_add_get [0; 1] [0, Gen_rct_code.cc_mk_call id_op__j;
						1, Gen_rct_code.cc_mk_call id_op__i] e
			| Gen_rct_code.Dim_ik ->
					cc_call_add_get [0; 1] [0, Gen_rct_code.cc_mk_call id_op__i;
						1, Gen_rct_code.cc_mk_call id_op__k] e
			| Gen_rct_code.Dim_kj ->
					cc_call_add_get [0; 1] [0, Gen_rct_code.cc_mk_call id_op__k;
						1, Gen_rct_code.cc_mk_call id_op__j] e
		in
		v_gen e
	
	let v_gen_exp_in_v_context ro e =
		let rotyp = ro.trs_type in
		let this, exp = Gen_rct_code.this_in_CC_context ro in
		Util_gen.set_cc_context rotyp this (prepare_subst rotyp) exp;
		v_gen e
	
	(* -------------------------------------------------------------------------
	Generation of Actions
	------------------------------------------------------------------------- *)
	let v_initval t = ()
	
	let v_mk_val_init lbl sv typ =
		match typ with
		| Null ->
				Err.intern "mk_val_init"
		| Simple _ ->
				ps "\n  "; ps sv; ps " = "; v_initval (type2id typ); ps ";"
		| Typ(cid, _) when cid = id_string ->
				ps "\n  "; ps sv; ps " = "; v_initval (type2id typ) ; ps ";"
		| Array(t, DimLen 1, DimLen n)
		| Array(t, DimLen n, DimLen 1) ->
				ps "for (__i="; pi (n - 1); ps ";__i>=0;__i--) {\n";
				ps "  (("; ps (v_typedef t); ps "*)&"; ps sv;
				ps ".a)[__i] = "; v_initval (type2id t);
				ps ";\n};\n";
		| Array(t, DimLen n1, DimLen n2) ->
				ps "for (__i="; pi (n1 - 1); ps ";__i>=0;__i--) {\n";
				ps "for (__j="; pi (n2 - 1); ps ";__j>=0;__j--) {\n";
				ps "  (("; ps (v_typedef t); ps "*)&"; ps sv;
				ps ".a)[__i*"; pi n2; ps "+__j] = ";
				v_initval (type2id t);
				ps ";\n};\n};\n";
		| _ ->
				let cid = type2id typ in
				let cps = c2str cid in
				ps "f_"; ps cps; ps "_"; ps cps; ps "_0(i_"; ps cps;
				ps "(&"; ps sv; ps "));\n"
	
	let v_mk_sig_init lbl xs =
		let t = sig2valtyp xs in
		let sv = Sc.sgn2str (Sc.sgn2val (sig2sgn xs)) in
		v_mk_val_init lbl sv t;
		( match sig2mem xs with
			| None -> ()
			| Some m -> v_mk_val_init lbl (Sc.sgn2str m) t
		);
		( match sig2buf xs with
			| [] -> ()
			| bl ->
					Err.msg(Err.NYI "Not yet supported in Verilog code generation")
		)
	
	let rec v_mk_action a =
		let err ro lbll = Err.msg(Err.VerilogNYI(type2id ro.trs_type, list_last lbll))
		in
		match a with
		| ActStmt (_, e, ro) ->
				ps (v_gen e).at_name
		| ActProd (lbll, _, _, _, _, ro) ->
				err ro lbll
		| ActSigInit(lbll, _, xs, ro) ->
				v_mk_sig_init (list_last lbll) xs
		| ActInit (lbll, sn, t, ro) ->
				v_mk_val_init (list_last lbll) (Sc.sgn2str sn) t
		| ActBlck(bk) ->
				v_mk_blck bk
		| ActEmit (lbll, _, a, _, Rct e, bk, _, ro) ->
				v_mk_blck bk
		| ActEmit (lbll, _, _, dl, Val e, bk, xs, ro) ->
				let sv = Sc.sgn2str (Sc.sgn2val (sig2sgn xs)) in
				let sv = if is_delayed_typ (sig2typ xs) then sv^"__n" else sv in
				let typ = sig2valtyp xs in
				( match dl, typ with
					| [], Null ->
							() (* nothing to do *)
					| _, Null ->
							Err.intern "mk_call:ActEmit:1"
					(* vector asssigned to a vector signal *)
					| [], Array(t, DimLen 1, DimLen n)
					| [], Array(t, DimLen n, DimLen 1) ->
							ps "\nfor (__i=0;__i<"; pi (n); ps ";__i=__i+1) begin\n";
							v_mk_blck bk;
							ps sv; ps "[__i] = ";
							ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_i).at_name;
							ps ";\nend;";
					(* matrix asssigned to a matrix signal *)
					| [], Array(t, DimLen n1, DimLen n2) ->
							ps "for (__i=0;__i<"; pi (n1); ps ";__i=__i+1) begin\n";
							ps "for (__j=0;__j<"; pi (n2); ps ";__j=__j+1) begin\n";
							v_mk_blck bk;
							ps sv; ps "[__i*"; pi n2; ps "+__j] = ";
							ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_ij).at_name;
							ps ";\nend;\nend;";
					| [], _ ->
							v_mk_blck bk;
							ps "\n  "; ps sv; ps " = ";
							ps (v_gen e).at_name; ps ";"
					(* vector asssigned to a slice of a vector signal *)
					| [d], Array(t, DimLen 1, DimLen n)
					| [d], Array(t, DimLen n, DimLen 1) ->
							let l, u = mk_index (React2sya.ro2cid ro) n d in
							if l = u then (
								ps sv; ps "["; pi l; ps "] = ";
								ps (v_gen_exp_in_v_context ro e).at_name; ps ";";
							) else (
								ps "for (__i=0; __i<"; pi (u - l + 1); ps ";__i=__i+1) begin\n";
								v_mk_blck bk;
								ps sv; ps "[__i+"; pi l; ps "] = ";
								ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_i).at_name;
								ps ";\nend;";
							)
					(* matrix asssigned to a slice of a matrix signal *)
					| [d1; d2], Array(t, DimLen n1, DimLen n2) ->
							let l1, u1 = mk_index (React2sya.ro2cid ro) n1 d1
							and l2, u2 = mk_index (React2sya.ro2cid ro) n2 d2 in
							if l1 = u1 && l2 = u2 then (
								ps sv; ps "["; pi l2; ps "*"; pi n2; ps "+"; pi l1; ps "] = ";
								ps (v_gen_exp_in_v_context ro e).at_name; ps ";\n"
							) else if l1 = u1 then (
								ps "for (__j=0; __j<"; pi (u2 - l2); ps ";__j=__j+1) begin\n";
								v_mk_blck bk;
								ps sv; ps "["; pi (l1); ps "*"; pi n2; ps "+__j+";
								pi l2; ps "] = ";
								ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_j).at_name;
								ps ";\nend;;";
							) else if l2 = u2 then (
								ps "for (__i=0; __i<"; pi (u1 - l1); ps ";__i=__i+1) begin\n";
								v_mk_blck bk;
								ps sv; ps "[(__i+"; pi (l1); ps ")*"; pi n2; ps "+";
								pi l2; ps "] = ";
								ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_i).at_name;
								ps ";\nend;;";
							) else (
								ps "for (__i=0; __i<"; pi (u1 - l1); ps ";__i=__i+1) begin\n";
								ps "for (__j=0; __j<"; pi (u2 - l2); ps ";__j=__j+1) begin\n";
								v_mk_blck bk;
								ps sv; ps "[(__i+"; pi (l1); ps ")*"; pi n2; ps "+__j+";
								pi l2; ps "] = ";
								ps (v_gen_exp_in_array_context ro e Gen_rct_code.Dim_ij).at_name;
								ps ";\nend;\nend;";
							)
					| _ -> Err.intern "mk_call:Emit:no_case"
				);
				( match sig2buf xs with
					| [] -> ()
					| d -> err ro lbll
				)
		| ActSigPres _
		| ActSigVal _
		| ActSigIn _
		| ActSigOut _ ->
				()
		| ActPrm (lbll, _, _, _, ro) ->
				err ro lbll
		| ActAssgn (lbll, _, _, _, ro) ->
				err ro lbll
		| ActAssgnInBlck (lbll, _, _, ro) ->
				err ro lbll
		| ActBool (lbll, result, e, ro) ->
				ps "\n  "; ps (Sc.sgn2str result);
				ps (v_gen e).at_name
		| ActTimeInit(lbll, _, ro) ->
				err ro lbll
		| ActTimeAdd(lbll, _, ro) ->
				err ro lbll
		| ActTimeCond(lbll, _, _, e, ro) ->
				err ro lbll
		| ActDbg _ ->
				Err.intern "v_mk_action"
	
	and v_mk_blck bk =
		List.iter
			( fun (e, cll) ->
						let c = v_rctexpr e in
						if c = "0" then (
						(* nothing to do *)
						) else if c = "1" then (
							v_mk_action cll
						) else (
							ps "if ("; ps c; ps ") {\n";
							v_mk_action cll;
							ps "};\n"
						)
			) bk
	
	(* -------------------------------------------------------------------------
	Generation of Definitions
	------------------------------------------------------------------------- *)
	let v_mk_def ~mem d =
		match d.sc_act with
		| None ->
				ps "\nassign "; ps (Sc.sgn2str d.sc_sgn);
				if mem then ps "_w";
				ps " = "; ps (v_rctexpr d.sc_def); ps ";"
		| Some a ->
				ps "\n\nalways @(posedge "; ps (v_rctexpr d.sc_def); ps ")";
				ps "\nbegin";
				v_mk_action a;
				ps "\nend"
	
	(* -------------------------------------------------------------------------
	Generation of Module
	------------------------------------------------------------------------- *)
	let make_verilog_code () =
		let sa = some ac.ac_sca "make_verilog_code" in
		let alpha = Sc.sgn2str sa.sa_alpha in
		ps "module MAIN";
		lst_sep2BpsB "(" ")" ps "," ("clck":: (input_sigs sa)@(output_sigs sa)); ps ";";
		ps "\n\ninput clck;";
		
		ps "\ninitial";
		ps "\nbegin";
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							ps "\n "; print_sgn (sig2sgn xs); ps " = 0;";
						) else ()
			) sa.sa_sigs;
		List.iter
			( fun xs ->
						if is_output_sig xs then (
							ps "\n "; ps (sig2str xs); ps "_r = 0;";
						) else ()
			) sa.sa_sigs;
		List.iter
			( fun xs ->
						if not (is_input_sig xs && is_output_sig xs) then (
							ps "\n "; ps (sig2str xs); ps "_r = 0;";
						) else ()
			) sa.sa_sigs;
		ps "\n "; ps alpha; ps " = 1;";
		ps "\n __i = 0;";
		ps "\n __j = 0;";
		ps "\n __k = 0;";
		List.iter
			( fun d -> ps "\n "; ps (Sc.sgn2str d.sc_sgn); ps " = ";
						ps "0;"
			) sa.sa_mems;
		ps "\nend\n";
		
		ps "\n\nalways @(posedge clck)";
		ps "\nbegin";
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							ps "\n "; print_sgn (sig2sgn xs); ps " = 0;";
						) else ()
			) sa.sa_sigs;
		List.iter
			( fun xs ->
						if is_output_sig xs then (
							ps "\n "; ps (sig2str xs); ps "_r = 0;";
						) else ()
			) sa.sa_sigs;
		ps "\n "; ps alpha; ps " = 0;";
		List.iter
			( fun xs -> ps "\n "; ps (Sc.sgn2str xs.sc_sgn); ps " = ";
						ps (Sc.sgn2str xs.sc_sgn); ps "_w;"
			) sa.sa_mems;
		ps "\nend\n";
		
		List.iter mk_input_callback sa.sa_sigs;
		pn();
		List.iter mk_ext_sign_decl sa.sa_sigs;
		pn();
		List.iter mk_wire sa.sa_eqns;
		pn();
		ps "\nreg    "; ps alpha; ps ";";
		ps "\nreg    __i;";
		ps "\nreg    __j;";
		ps "\nreg    __k;";
		pn();
		List.iter (fun d -> ps "\nreg    "; ps (Sc.sgn2str d.sc_sgn); ps ";";
						ps "\nwire   "; ps (Sc.sgn2str d.sc_sgn); ps "_w;"
			) sa.sa_mems;
		List.iter (v_mk_def ~mem: false) sa.sa_eqns;
		List.iter (v_mk_def ~mem: true) sa.sa_mems;
		
		List.iter mk_output_callback sa.sa_sigs;
		pn();
		
		ps "\n\nendmodule\n"
	
	(* -------------------------------------------------------------------------
	Generation of Test Module
	------------------------------------------------------------------------- *)
	let make_verilog_test instants =
		let sa = some ac.ac_sca "make_test_module" in
		
		let conf_cid = type2id se.conf_class in
		let conf_str = c2str conf_cid in
		let fn = se.file_prefix^conf_str^".v" in
		let time = 5 in
		
		ps "`include \""; ps fn; ps "\"";
		ps "\n\nmodule test;";
		(*lst_sep2BpsB "(" ")" ps "," (output_sigs sa);ps ";\n";*)
		
		ps "\nreg clck;";
		ps "\nreg ok;";
		ps "\nreg inst_ok;";
		ps "\n";
		
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							ps "\nreg    "; ps (sig2str xs); ps ";";
							( match sig2valtyp xs with
								| Null -> ()
								| Simple cid ->
										let range = class2range cid in
										ps "\nreg "; ps range; ps "  _";
										ps (sig2str xs); ps ";";
								| _ -> Err.msg(Err.VerilogIO("Arrays are not allowed for input '"
													^mf2str(sig2id xs)^"'"));
							);
						) else ()
			) sa.sa_sigs;
		List.iter
			( fun xs ->
						if is_output_sig xs then (
							ps "\nwire "; ps (sig2str xs); ps ";";
							( match sig2valtyp xs with
								| Null -> ()
								| Simple cid ->
										let range = class2range cid in
										ps "\nwire "; ps range; ps "  _";
										ps (sig2str xs); ps ";";
								| _ -> Err.msg(Err.VerilogIO("Arrays are not allowed for output '"
													^mf2str(sig2id xs)^"'"));
							);
						) else ()
			) sa.sa_sigs;
		List.iter
			( fun xs ->
						if is_input_sig xs || is_output_sig xs then (
						) else (
							ps "\nwire "; ps (sig2str xs); ps ";";
							( match sig2valtyp xs with
								| Null -> ()
								| Simple cid ->
										let range = class2range cid in
										ps "\nwire "; ps range; ps "  _";
										ps (sig2str xs); ps ";";
								| Array(t, DimLen 1, DimLen n)
								| Array(t, DimLen n, DimLen 1) ->
										( match t with
											| Simple cid ->
													let range = class2range cid in
													ps "\nreg "; ps range; ps "    ";
													print_sgn (Sc.sgn2val(sig2sgn xs));
													ps ("[0:"^i2s (n - 1)^"];");
											| _ -> Err.intern "make_test_module - local signals"
										);
								| Array(t, DimLen n1, DimLen n2) ->
										( match t with
											| Simple cid ->
													let range = class2range cid in
													ps "\nreg "; ps range; ps "    ";
													print_sgn (Sc.sgn2val(sig2sgn xs));
													ps ("[0:"^i2s (n1 * n2 - 1)^"];");
											| _ -> Err.intern "make_test_module - local signals"
										);
								| _ -> Err.intern "make_test_module - local signals"
							);
						)
			) sa.sa_sigs;
		
		ps "\n\nMAIN main";
		lst_sep2BpsB "(" ")" ps "," ("clck":: (input_sigs sa)@(output_sigs sa)); ps ";";
		
		ps "\n\ninitial\nbegin";
		ps "\n  clck=0;";
		ps "\n  ok=1;";
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							ps "\n  "; ps (sig2str xs); ps " = 0;";
						) else ()
			) sa.sa_sigs;
		ps "\n  #"; pi ((List.length instants) * time + 1); ps ";";
		ps "\n  if (ok)";
		ps "\n    $display(\"#success\");";
		ps "\n  #1 $finish;";
		ps "\nend";
		
		let counter = ref 0 in
		List.iter
			( fun x ->
						counter := !counter + 1;
						ps "\n\ninitial #"; pi (time * !counter);
						ps "\nbegin";
						ps "\n  inst_ok = 1;";
						ps "\n  clck = 1;";
						List.iter
							( fun xs ->
										if is_input_sig xs then (
											let sg = sig2str xs in
											( try let inp =
													List.find
														( fun y -> sg = y.Sim_type.emitname
														) x.Sim_type.tr_emit
												in
												ps "\n  "; ps sg; ps " = 1;";
												( match inp.Sim_type.emitarg with
													| Sim_type.SLiteral (_, arg, _)
													-> ps "\n  _"; ps sg; ps " = ";
															ps arg; ps ";"
													| Sim_type.SNull -> ()
													| _ -> Err.intern "make_verilog_test:input arguments"
												)
											with Not_found ->
													ps "\n  "; ps sg; ps " = 0;"
											)
										)
							) sa.sa_sigs;
						ps "\n#1;";
						List.iter
							( fun xs ->
										if is_output_sig xs then (
											let sg = sig2str xs in
											( try let outp =
													List.find
														( fun y -> sg = y.Sim_type.emitname
														) x.Sim_type.tr_outmsg
												in
												ps "\n  if ("; ps sg; ps " != 1";
												( match outp.Sim_type.emitarg with
													| Sim_type.SNull ->
															ps ") begin";
															ps "\n    if (inst_ok)";
															ps "\n      $display(\"# instant: ";
															pi (time * !counter); ps "\");";
															ps "\n    inst_ok = 0;";
															ps "\n    $display(\"#   expected: ";
															ps sg; ps "\\n#   found: \");";
													| Sim_type.SLiteral (_, arg, _) ->
															ps " | _"; ps sg; ps " != ";
															ps arg;
															ps ") begin";
															ps "\n    if (inst_ok)";
															ps "\n      $display(\"# instant: ";
															pi (time * !counter); ps "\");";
															ps "\n    inst_ok = 0;";
															ps "\n    $display(\"#   expected: ";
															ps sg; ps "(\", "; ps "_"; ps sg;
															ps ", \")\\n#   found: ";
															ps sg; ps "("; ps arg; ps ")\");";
													| _ -> Err.intern "make_verilog_test:output arguments"
												)
											
											with Not_found ->
													ps "\n  if ("; ps sg; ps ") begin";
													ps "\n    if (inst_ok)";
													ps "\n      $display(\"# instant: ";
													pi (time * !counter); ps "\");";
													ps "\n    inst_ok = 0;";
													ps "\n    $display(\"#   expected: \\n#   found: ";
													ps sg; ps "\");";
											);
											ps "\n    ok = 0;";
											ps "\n  end"
										)
							) sa.sa_sigs;
						
						ps "\n  clck = 0;";
						ps "\nend"
			) instants;
		
		(*{ emitname : string ;
		emitarg : tliteral list ;
		mutable emitflow : sigflow_t option }
		
		{ tr_break : bool;
		tr_emit : emit_t list;
		tr_outmsg : emit_t list;
		tr_eof : bool }
		*)
		ps "\n\ninitial\nbegin";
		let conf_cid = type2id se.conf_class in
		let conf_str = c2str conf_cid in
		let fn = se.file_prefix^conf_str^"_test.vcd" in
		ps "\n  $dumpfile(\""; ps fn; ps "\");";
		ps "\n  $dumpvars;";
		ps "\nend";
		
		ps "\n\nendmodule\n"
	
	let process_trace_file () =
		let list_of_instants = ref [] in
		let process_instant x =
			list_of_instants := x:: !list_of_instants in
		try
			Lex_wrapper.prc_trace_file
				process_instant
				(fun sl -> pn(); List.iter ps sl; pF())
				se.trace_file;
			List.rev !list_of_instants
		with
			_ -> raise (Error("\nerror while reading trace file"))
	
	let make_verilog () =
		let conf_cid = type2id se.conf_class in
		let conf_str = c2str conf_cid in
		let fn = se.file_prefix^conf_str^".v" in
		let oc = open_out fn in
		( try
			push_print_fct (output_string oc);
			( try
				make_verilog_code ();
				pop_print_fct ();
				close_out oc;
			with x ->
					pop_print_fct ();
					close_out oc;
					raise x
			)
		with
		| Error t -> raise ( Error t)
		| _ -> ()
		);
		match se.target_sys with
		| Verilog _ ->
				()
		| VerilogSimulation ->
				let fn = se.file_prefix^conf_str^"_test.v" in
				let instants = process_trace_file () in
				let oc = open_out fn in
				( try
					push_print_fct (output_string oc);
					( try
						make_verilog_test instants;
						pop_print_fct ();
						close_out oc;
					with x ->
							pop_print_fct ();
							close_out oc;
							raise x
					)
				with
				| Error t -> raise ( Error t)
				| _ -> ()
				)
		| _ -> Err.intern "make_verilog"
	
end
