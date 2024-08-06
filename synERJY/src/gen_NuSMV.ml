open Ast
open P
open Util
open Util_gen
open Util_print
open Gen_application
open Gen_formula
(* ==========================================================================
Code generation for NuSMV
========================================================================== *)

module SMV =

struct
	
	let dec = "0d"^se.word_width^"_"
	
	let label2str ll =
		lst_sep2str (fun x -> Ly.lbl2str x) "__" ll
	
	let sig2str xs =
		match xs.trs_sname with
		| (Call(_, None), _):: cl -> calll2str "_" cl
		| _ -> assert false
	
	(* -------------------------------------------------------------------------
	filter fields of type time
	------------------------------------------------------------------------- *)
	let get_fields_of_type_time () =
		List.fold_left
			( fun l ro ->
						let ast = i_ast (type2id ro.trs_type) in
						List.fold_left
							( fun l dcl ->
										match dcl.entry with
										| Field f when dcl.signature.rt = t_time
										-> (dcl.name, f, ro):: l
										| _ -> l
							) l ast.declseq
			) [] ac.ac_rctobj_tree
	
	(* -------------------------------------------------------------------------
	Input and Output Signals for Modul Parameters
	------------------------------------------------------------------------- *)
	let mk_sig xs =
		[sig2sgn xs, sig2str xs]
	
	let mk_val_sig xs =
		let sn = sig2sgn xs in
		let s = sig2str xs in
		[sn, s; Sc.sgn2val sn, s^"__v"]
	
	let mk_ctl_sigs cond sigs =
		List.fold_left
			( fun l xs ->
						if cond xs
						then
							( match sig2valtyp xs with
								| Null
								-> (mk_sig xs)@l
								| Simple t when t = id_bool
								-> (mk_val_sig xs)@l
								| _ -> (mk_sig xs)@l
							)
						else l
			) [] sigs
	
	let input_sigs sigs = mk_ctl_sigs is_input_sig sigs
	let output_sigs sigs = mk_ctl_sigs is_output_sig sigs
	
	(* -------------------------------------------------------------------------
	Input labels
	------------------------------------------------------------------------- *)
	let get_delta d =
		match d.sc_act with
		| Some(ActBool (ll, d, e, ro)) ->
				if Ly.is_specified_label e.elbl then (
					let sn = ro_mf2str ro (Ly.lbl2mf e.elbl) in
					(d, sn)
				) else (
					(d,"INPUT__"^Sc.sgn2str d)
				)
		| _ -> Err.intern "get_delta"
	
	let mk_data_inputs dl =
		List.map get_delta dl
	
	(* -------------------------------------------------------------------------
	Output labels
	------------------------------------------------------------------------- *)
	let rec data_outputs_call cll l =
		match cll with
		| ActStmt (_, e, ro)
		| ActEmit (_, _, _, _, Val e, _, _, ro)
		| ActAssgn (_, _, e, _, ro) ->
				let sn = ro_mf2str ro (Ly.lbl2mf e.elbl) in
				(Sc.next_ext(), sn):: l
		| ActBlck bk ->
				mk_data_outputs_blck bk l
		| _ -> l
	and mk_data_outputs_blck bk l =
		List.fold_right
			( fun (_, cll) l ->
						data_outputs_call cll l
			) bk l
	
	let mk_data_outputs dl =
		List.fold_right
			( fun d l ->
						match d.sc_act with
						| Some a -> data_outputs_call a l
						| None -> Err.intern "mk_data_outputs"
			) dl []
	
	(* -------------------------------------------------------------------------
	Reactive expressions to SMV format
	------------------------------------------------------------------------- *)
	let rec smv_rctexp = function
		| S x -> sgn2__str x
		| CC x -> Err.intern "npexp2str"
		| TT -> "1"
		| FF -> "0"
		| NNot x -> "!("^smv_rctexp x^")"
		| AAnd el -> "("^(lst_sep2str smv_rctexp " & " el)^")"
		| OOr el -> "("^(lst_sep2str smv_rctexp " | " el)^")"
	
	(* -------------------------------------------------------------------------
	Generation of Expressions
	------------------------------------------------------------------------- *)
	let smv_this_in_CC_context ro =
		let ctyp = cc_type2str ro.trs_type in
		"__"^ctyp^obj2str ro
	
	let smv_constant (t, s, v) =
		if t = t_time ||
		le_type ~sub: t ~super: t_cint64 || le_type ~sub: t ~super: t_cuint64 then (
			let conf_cid = type2id se.conf_class in
			let (t, s, v') = field2literal conf_cid id_timing in
			if t != t_time then Err.intern "smv_constant:1";
			let timing = some v' "smv_constant:2" in
			Int64.to_string (Int64.div (some v "smv_constant:3") timing)
		) else if t = t_int then (
			s
		) else if t = t_float || t = t_double then (
			""
		) else if t = t_bool then (
			if s ="true" then "1" else
			if s ="false" then "0" else
				Err.intern "smv_constant:4"
		) else if t = t_string then (
			""
		) else (
			Err.intern "smv_constant:5"
		)
	
	let smv_field context lbl mf_typ mf =
		let mf_typ = some mf_typ "smv_field:1" in
		let mf_typ = subst_map mf_typ cc.cc_v2t in
		let cid = type2id context.at_type in
		try
			let dcl = get_decl_after_tc cid mf (- 1) in
			(* ------- all cases are field accesses ------------------------- *)
			( try
				if is_sensor_or_signal_typ mf_typ then (
					cc.cc_this_ctxt
				) else if mf_typ = t_bool || mf_typ = t_time then (
					let fld = if dcl.scope = Class then (
							match native2name dcl with
							| Some s -> s
							| None -> "__"^(Ly.c2str dcl.origin)^"_"^(Ly.mf2str mf)
						) else (
							context.at_name^"_"^(Ly.mf2str mf)
						)
					in
					mk_ctxt fld mf_typ
				) else (
					mk_ctxt (Ly.mf2str mf) mf_typ
				)
			with _ -> Err.intern "smv_field:2"
			)
		with _ ->
		(* ------- formal or local parameter -------------------------------- *)
				mk_ctxt (Ly.mf2str mf) mf_typ
	
	let rec smv_expr ?(filter = false) expr =
		(smv_gen filter expr).at_name
	
	and smv_gen filter expr =
		smv_call filter cc.cc_this_ctxt expr
	
	and smv_call filter context expr =
		let smv_expr = smv_expr ~filter: filter in
		let smv_call = smv_call filter in
		let smv_gen = smv_gen filter in
		let t = some expr.etyp "smv_call" in
		if t = t_bool || t = t_time || not(filter) then (
			let some x = some x "smv_call:some" in
			match expr.expr with
			| Dot({ expr = DeltaT },{ expr = Call(o, Some []) }) when o = id_op_value ->
					mk_ctxt "1" t_time (* instants are normalized to have timing 1*)
			
			| Dot(e1, e2) ->
					smv_call (smv_call context e1) e2
			
			| Call(mf, None) ->
					smv_field context expr.elbl expr.etyp mf
			
			| Call(mf, Some apl) ->
					let t = some expr.etyp in
					if t = t_time || t = t_bool then (
						match (apl, primop2str mf) with
						| [ap], Some s ->
								let n = "("^context.at_name^s^smv_expr ap^")" in
								mk_ctxt n (some expr.etyp)
						| [], Some s ->
								let n = "("^s^context.at_name^")" in
								mk_ctxt n (some expr.etyp)
						| _ -> cc.cc_this_ctxt
					) else (
						cc.cc_this_ctxt
					)
			
			| Assign(f, idx, kind, rhs) ->
					let cid = type2id context.at_type in
					let rt = subst_map (some expr.etyp) cc.cc_v2t in
					let prim = is_primitive_typ rt in
					let expansion = try
							let dcl = get_decl_after_tc cid f (- 1) in
							if can_expand dcl
							then Some dcl
							else None
						with _ -> None
					in
					( match expansion, idx with
						| Some dcl, NoArr when (not prim) ->
								cc.cc_this_ctxt
						| _, NoArr -> (* no arrays, primitive or not expandable fields *)
								let rhs = smv_gen rhs
								and asg = if kind = id_op_assign then (
										" := "
									) else (
										match primop2str kind with
										| Some s -> " "^s^"= "
										| None -> Err.intern "smv_call:primop2str"
									)
								and fld = (smv_field context expr.elbl expr.etyp f).at_name in
								mk_ctxt ("("^fld^asg^rhs.at_name^")") rt
						| _, ADim1 _
						| _, ADim2 _ -> cc.cc_this_ctxt
					)
			| If(tl) ->
					let rec theni = function
						| [] ->
								""
						| [Else(_,[ExprStmt(_, s)])] ->
								smv_expr s
						| Then(_, e,[ExprStmt(_, s)]):: tl ->
								"case ("^(smv_expr e)^") 1 : "^ (smv_expr s)
								^"; 0 : "^(theni tl)^"esac"
						| _ -> Err.intern "smv_call:then"
					in
					mk_ctxt (theni tl) (some expr.etyp)
			
			| Literal c -> mk_ctxt (smv_constant c) (some expr.etyp)
			| DeltaT -> mk_ctxt "1" t_time
			| Instant -> Err.intern "smv_call:instant"
			| Present(s) -> mk_ctxt (Sc.sgn2str s) (some expr.etyp)
			| Timestamp(s) -> mk_ctxt (sgn2__str s^"__t") (some expr.etyp)
			| SigVal(_, xs, 0) ->
					let s = Sc.sgn2val (sig2sgn xs) in
					let sv = Sc.sgn2str s in
					let typ = some expr.etyp in
					let name = sv^"__v" in
					{ at_name = name;
						at_type = typ;
						at_exp = true;
					}
			| _ -> cc.cc_this_ctxt
		) else (
			raise Not_found
		)
	
	(* -------------------------------------------------------------------------
	Evaluation of an expression to a constant
	------------------------------------------------------------------------- *)
	let sgn2sig s ro = React2sya.sgn2sig s (ro2sigs ro)
	
	let rec ev_mf time_cnstrs mf =
		try
			let (mf, r, _) = List.find (fun (x, _, _) -> x = mf) time_cnstrs in
			ev_literal r.upper.expr
		with Not_found -> 1
	
	and ev_const_expr time_cnstrs ro expr =
		if some expr.etyp "ev_const_expr" = t_time then (
			match expr.expr with
			| Dot({ expr = DeltaT },{ expr = Call(o, Some []) }) when o = id_op_value ->
					1
			| Dot(e1, e2) ->
					let v1 = ev_const_expr time_cnstrs ro e1 in
					ev_const_op time_cnstrs ro v1 e2
			| Call(mf, None) ->
					ev_mf time_cnstrs mf
			| Timestamp(s) ->
					ev_mf time_cnstrs (sig2id (sgn2sig s ro))
			| Value(s) ->
					ev_mf time_cnstrs (sig2id (sgn2sig s ro))
			| SigVal(_, xs, 0) ->
					ev_mf time_cnstrs (sig2id xs)
			| _ ->
					ev_literal expr.expr
		) else (
			raise Not_found
		)
	
	and ev_const_op time_cnstrs ro v1 e =
		match e.expr with
		| Call(o, Some [e]) -> let v2 = ev_const_expr time_cnstrs ro e in
				if o = id_op_add then v1 + v2 else
				if o = id_op_sub then v1 - v2 else
				if o = id_op_mult then v1 * v2 else
				if o = id_op_div then v1 / v2 else
					raise Not_found
		| Call(o, Some []) -> if o = id_op_minus
				then (- v1)
				else raise Not_found
		| _ -> raise Not_found
	
	and ev_literal = function
		| Literal (t, s, Some v) -> int64_2_int v
		| Literal (t, s, None) -> int_of_string s
		| _ -> raise Not_found
	
	let eval_const_expr time_cnstrs ro expr =
		try let conf_cid = type2id se.conf_class in
			let (t, s, v) = field2literal conf_cid id_timing in
			let timing = int64_2_int (some v "eval_const_expr") in
			( ev_const_expr time_cnstrs ro expr ) / timing
		with Not_found ->
				Err.msg (Err.SMV "this expression should be a constant integral \
						expression, but contains illegal (variable) components")
	
	(* -------------------------------------------------------------------------
	set context for expressions depending on the object
	------------------------------------------------------------------------- *)
	let set_smv_context ro =
		let rotyp = ro.trs_type in
		set_cc_context rotyp (smv_this_in_CC_context ro) (prepare_subst rotyp) false
	
	(* -------------------------------------------------------------------------
	Generation of definitions
	------------------------------------------------------------------------- *)
	let peqn l r = pnT 1; ps l; ps " := "; ps r; ps ";"
	
	(* -------------------------------------------------------------------------
	Generation of Actions
	------------------------------------------------------------------------- *)
	let rec smv_mk_action d cll =
		match cll with
		| ActStmt (_, e, ro) ->
				if Ly.is_specified_label e.elbl then (
					peqn (label2str [e.elbl]) (smv_rctexp d.sc_def)
				)
		| ActProd _ -> ()
		| ActSigInit _ -> ()
		| ActInit _ -> ()
		| ActBlck(bk) -> smv_mk_blck d bk
		| ActEmit (_, _, _, _, Rct _, bk, _, _) -> smv_mk_blck d bk
		| ActEmit (_, _, _, _, Val e, _, _, _) ->
				if Ly.is_specified_label e.elbl then (
					peqn (label2str [e.elbl]) (smv_rctexp d.sc_def)
				)
		| ActSigPres _
		| ActSigVal _
		| ActSigIn _
		| ActSigOut _
		| ActPrm _ -> ()
		| ActAssgn (_, _, e, _, _) ->
				if Ly.is_specified_label e.elbl then (
					peqn (label2str [e.elbl]) (smv_rctexp d.sc_def)
				)
		| ActAssgnInBlck _ -> ()
		| ActBool(_, delta, e, ro) ->
				set_smv_context ro;
				if Ly.is_specified_label e.elbl then (
					peqn (sgn2__str delta)
						(smv_rctexp d.sc_def^" | "^label2str [e.elbl])
				)
		| ActTimeInit _
		| ActTimeAdd _ -> ()
		| ActTimeCond(_, delta, sv, e, ro) ->
				set_smv_context ro;
				let trigger = smv_rctexp d.sc_def in
				let cond = smv_expr e in
				peqn (sgn2__str delta)
					(trigger^" & ("^sgn2__str sv^" + 1 >= "^cond^")")
		| ActDbg _ -> ()
	
	and smv_mk_blck d bk =
		List.iter
			( fun (c, cll) ->
						if c = FF then (
						(* nothing to do *)
						) else if c = TT then (
							smv_mk_action d cll
						) else (
							d.sc_def <- AAnd[d.sc_def; c];
							smv_mk_action d cll
						)
			) bk
	
	let smv_mk_eqn d =
		match d.sc_act with
		| None -> peqn (sgn2__str d.sc_sgn) (smv_rctexp d.sc_def)
		| Some cll -> smv_mk_action d cll
	
	(* -------------------------------------------------------------------------
	Handle declarations of signals with a timestamp
	------------------------------------------------------------------------- *)
	let timestamp_sigs sa =
		List.fold_left
			( fun l xs ->
						if xs.trs_sdecl.sig_tmstmp
						then (sig2id xs, sig2sgn xs, sig2ro xs):: l
						else l
			) [] sa.sa_sigs
	
	let find_time_constraints sa =
		List.fold_right
			( fun sp l ->
						match sp with
						| ValConstraint (_, mf, r), ro -> (mf, r, ro):: l
						| _ -> l
			) sa.sa_axms []
	
	(* -------------------------------------------------------------------------
	Split Actions
	------------------------------------------------------------------------- *)
	type def_kind_t =
		{ simple : tsc_ctrl list;
			act_bool : tsc_ctrl list;
			time_cond : tsc_ctrl list;
			time_init : (tsc_id * tsc_exp) list;
			time_add : (tsc_id * tsc_exp) list;
			out_act : tsc_ctrl list;
		}
	
	let rec distinguish_def d =
		match d.sc_act with
		| None ->
				{ simple = [d];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [];
				}
		| Some(ActStmt (_, e, _)) when Ly.is_specified_label e.elbl ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [d];
				}
		| Some(ActEmit (_, _, _, _, Val e, _, _, _)) when Ly.is_specified_label e.elbl ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [d];
				}
		| Some(ActAssgn (_, _, e, _, _)) when Ly.is_specified_label e.elbl ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [d];
				}
		| Some(ActBool _)	->
				{ simple = [];
					act_bool = [d];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [];
				}
		| Some(ActTimeCond _) ->
				{ simple = [];
					act_bool = [];
					time_cond = [d];
					time_init = [];
					time_add = [];
					out_act = [];
				}
		| Some(ActTimeInit (_, sv, _)) ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [sv, d.sc_def];
					time_add = [];
					out_act = [];
				}
		| Some(ActTimeAdd (_, s, _)) ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [s, d.sc_def];
					out_act = [];
				}
		(* | Some(ActBlck _)
		-> XXX Not quite clear what to do *)
		| _ ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [];
				}
	and distinguish_def_l dl =
		match dl with
		| [] ->
				{ simple = [];
					act_bool = [];
					time_cond = [];
					time_init = [];
					time_add = [];
					out_act = [];
				}
		| d:: dl ->
				let r1 = distinguish_def d
				and r2 = distinguish_def_l dl in
				{ simple = r1.simple@r2.simple;
					act_bool = r1.act_bool@r2.act_bool;
					time_cond = r1.time_cond@r2.time_cond;
					time_init = r1.time_init@r2.time_init;
					time_add = r1.time_add@r2.time_add;
					out_act = r1.out_act@r2.out_act;
				}
	
	(* -------------------------------------------------------------------------
	Generation
	------------------------------------------------------------------------- *)
	let pc_smv s = ps "-- "; ps s; ps "\n"
	
	let make_smv sa spec props =
		let eqns = distinguish_def_l sa.sa_eqns in
		let inputs = input_sigs sa.sa_sigs
		and outputs = output_sigs sa.sa_sigs
		and data_inputs = mk_data_inputs eqns.act_bool
		and data_outputs = mk_data_outputs eqns.out_act
		and fields = get_fields_of_type_time()
		and timestamps = timestamp_sigs sa
		and time_cnstrs = find_time_constraints sa
		and alpha = sgn2__str sa.sa_alpha in
		let vars, fair, defs, ins, dins, outs, mems =
			gen_model ~insig: outputs
				~indatasig: data_outputs
				~outsig: inputs
				~outdatasig: data_inputs
				~spec: spec
		in
		ps "MODULE main";
		ps "\n\nVAR";
		List.iter (fun (_, sn) -> pnT 1; ps sn; ps " : boolean;") inputs;
		(* declare fields *)
		List.iter
			( fun (mf, _, ro) ->
						let name = Ly.mf2str mf in
						if name = "timing"
						then (
						(* not to be declared *)
						) else (
							let name = smv_this_in_CC_context ro^"_"^name in
							try
								let (mf, r, ro') =
									List.find (fun (x, _, _) -> x = mf) time_cnstrs in
								pnT 1; ps name; ps " : 0 .. "; ps (smv_expr r.upper); ps ";"
							with Not_found ->
									pnT 1; ps name ; ps ": 0 .. 1;"
						)
			) fields;
		ps "\n";
		pnT 1; ps alpha; ps " : boolean;";
		(*pnT 1;ps beta; ps " : boolean;";*)
		List.iter
			( fun d -> pnT 1; ps (sgn2__str d.sc_sgn); ps " : boolean;"
			) sa.sa_mems;
		ps "\n";
		List.iter
			( fun sn -> pnT 1; ps sn; ps " : boolean;"
			) vars;
		ps "  \n";
		List.iter
			( fun (mf, sn, ro) ->
						let sn = sgn2__str sn in
						set_smv_context ro;
						try
							let (mf, r, _) =
								List.find (fun (x, _, _) -> x = mf) time_cnstrs in
							pnT 1; ps sn; ps "__t : 0 .. "; ps (smv_expr r.upper); ps ";"
						with Not_found ->
								pnT 1; ps sn; ps "__t : 0 .. 1;"
			) timestamps;
		List.iter
			( fun d ->
						match d.sc_act with
						| Some(ActTimeCond(_, delta, sv, e, ro)) ->
								set_smv_context ro;
								pnT 1; ps (sgn2__str sv); ps " : 0 .. ";
								pi (eval_const_expr time_cnstrs ro e); ps ";"
						| _ -> Err.intern "mk_smv:time_cond"
			) eqns.time_cond;
		ps "\n\nDEFINE";
		List.iter (fun (sn, e) -> peqn sn e) dins;
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							let sn = sgn2__str (sig2sgn xs) in
							( try
								let (_, e) = List.find (fun (s, _) -> s = sn) ins in
								peqn sn (sig2str xs^" | "^e)
							with Not_found ->
									peqn sn (sig2str xs);
							);
							if sig2valtyp xs = t_bool then (
								let sv = sgn2__str (Sc.sgn2val (sig2sgn xs)) in
								try
									let (_, e) = List.find (fun (s, _) -> s = sv) ins in
									peqn (sn^"__v") (sig2str xs^"__v | "^e)
								with Not_found ->
										peqn (sn^"__v") (sig2str xs^"__v")
							)
						)
			) sa.sa_sigs;
		ps "\n";
		List.iter smv_mk_eqn (eqns.simple@eqns.act_bool@eqns.time_cond@eqns.out_act);
		ps "\n";
		List.iter (fun (sn, e) -> pnT 1; ps sn; ps " := "; ps e; ps ";") defs;
		ps "\n";
		List.iter
			( fun xs ->
						if is_output_sig xs then (
							let sn = sgn2__str (sig2sgn xs) in
							( try
								let (_, e) = List.find (fun (s, _) -> s = sn) outs in
								peqn (sig2str xs) ("("^sn^" | "^e^")")
							with Not_found ->
									peqn (sig2str xs) sn
							);
							if xs.trs_sdecl.sig_tmstmp then (
								peqn (sig2str xs^"__t") (sn^"__t")
							);
							if sig2valtyp xs = t_bool then (
								let sv = sgn2__str (Sc.sgn2val (sig2sgn xs)) in
								try
									let (_, e) = List.find (fun (s, _) -> s = sv) outs in
									peqn (sig2str xs^"__v") ("("^sv^" | "^e^")")
								with Not_found ->
										peqn (sig2str xs^"__v") sv
							);
						)
			) sa.sa_sigs;
		ps "\n\nASSIGN";
		pnT 1; ps "init("; ps alpha; ps ") := 1;";
		pnT 1; ps "next("; ps alpha; ps ") := 0;";
		(* pnT 1; ps "init("; ps beta; ps ") := 0;";
		pnT 1; ps "next("; ps beta; ps ") := 1;";*)
		List.iter
			( fun (mf, fld, ro) ->
						let name = Ly.mf2str mf in
						if name = "timing"
						then (
						(* not to be declared *)
						) else (
							match fld.init with
							| None
							-> ()
							| Some e
							-> let name = smv_this_in_CC_context ro^"_"^name in
									set_smv_context ro;
									pnT 1; ps "init("; ps name; ps ") := ";
									ps (smv_expr e); ps ";"
						)
			) fields;
		List.iter
			( fun d ->
						pnT 1; ps "init("; ps (sgn2__str d.sc_sgn); ps ") := 0;";
						peqn ("next("^(sgn2__str d.sc_sgn)^")") (smv_rctexp d.sc_def)
			) sa.sa_mems;
		List.iter
			( fun (mf, sn, ro) ->
						let sn = sgn2__str sn in
						pnT 1; ps "init("; ps sn; ps "__t) := 0;";
			) timestamps;
		List.iter
			( fun (sv, _) ->
						pnT 1; ps "init("; ps (sgn2__str sv); ps ") := 0;"
			) eqns.time_init;
		ps "\n";
		List.iter
			( fun (sn, i, n) ->
						pnT 1; ps "init("; ps sn; ps ") := "; ps i; ps ";";
						pnT 1; ps "next("; ps sn; ps ") := "; ps n; ps ";"
			) mems;
		ps "\n";
		List.iter
			( fun (mf, sn, ro) ->
						let sn = sgn2__str sn in
						ps "\nTRANS";
						pnT 1; ps "case";
						pnT 2; ps "("; ps sn; ps ") : "; ps "next("; ps sn; ps "__t) = 0;";
						( try
							let (mf, r, ro') = List.find (fun (x, _, _) -> x = mf) time_cnstrs in
							let bnd = smv_expr r.upper in
							pnT 2; ps "(!"; ps sn; ps " & "; ps sn; ps "__t < "; ps bnd; ps") : ";
							ps "next("; ps sn; ps "__t) = "; ps sn; ps "__t + 1;";
						with Not_found ->
								pnT 2; ps "(!"; ps sn; ps " & "; ps sn; ps "__t < 5"; ps") : ";
								ps "next("; ps sn; ps "__t) = "; ps sn; ps "__t + 1;";
						);
						pnT 2; ps "1 : next("; ps sn; ps "__t) = "; ps sn; ps "__t;";
						pnT 1; ps "esac;\n";
			) timestamps;
		List.iter
			( fun (sv, e1) ->
						ps "\nTRANS";
						( try
							let (_, e2) = List.find (fun (sv', _) -> sv = sv') eqns.time_add in
							pnT 1; ps "case";
							pnT 2; ps (smv_rctexp e1); ps " : ";
							ps "next("; ps (sgn2__str sv); ps ") = 0;";
							pnT 2; ps (smv_rctexp e2); ps " : ";
							ps "next("; ps (sgn2__str sv); ps ") = ";
							ps (sgn2__str sv); ps " + 1;";
							pnT 2; ps "1 : next("; ps (sgn2__str sv); ps ") = ";
							ps (sgn2__str sv); ps ";";
							pnT 1; ps "esac;\n";
						with Not_found -> Err.intern ("make_smv "^sgn2__str sv)
						)
			) eqns.time_init;
		ps "\n";
		List.iter ( fun sn -> ps "JUSTICE "; ps sn) fair;
		ps "\n";
		
		let all = List.map snd (inputs@outputs@data_inputs@data_outputs) in
		List.iter (make_smv_spec all) props
	
	(* -------------------------------------------------------------------------
	filter the specs
	------------------------------------------------------------------------- *)
	let get_props sa logic =
		List.fold_right
			( fun (sp, ro) l ->
						match sp with
						| Ctl _ -> if logic = "CTL" then (sp, ro):: l else l
						| Ltl _ -> if logic = "LTL" then (sp, ro):: l else l
						| _ -> l
			) sa.sa_prps []
	
	let get_axioms sa logic =
		List.fold_right
			( fun (sp, ro) l ->
						match sp with
						| ACtl _ -> if logic = "CTL" then (sp, ro):: l else l
						(*| Ltl _ -> if logic = "LTL" then (sp, ro):: l else l *)
						| _ -> l
			) sa.sa_axms []
	
	let filter_specs specs label =
		try [List.find ( function
					| Ctl(lbl, _, _), ro -> ro_lbl2str ro lbl = label
					| Ltl(lbl, _), ro -> ro_lbl2str ro lbl = label
					| Ptl(lbl, _), ro -> ro_lbl2str ro lbl = label
					| _ -> false
				) specs
			]
		with Not_found -> []
	
	(* -------------------------------------------------------------------------
	generation of pure synchronous components
	------------------------------------------------------------------------- *)
	let make_smv_sa cid sa spec logic =
		let fn = se.file_prefix^cid^".smv" in
		let oc = open_out fn in
		push_print_fct (output_string oc);
		try
			let axioms = get_axioms sa logic
			and props =
				if spec = "All"
				then get_props sa logic
				else filter_specs (get_props sa logic) spec
			in
			make_smv sa axioms props;
			pop_print_fct ();
			close_out oc;
			ps ("\nSMV-format generated. file: "^fn^"\n")
		with x ->
				pop_print_fct ();
				close_out oc;
				raise x
	
	let make_smv_class cid spec logic =
		let sa = React2sc.rctclass2appl (i_sync_beh cid) in
		make_smv_sa (Ly.c2str cid) sa spec logic
	
	let make_smv_appl cid spec logic =
		let sa =
			match Gen_application.ac.Gen_application.ac_sca with
			| None -> Err.msg (Err.NoCode)
			| Some sa -> sa
		in
		make_smv_sa (Ly.c2str cid) sa spec logic
	
	let make_smv_model logic model spec =
		let conf_cid = type2id se.conf_class in
		let (t, s, v) = field2literal conf_cid id_timing in
		if t != t_time then Err.intern "make_smv_model:time";
		let timing = int64_2_int (some v "make_smv_model:tval") in
		if timing = 0 then Err.msg (Err.SMV "No period specified. Code generation abandoned.");
		if model = "Application" then (
			Gen_application.make ();
			make_smv_appl (type2id se.conf_class) spec logic
		) else (
			let cid = Lex_wrapper.parse_string Yacc.prs_classid model in
			make_smv_class cid spec logic
		)
	
end
