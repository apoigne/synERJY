open Ly
open P
open Ast
open Util
open Util_print
open React2sya

(* =========================================================================

GENERAL REMARKS ABOUT THE TRANSLATION SCHEME

The translation scheme is denotational in that, for every syntactic
constructor, there is an equation of the form

sem (C(term_1,..., tern_n)) inh
= sem_op_C (sem (term_1) inh,..., sem(term_n) inh)

where "sem_op_C" is the semantic operator related to the constructor "C".
The definition of the semantics or translation "sem" is done by
recursive descent along the structure of terms. "inh" is a list of
inherited attributes (e.g. comprising "system signals"),
commonly called environment in denotational semantics.

The typing of "sem" is

sem : term inh -> sya * syn

where sya is a synchronous automata, and where "syn" is a list of inherited
attributes (again comprising e, g. system signals). The structure of "inh",
"sya", and "syn" is explained in "react2sya".

We have modified this standard denotational scheme above exploiting
partial evaluation that is we specify in terms of "functionals" with
parameters "inh":

sem (C(term_1,..., tern_n)) =
= sem_C (sem (term_1),..., sem(term_n))

with the semantic functionals "sem_C" being of type

sem_C (f_1,..., f_n) = sem_op_C(f_1 inh,..., f_n inh)

i.e. sem_C : inh (inh -> sya * syn,..., inh -> sya * syn) -> sya * syn

This may be considered too abstract but has the advantage to
confine all semantical definitions to the functioals "sem_C" while
the translation scheme is quite independent of the choice of
"inh", "sya", and "syn".

Remsark: This has, in fact, been exploited several times
in that we changed the environment without touching the
denotational translation scheme. Another advantage is that it
it very easy to design translation for other languages provided
that the respective semantic functionals can be synthesised from
the given ones)

We distinguish essentially three translation functionals
according to the syntactic category of constructors:

expr2sye - translates expressions
rctstmt2sya - translates statements
rctdcl2syd - translates declarations

The three function vary in terms of "inh" and "syn". There are several
auxiliary such semantic functionals.

The semantic functionals "sem_C" are defined in file "react2sya" while
in this file only the translation scheme is specified.

========================================================================= *)

(* -------------------------------------------------------------------------
THE DENOTATIONAL TRANSLATION SCHEME
------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------
auxiliary functions for expressions
------------------------------------------------------------------------- *)
let pref xe el =
	match (xe, el) with
	| { expr = Dot(_, y) },[x] ->
			{ xe with expr = Dot(x, y) }
	| _ -> Err.intern "pref"

and inff xe el =
	match (xe, el) with
	| { expr = Dot(_ ,{ expr = Call(op, Some [_ ]); etyp = t; elbl = sp }) },[e1; e2] ->
			{ xe with expr = Dot(e1,{ expr = Call(op, Some [e2]); etyp = t; elbl = sp }) }
	| _ -> Err.intern "inff"

and dotf xe el =
	match (xe, el) with
	| { expr = Dot(_ , _ ) },[e1; e2] ->
			{ xe with expr = Dot(e1, e2) }
	| { expr = Dot(xe1 , _ ) }, [e2] ->
			{ xe with expr = Dot(xe1, e2) }
	| _,[] ->
			xe
	| _ -> Err.intern "dotf"

and assf xe el =
	match (xe, el) with
	| { expr = Assign(id, NoArr, op, _) },[e] ->
			{ xe with expr = Assign(id, NoArr, op, e) }
	| { expr = Assign(id, ADim1 _, op, _) },[e1; e2] ->
			{ xe with expr = Assign(id, ADim1 e1, op, e2) }
	| { expr = Assign(id, ADim2 _, op, _) },[e1; e2; e3] ->
			{ xe with expr = Assign(id, ADim2(e1, e2), op, e3) }
	| _ -> Err.intern "assf"

(* -------------------------------------------------------------------------
turn a state equation into a flow equation
------------------------------------------------------------------------- *)
let mk_state_equation lbl fm e init =
	let t = some e.etyp "mk_state_equation:1" in
	let cast =
		match t with
		| Simple id when id = id_float -> id_op_to_float
		| Simple id when id = id_double -> id_op_to_double
		| _ -> Err.intern "mk_state_equation:2" in
	let pre = Expr.mk_pre lbl t id_op_pre (Expr.mk_ecall lbl t fm None) in
	let past =
		match init with
		| None -> pre
		| Some init -> Expr.mk_inf lbl t id_op_fby init pre in
	( Expr.mk_inf lbl t id_op_add
			past
			(Expr.mk_inf lbl t id_op_mult
					e
					(Expr.mk_pre lbl t cast (Expr.mk_e lbl t (DeltaT)))
			)
	)
(* -------------------------------------------------------------------------
preprocessing for pre

Here the depth of pres is determined for signals and dotted signals.
The depth is part of the signal term and will be used as offset
in the signal buffer
------------------------------------------------------------------------- *)
let rec count_pres xe offset =
	match xe.expr with
	| Dot(xe1,{ expr = Call(op, Some []) }) when op = id_op_pre ->
			count_pres xe1 (offset + 1)
	| _ -> xe, offset

(* -------------------------------------------------------------------------
EXPRESSIONS

parameters:
p - position information
e - syntactic expression

the first parameter "rct" in "inh" is of type Boolean. If is is true
a syntactical Boolean expression is translated to a reactive expression.

The scheme should be fairly obvious if the expressions are understood.
In general, first the subterms are partially evaluated (usig a let),
then the semantic operators are applied.

------------------------------------------------------------------------- *)
let rec expr2sye xe =
	let some t = some t "expr2sye:typ" in
	( match some xe.etyp with
		| Array(t, DimRef r1, DimRef r2) ->
				xe.etyp <- Some (Array(t,!(!r1),!(!r2)))
		| Array(t, d1, DimRef r2) ->
				xe.etyp <- Some (Array(t, d1,!(!r2)))
		| Array(t, DimRef r1, d2) ->
				xe.etyp <- Some (Array(t,!(!r1), d2))
		| _ -> ()
	);
	match xe.expr with
	| DeltaT -> sye_deltat xe v_deltat
	| l when l = l_true -> sye_true xe
	| l when l = l_false -> sye_false xe
	| If(thenl) -> sye_exp_if xe (List.map exprthen2sye thenl)
	| Dot({ expr = DeltaT },{ expr = Call(sel, Some []) }) when sel = id_op_value ->
			sye_deltat xe v_deltat
	| Dot({ expr = Call(fid, None) },{ expr = Call(sel, Some []) })
	when sel = id_op_present || sel = id_op_value
	|| sel = id_op_timestamp || sel = id_op_dotdot
	|| sel = id_op_diagonal ->
			sye_identifier xe fid (Some sel) 0
	| Dot(xe1,{ expr = Call(op, Some [xe2]) }) ->
			let e1 = expr2sye xe1 and e2 = expr2sye xe2 in
			if op = id_op_log_and then sye_and xe e1 e2 else
			if op = id_op_log_or then sye_or xe e1 e2 else
			if op = id_op_fby then sye_arrow xe e1 e2 else
			if op = id_op_downspl && xe2.expr = l_true then e1 else
			if op = id_op_downspl then sye_when xe1 xe2 e1 e2 else
			if some xe.etyp = t_bool && op = id_op_equal
			then sye_equal xe e1 e2 else
			if some xe.etyp = t_bool && op = id_op_not_equal
			then sye_not_equal xe e1 e2 else
			if op = id_op_mult && ( is_array_typ (some xe1.etyp)
				& is_array_typ (some xe2.etyp))
			then sye_product xe e1 e2 else
				sye_exp xe (inff xe) [e1; e2]
	| Dot(xe1,{ expr = Call(op, Some []) }) when op = id_op_pre ->
	(* we only preprocess pre's for non-boolean data *)
			if xe.etyp = None || xe.etyp = Some t_bool then (
				let e1 = expr2sye xe1 in
				sye_pre xe e1 0
			) else (
				let xe2, offset = count_pres xe1 1 in
				match xe2.expr with
				| Dot({ expr = Call(fid, None) },{ expr = Call(op, Some []) }) when op = id_op_dotdot ->
						sye_identifier xe fid (Some id_op_dotdot) offset
				| Call(fid, None) ->
						sye_identifier xe fid None offset
				| _ ->
						let e2 = expr2sye xe2 in
						sye_pre xe e2 offset
			)
	| Dot(xe1,{ expr = Call(op, Some []) }) ->
			let e1 = expr2sye xe1 in
			if op = id_op_not then sye_not xe e1 else
			if op = id_op_upspl then sye_current xe e1 else
			if op = id_op_diagonal then sye_diagonal xe e1 else
				sye_exp xe (pref xe) [e1]
	| Dot(xe1, xe2) ->
			let e1 = expr2sye xe1 and e2 = exprdot2sye xe2 in
			sye_exp xe (dotf xe) [e1; e2]
	| Call(fid, None) ->
			sye_identifier xe fid None 0
	| Call(fid, Some xel) ->
			let el = List.map expr2sye xel in
			sye_exp xe (fun el -> { xe with expr = Call(fid, Some el) }) el
	| ClassCall(cid, fid, Some xel) ->
			let el = List.map expr2sye xel in
			sye_exp xe
				( fun el -> { xe with expr = ClassCall(cid, fid, Some el) }
				) el
	| ClassCall(cid, fid, None) ->
			sye_exp xe (fun _ -> xe) []
	| New(typ, xel) ->
			let el = List.map expr2sye xel in
			sye_exp xe (fun el -> { xe with expr = New(typ, el) }) el
	| Super(cid, xel) ->
			let el = List.map expr2sye xel in
			sye_exp xe (fun el -> { xe with expr = Super(cid, el) }) el
	| Assign(id, NoArr, op, e1) ->
			sye_exp xe (assf xe) [expr2sye e1]
	| Assign(id, ADim1 e1, op, e2) ->
			sye_exp xe (assf xe) [expr2sye e1; expr2sye e2]
	| Assign(id, ADim2(e1, e2), op, e3) ->
			sye_exp xe (assf xe) [expr2sye e1; expr2sye e2; expr2sye e3]
	| ArrayLiteral _ ->
			sye_array_literal xe
	| _ ->
			sye_exp xe (fun _ -> xe) []

and exprthen2sye = function
	| Then(lbl, xe1,[ExprStmt(_, xe2)]) ->
			sye_exp_then lbl xe2 (expr2sye xe1) (expr2sye xe2)
	| Else(lbl,[ExprStmt(_, xe)]) ->
			sye_exp_else lbl xe (expr2sye xe)
	| _ ->
			Err.intern "exprthen2sye"

and exprdot2sye xe =
	match xe.expr with
	| Dot(xe1, xe2) ->
			let e1 = exprdot2sye xe1 and e2 = exprdot2sye xe2 in
			sye_exp xe (dotf xe) [e1; e2]
	| Call(_, None) -> sye_exp xe (fun _ -> xe) []
	| Call _ -> expr2sye xe
	| _ -> Err.intern "exprdot2sye"

let expr2valsye xe = expr2sye xe false
(* the interpretation is not reactive *)
let expr2rctsye xe = expr2sye xe true
(* the interpretation is reactive *)
let expr2bothsye xe =
	let t = some xe.etyp "expr2bothsye" in
	expr2sye xe (t = t_bool)      (* if reactive or not depends on the type *)

(* -------------------------------------------------------------------------
CLOCKS

compute the "formal clock" from an expression.
The idea is that clocks are part of a flow as formal parameter.
Clocks are always "input".
( there should be no way to export a clock because otherwise
clock check cannot be done locally in a component)
------------------------------------------------------------------------- *)
let rec expr2frq e =
	match e.expr with
	| DeltaT ->
			expr2bf
	| l when l = l_true ->
			expr2bf
	| l when l = l_false ->
			expr2bf
	| If(thenl) ->
			expr_if2frq e.elbl (List.map rctexpthen2frq thenl)
	| Dot({ expr = Call(fid, None); etyp = etyp },{ expr = Call(sel, Some []) })
	when sel = id_op_present || sel = id_op_value || sel = id_op_timestamp || sel = id_op_dotdot ->
			expr2bf
	| Dot(e1,{ expr = Call(op, Some [e2]) }) ->
			let f1 = expr2frq e1 and f2 = expr2frq e2
			and e2' = Some e2 in
			if op = id_op_log_and then expr_and2frq e.elbl f1 f2 else
			if op = id_op_log_or then expr_or2frq e.elbl f1 f2 else
			if op = id_op_fby then expr_fby2frq e.elbl f1 f2 else
			if op = id_op_downspl && e2.expr = l_true then f1 else
			if op = id_op_downspl then expr_when2frq e.elbl e2' f1 f2 else
			if op = id_op_fby then expr_fby2frq e.elbl f1 f2 else
			if op = id_op_equal then expr_eq2frq e.elbl f1 f2 else
			if op = id_op_not_equal then expr_not_eq2frq e.elbl f1 f2 else
				expr_bin_op2frq e.elbl f1 f2
	| Dot(e1,{ expr = Call(op, Some []) }) ->
			let f = expr2frq e1 in
			if op = id_op_upspl
			then expr_current2frq e.elbl f
			else expr_un_op2frq e.elbl f
	| Dot(e1, e2) ->
			let f1 = expr2frq e1 and f2 = expr2frq e2 in
			expr_dot2frq e.elbl f1 f2
	| Call(fid, None) ->
			expr_fid2frq e.elbl fid
	| Call(fid, Some pl) ->
			expr_call2frq e.elbl (List.map expr2frq pl)
	| ClassCall(cid, fid, Some pl) ->
			expr_call2frq e.elbl (List.map expr2frq pl)
	| ClassCall(cid, fid, None) ->
			expr2bf
	| New(typ, pl) ->
			expr2bf
	| Super(cid, pl) ->
			expr_call2frq e.elbl (List.map expr2frq pl)
	| Assign(id, NoArr, op, e1) ->
			Err.intern "expr2frq"
	| _ -> expr2bf

and rctexpthen2frq = function
	| Then(_, e1,[ExprStmt(lbl, e2)]) ->
			expr_then2frq lbl (Some(expr2frq e1)) (expr2frq e2)
	| Else(_,[ExprStmt(lbl, e2)]) ->
			expr_then2frq lbl None (expr2frq e2)
	| _ -> Err.intern "rctexpthen2frq: else"

(* -------------------------------------------------------------------------
DECLARATIONS
------------------------------------------------------------------------- *)
let rctdcl2syd sg =
	(* we have to distinguish whether we have a "formal" declaration of
	* a class or a principal signal / locally declared signal *)
	( match sg with
		| VarPrm x ->
				syd_parameter x
		| VarLcl ld ->
				let e = match ld.letexpr with
					| None -> None
					| Some e -> Some (expr2bothsye e)
				in
				syd_variable ld.letent e
		| SigPrm xs ->
				syd_param_signal xs (expr2frq xs.trs_sdecl.sig_clock)
		| SigFld xs ->
				syd_param_signal xs (expr2frq xs.trs_sdecl.sig_clock)
		| SigPrc xs ->
				syd_prcp_signal xs (expr2rctsye xs.trs_sdecl.sig_clock)
		| SigLcl xs ->
				syd_lcl_signal xs (expr2rctsye xs.trs_sdecl.sig_clock)
	)
(* TODO: causality on declarations *)

let rctdcll2syd xsl =
	let dl = List.map rctdcl2syd xsl in
	syd_declarations dl

let rctlcldcll2syd xsl =
	let dl = List.map rctdcl2syd xsl in
	syd_lcl_declarations dl

(* -------------------------------------------------------------------------
STATEMENTS

parameters:
sp - position information
e - syntactic expression

Again fairly obvious. There is one hiccup since the "if - statement"
is an expression of type "unit".

Evaluation of Boolean expression in statements depend on whether they
should be translated to reactive expressions or not. The first parameter
"rct" is set respectively. To let this surface the subsequent auxiliaries
are introduced.

------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------
auxiliary functions for statements
------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------
transcription of a signal to a reactive signal
------------------------------------------------------------------------- *)
let mk_rctsig ent ~prc ~sname ~ro =
	let sdcl = Util_parse.mk_sigdecl
			ent.p_lbl ent.p_type ent.p_clock Util_parse.NoInit in
	{ trs_sid = ent.p_name;
		trs_styp = ent.p_type;
		trs_sdecl = sdcl;
		trs_principal = prc;
		trs_ro = ro;
		simulinkno = 0;
		rct_sgndcl = None;
		trs_sname = sname;
	}

let mth_formal2rctparam ro ent =
	if is_sensor_or_signal_typ ent.p_type then (
		let nm = [Call(ent.p_name, None), ent.p_type] in
		let xs = mk_rctsig ent ~prc: Visible ~sname: nm ~ro: (Some ro) in
		SigPrm xs
	) else (
		VarPrm { prm_ent = ent; prm_sgn = None }
	)

let rec mth_formals2rctparams ro = function
	| [] -> []
	| f:: fl -> (mth_formal2rctparam ro f):: (mth_formals2rctparams ro fl)

let letdcl2rctsig ld =
	let ent = ld.letent in
	if is_signal_typ ent.p_type then (
		let nm = [Call(ent.p_name, None), ent.p_type] in
		let xs = mk_rctsig ent ~prc: Invisible ~sname: nm ~ro: None in
		SigLcl xs
	) else (
		VarLcl { ld with letent = { ent with p_clock = v_true } }
	)

(* -------------------------------------------------------------------------
check existence of an else branch
------------------------------------------------------------------------- *)
let has_else_branch =
	function
	| Then(_, e, _) -> e = v_true
	| Else(_, _) -> true

(* -------------------------------------------------------------------------
checks whether a conditional statement list has a condition of type time
------------------------------------------------------------------------- *)
let has_timing cpl =
	List.exists
		( function
			| Then(_, e, _) -> some e.etyp "has_timing" = t_time
			| Else _ -> false
		) cpl

(* -------------------------------------------------------------------------
DERIVED FUNCTIONALS

we use the functionals in "react2sya" to generate new ones.
for the interpretation consult the explanation of the semantic operators
in "react2sya".
------------------------------------------------------------------------- *)
let sya_await lbl next has_timing cpl =
	sya_cancel lbl ~strongly: false ~next: (next || has_timing)
		~split: false ~timing: has_timing
		(sya_halt lbl) cpl

let add_exit exit trans =
	List.map
		( function
			| Then(lbl, e, q) -> Then(lbl, e, exit@q)
			| Else(lbl, q) -> Else(lbl, exit@q)
		) trans

let sya_sustain lbl p =
	sya_set_flow_context ~next: false
		(sya_loop lbl (sya_seq lbl p (sya_next lbl)))

let sya_during lbl p =
	sya_set_flow_context ~next: true
		(sya_loop lbl (sya_seq lbl (sya_next lbl) p))

let sya_state lbl is_cyclic has_timing sname sdo sentry sduring strans =
	sya_do_at lbl sname
		( sya_seq nolbl
				( sentry )
				( sya_cancel nolbl ~strongly: false ~next: true
						~split: is_cyclic ~timing: has_timing
						( sya_par nolbl [sdo; sya_during nolbl sduring] )
						strans
				)
		)

(* ------------------------------------------------------------------------- *)
let rec rctstmt2sya = function
	| ExprStmt(lbl,{ expr = If(sl) }) -> rctif2sya lbl sl
	| ExprStmt(lbl, e) -> rct_call2sya lbl e
	| Schedule(lbl, e) -> Err.intern "rctstmt2sya:nyi"
	| Nothing(lbl) -> sya_skip
	| Sustain(lbl, sl) -> sya_sustain lbl (rctstmtl2sya sl)
	| Halt(lbl) -> sya_halt lbl
	| Next(lbl) -> sya_next lbl
	| Emit(lbl, fe, s, i, e) ->
			( match fe with
				| FlowEq ->
						sya_update ~is_floweq: true lbl s i
							(expr2bothsye e)
				| StdEmit ->
						sya_update ~is_floweq: false lbl s i
							(expr2bothsye e)
				| StateEq init ->
						let e = mk_state_equation lbl s e init in
						sya_update ~is_floweq: true lbl s i
							(expr2bothsye e)
			)
	| FlowContext(lbl, fc) ->
			rctflowcontext lbl fc
	| Par(lbl, pl) ->
			sya_par lbl (List.map rctstmtl2sya pl)
	| RctLoop(lbl, pl) ->
			sya_loop lbl (rctstmtl2sya pl)
	| Await(lbl, next, cpl) ->
			sya_await lbl next	(has_timing cpl)(rctcancelwhen2sya cpl)
	| Cancel(lbl, strong, next, pl, cpl) ->
			sya_cancel lbl
				~strongly: strong ~next: next
				~split: false ~timing: (has_timing cpl)
				(rctstmtl2sya pl) (rctcancelwhen2sya cpl)
	| Activate(lbl, next, pl, e) ->
			sya_activate lbl next (rctstmtl2sya pl) (expr2sye e)
	| NextState(lbl, st) ->
			if st = id_exit then sya_skip else sya_jump_to lbl st
	| TextStM (lbl, a) ->
			rctautomaton2sya lbl a
	| GraphicStM (lbl, g) ->
			rct_graphic_call2sya lbl (some g.gstm_mf "GStmt")
	| LetStmt(lbl, ld) ->
			let d = rctdcl2syd (letdcl2rctsig ld)
			and p = rctstmtl2sya ld.letil in
			sya_local ~is_flwctxt: false lbl d p
	| _ -> Err.intern "rctstmt2sya"

and rctflowcontext lbl { f_dcls = dcls; f_equ = sl } =
	let dcl2rctsig =
		function
		| LetStmt(_, ld) -> letdcl2rctsig ld
		| _ -> Err.intern "rctflowcontext" in
	let dl = rctlcldcll2syd (List.map dcl2rctsig dcls)
	and p = sya_par lbl (List.map rctstmt2sya sl) in
	sya_local ~is_flwctxt: true lbl dl p

and rctstmtl2sya =
	function
	| [] -> sya_skip
	| [p] -> rctstmt2sya p
	| p:: pl -> sya_seq (Err.stmt2lbl (List.hd pl))
				(rctstmt2sya p)
				(rctstmtl2sya pl)

(* subsequently, we have two variations if the conditional
depending on whether it is either used as the standard
"if" for statements, or as a when condition for a cancel.
(cf. react2sya) The first parameter is the switch. To make
the distinction explicit we introduce auxiliary functions *)

and rctthen2cond_sya has_timing =
	function
	| Then(lbl, xe, pl) ->
			if some xe.etyp "rctthen2cond_sya" = t_time
			then (
				let e = sye_time xe has_timing (expr2sye xe) false in
				sya_then lbl e (rctstmtl2sya pl)
			) else (
				sya_then lbl (expr2rctsye xe) (rctstmtl2sya pl)
			)
	| Else(lbl, pl) ->
			sya_then lbl (expr2rctsye v_true) (rctstmtl2sya pl)

and rctif2sya lbl sl =
	let sl =
		if List.exists has_else_branch sl
		then sl
		else sl@[Else(nolbl,[Nothing(lbl)])]
	in
	sya_cond ~is_if: true (List.map (rctthen2cond_sya None) sl)

and rctcancelwhen2sya cq alpha =
	sya_cond ~is_if: false (List.map (rctthen2cond_sya alpha) cq)

(* we minimize the number of reincarnated signals.
For automata only those signals have to be reincarnated
that are "alpha" in an initially reachable state *)

and init2sya init_trans = rctif2sya nolbl init_trans

and rctstate2sya st =
	let st_do = rctstmtl2sya st.sdo
	and st_entry = rctstmtl2sya st.sentry
	and st_during = rctstmtl2sya st.sduring
	and st_trans = rctcancelwhen2sya (add_exit st.sexit st.strans) in
	sya_state st.slbl
		(Util_parse.is_state_cyclic st) (has_timing st.strans)
		st.sname st_do st_entry st_during st_trans

and rctautomaton2sya lbl a rho =
	let init_states = Util_parse.init2nextstates a in
	let state2sgn = List.map (fun st -> st.sname, Sc.next_jump()) a.a_states in
	let rho = { rho with state2sgn = state2sgn } in
	sya_automaton lbl
		~init: ( init2sya a.a_init.strans )
		~stpl: ( List.map
				( fun st ->
							List.mem st.sname init_states, rctstate2sya st
				) a.a_states
		) rho

and rct_call2sya lbl e rho =
	let cid = type2id rho.ro.trs_type in
	( match e.expr with
		| Call(id, Some el) ->
				let dcl = get_decl_after_tc cid id (List.length el) in
				( match dcl.entry with
					| Method m ->
							( match m.method_kind with
								| RctMethod(Available(sc)) ->
										if debug_level DbgParamList then (
											pn(); ps "link method: "; ps (mf2str id)
										);
										let apl = List.map expr2valsye el in
										sc_link_method ~is_node: false lbl sc el apl rho
								| RctNode(Available(sc)) ->
										if debug_level DbgParamList then (
											pn(); ps "link method: "; ps (mf2str id)
										);
										let apl = List.map expr2valsye el in
										sc_link_method ~is_node: true lbl sc el apl rho
								| RctMethod(Touched) ->
										Err.msg(Err.RecursiveCallOfReactiveMethod(cid, id))
								| RctNode(Touched) ->
										Err.msg(Err.RecursiveCallOfNode(cid, id))
								| RctMethod(NotAvailable) ->
										let rl = match m.mbody with
											| TextStmtL rl -> rl
											| _ -> Err.intern "rctstmt2sya"
										in
										m.method_kind <- RctMethod(Touched);
										if debug_level DbgParamList then (
											pn(); ps "\n\n\ncompile method: "; ps (mf2str id)
										);
										let dl = mth_formals2rctparams rho.ro m.mformals in
										let dl = rctdcll2syd dl in
										let pl = rctstmtl2sya rl in
										let sc = sc_method ~is_node: false dl pl rho in
										if debug_level DbgRctComp then (
											ps "\n\n\nMETHOD: "; psmf id;
											print_rctcomp sc
										);
										m.method_kind <- RctMethod(Available(sc));
										if debug_level DbgParamList then (
											pn(); ps "\n\n\nlink method: "; ps (mf2str id)
										);
										let apl = List.map expr2valsye el in
										sc_link_method ~is_node: false lbl sc el apl rho
								| RctNode(NotAvailable) ->
										let rl = match m.mbody with
											| TextStmtL rl -> rl
											| _ -> Err.intern "rctstmt2sya"
										in
										m.method_kind <- RctNode(Touched);
										if debug_level DbgParamList then (
											pn(); ps "\n\n\ncompile node: "; ps (mf2str id)
										);
										let dl = mth_formals2rctparams rho.ro m.mformals in
										let dl = rctdcll2syd dl in
										let pl = rctstmtl2sya rl in
										let sc = sc_method ~is_node: true dl pl rho in
										if debug_level DbgRctComp then (
											ps "\n\n\nNODE: "; psmf id;
											print_rctcomp sc
										);
										m.method_kind <- RctNode(Available(sc));
										if debug_level DbgParamList then (
											pn(); ps "\n\n\nlink node: "; ps (mf2str id)
										);
										let apl = List.map expr2valsye el in
										sc_link_method ~is_node: true lbl sc el apl rho
								| _ -> let ast = i_ast cid in
										(* This line is to highlight data methods like
										* emits. The corresponding function is declared
										* in ly.ml.
										*)
										ast.lbl2sp <- lbl2srcp_text2emit lbl ast.lbl2sp;
										sya_rct_expr lbl (expr2valsye e) rho
							)
					| _ -> Err.intern "rctstmt2sya"
				)
		| _ ->
				let ast = i_ast cid in
				(* This line is to highlight executed expressions like
				* emits. The corresponding function is declared
				* in ly.ml.
				*)
				ast.lbl2sp <- lbl2srcp_text2emit lbl ast.lbl2sp;
				sya_rct_expr lbl (expr2valsye e) rho
	)

and rct_graphic_call2sya lbl id rho =
	let cid = type2id rho.ro.trs_type in
	let dcl = get_decl_after_tc cid id 0 in
	( match dcl.entry with
		| Method m ->
				( match m.method_kind with
					| GraphicCall(Available(sc)) ->
							sc_link_method ~is_node: false lbl sc [] [] rho
					| GraphicCall(Touched) ->
							Err.msg(Err.RecursiveCallOfReactiveMethod(cid, id))
					| GraphicCall(NotAvailable) ->
							let rl = match m.mbody with
								| TextStmtL rl -> rl
								| _ -> Err.intern "rctstmt2sya"
							in
							m.method_kind <- GraphicCall(Touched);
							if debug_level DbgParamList then (
								pn(); ps "\n\n\ncompile method: "; ps (mf2str id)
							);
							let dl = mth_formals2rctparams rho.ro m.mformals in
							let dl = rctdcll2syd dl in
							let pl = rctstmtl2sya rl in
							let sc = sc_method ~is_node: false dl pl rho in
							if debug_level DbgRctComp then (
								ps "\n\n\nMETHOD: "; psmf id;
								print_rctcomp sc
							);
							sc_link_method ~is_node: false lbl sc [] [] rho
					| _ -> Err.intern "rct_graphic_call2sya: no call"
				)
		| _ -> Err.intern "rctstmt2sya"
	)

(* -------------------------------------------------------------------------
GENERATION OF COMPONENTS AND APPLICATIONS

- sc_object links a run statement to the respective component,
i.e. the component is called, the declarations are translated,
and formal parameters are updated by the actual principal
signal as defined by the signal bus

- the application is made by parallel composition of all linked
components plus a header declaration which coincides with that
of the first run statement (condition: the io of the first run
is the io of the application)

- mk_synchron generates a reactive component for each class.
the semantic function is "sc_comp". The declarations define
the formal io - parameters.
------------------------------------------------------------------------- *)
let class2simro ctype =
	let cid = type2id ctype in
	let ast = i_ast cid in
	let reset dcl =
		match dcl.entry with
		| Method k ->
				( match k.method_kind with
					| RctMethod _ -> k.method_kind <- RctMethod NotAvailable
					| RctNode _ -> k.method_kind <- RctNode NotAvailable
					| _ -> ()
				)
		| _ -> ()
	in
	List.iter reset ast.declseq;
	let sel dcl =
		if (decl2arity dcl) = - 1 then (
			match dcl.entry with
			| SigDecl sd ->
					let t = match dcl.signature with
						| { rt = t; pt = None } -> t
						| _ -> Err.intern "i_class2simro"
					in
					Some
					 { trs_sid = dcl.name;
						trs_styp = t;
						trs_sdecl = sd;
						trs_principal = Invisible;
						trs_ro = None;
						simulinkno = 0;
						rct_sgndcl = None;
						trs_sname =[Call(Ly.c2mf cid, None), ctype;
							Call(dcl.name, None), t];
					}
			| _ -> None
		) else (
			None
		)
	in
	let dcll = list_condense sel (i_ast cid).declseq in
	let ro = { trs_type = ctype;
		trs_name =[Call(Ly.c2mf cid, None), ctype];
		trs_sig = dcll;
	}
	in
	List.iter (fun s -> s.trs_ro <- Some ro) dcll;
	ro

let rec siginit2equ rosigl = function
	| [] -> []
	| ExprStmt(_, e):: sl
	-> ( match e.expr with
				| Assign(fld, NoArr, op, e) when op = id_op_assign
				-> ( match e.expr with
							| New _ -> siginit2equ rosigl sl
							| Call(param, None) -> (param, sig_try_find rosigl fld)
									:: (siginit2equ rosigl sl)
							| _ -> Err.intern "siginit2eq"
						)
				| _ -> Err.intern "siginit2eq"
			)
	| _ -> Err.intern "siginit2equ"

let rec mk_prc sid = function
	| [] -> Invisible
	| (s, p):: _ when s = sid -> Principal p
	| _:: l -> mk_prc sid l

let cstr_formal2rctparam equl ent =
	if is_sensor_or_signal_typ ent.p_type then (
		let prc = mk_prc ent.p_name equl
		and nm = [Call(ent.p_name, None), ent.p_type] in
		Some (SigPrm (mk_rctsig ent ~prc: prc ~sname: nm ~ro: None))
	) else (
		None
	)

let mk_rctclass cid =
	let ast = i_ast cid in
	let ck = ast.classkind in
	if not( is_reactive_classkind ck ) then (
		set_class_sta cid Synchron
	) else (
		let tp = Typecheck.typevarl2constraint ast in
		let ro = class2simro (Typ(cid, tp)) in
		let sfd = List.map (fun xs -> SigFld xs) ro.trs_sig in
		let cst = rctclass2constr ast in
		let equ = siginit2equ ro.trs_sig cst.csig in
		let cfr = cstr_formal2rctparam equ in
		let fpl = list_condense cfr cst.cformals in
		let pl = some cst.cactive "mk_rctclass:cstr" in
		let sfd = rctdcll2syd sfd in
		let fpl = rctdcll2syd fpl in
		let pl = rctstmtl2sya pl in
		let sc = sc_class ro ~fields: sfd ~params: fpl pl in
		if debug_level DbgRctComp then (
			pnT 1; ps "\n\n\nobject: ";
			ps (obj2str ro);
			print_rctcomp sc
		);
		set_class_sync_beh cid (Some sc);
		set_class_sta cid Synchron
	)

let rctclass2appl sc =
	let sfd = List.map (fun xs -> SigPrc xs) sc.sc_ro.trs_sig in
	let p = sc_object sc sc.sc_ro (rctdcll2syd sfd) in
	let sa = sc_appl [sc.sc_ro] [p] in
	if debug_level DbgRctComp then print_rctappl sa;
	sa

let mk_rctappl rtl =
	let mk_obj ro =
		let sc = i_sync_beh (type2id ro.trs_type) in
		let sfd = List.map (fun xs -> SigPrc xs) ro.trs_sig in
		sc_object sc ro (rctdcll2syd sfd)
	in
	let pl = List.map mk_obj rtl in
	let sa = sc_appl rtl pl in
	if debug_level DbgRctComp then print_rctappl sa;
	sa
