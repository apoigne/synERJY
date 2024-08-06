open Ly
open Sc
open Ast
open P
open Util
open Util_print

(* =========================================================================
!!!!! before inspection of the translation scheme the comments
in file "react2sc" should be read !!!!!!!
========================================================================= *)

(* =========================================================================
continue from react2sc

GENERAL REMARKS

We intend to define functions

sem_C : sf_1 ... sf_n inh -> sya * syn

where each of the sf_i's has type

sf_i : inh -> sya * syn

The nature of inherited attributes "inh" and of synthesised attribute
"syn" changes according to which kind of entity the functions are related
to. For example:

sya_sustain sp p rho alpha beta chi tau

sp - some position information (this somewhat out of the scheme,...)
p - functional (the sf_1)
rho - context (object, signal stack, frq list, mem list)

and now the inherited attributes (in curried form)

alpha - start signal (for the first instant)
beta - run signal (for later instants)
tau - preemption signal

for the result "sya * syn" we have a mixed strategy in that data are
added to global structures (those which are more or less not used for
the translation but only for reduction, causality check, etc.), and
others which directly influence the evaluation of the translation scheme.

"Global" data are e.g.

signal, register, debug equations
information about registers or actions which are exclusive

"local" data are of the form (for statements)

{ sya_alfa = "local signals for reincarnation";
sya_acts = "local actions";
sya_regs = "local registers";
sya_omga = "termination signal";
sya_taus = "preemption calls";
sya_ctrl = "control signal";
sya_inst = "code for termination behaviour, e.g instanteneous";
sya_splt = "if true splitting is needed in a loop";
}

We now proceed as follows:

1. the interface to the outside is specified
(essentially, what are data expressions. This is a sort of
parameterisation in that we could try tu use other kinds
of data presentation without much affecting the reactive translation)

2. The global data structures are defined concerning

- identifiers
- equations
- actions
- etc.

3. the components of the translation are given

- types of "local" synthesised attributes
- frequencies (clocks)
- declarations
- expressions
- infrastructure for reincarnation
- statements
- components
-- checking for conflicts / exclusion
-- generation of precedences
-- causality check
-- reduction
-- making components
- instantiation of components
-- copy mechanism
-- linking
-- "object creation"
- making the application

========================================================================= *)

(* -------------------------------------------------------------------------
extract all signal - identifier from reactive expressions
------------------------------------------------------------------------- *)
let rec rctexp2sgns =
	let rctexpl2sgns el = List.fold_left (fun l e -> (rctexp2sgns e)@l) [] el in
	function
	| TT -> []
	| FF -> []
	| S 1 -> []
	| S x -> if Sc.is_reg_or_mem_or_pre x then [] else [x]
	| CC x -> [x]
	| AAnd el -> rctexpl2sgns el
	| OOr el -> rctexpl2sgns el
	| NNot e -> rctexp2sgns e

let rec rctexp2vals =
	let rctexpl2vals el = List.fold_left (fun l e -> (rctexp2vals e)@l) [] el
	in function
	| TT -> []
	| FF -> []
	| S 1 -> []
	| S x -> if Sc.is_reg_or_mem_or_pre x then [] else
			if Sc.is_val x then [x] else
				[]
	| CC x -> [x]
	| AAnd el -> rctexpl2vals el
	| OOr el -> rctexpl2vals el
	| NNot e -> rctexp2vals e

(* =========================================================================
DATA EXPRESSIONS
The only interface to data expressions used in the translation scheme

- constructors for expressions
========================================================================= *)
module Expr

(* begin signature *)
: sig
	val mk_e : tlbl -> ttype -> tbsexpr -> texpr
	val mk_present : tlbl -> tsc_id -> texpr
	val mk_timestamp : tlbl -> tsc_id -> texpr
	val mk_sig_value : tlbl -> tsc_id -> trctsig -> int -> texpr
	val mk_value : tlbl -> ttype -> tsc_id -> texpr
	val mk_dotdot : tlbl -> ttype -> tsc_id -> trctsig -> int -> texpr
	val mk_then : tlbl -> ttype -> texpr -> texpr -> texpr
	val mk_else : tlbl -> ttype -> texpr -> texpr
	val mk_if : tlbl -> ttype -> texpr -> texpr -> texpr
	val mk_cond : tlbl -> ttype -> tsc_id -> texpr -> texpr -> texpr
	val mk_edot : tlbl -> ttype -> texpr -> texpr -> texpr
	val mk_ecall : tlbl -> ttype -> tmfid -> tactuals option -> texpr
	val mk_inf : tlbl -> ttype -> tmfid -> texpr -> texpr -> texpr
	val mk_pre : tlbl -> ttype -> tmfid -> texpr -> texpr
	val mk_not : tlbl -> texpr -> texpr
	val mk_or : tlbl -> texpr -> texpr -> texpr
	val mk_and : tlbl -> texpr -> texpr -> texpr
	val mk_equal : tlbl -> texpr -> texpr -> texpr
	val mk_not_equal : tlbl -> texpr -> texpr -> texpr
	val is_null : texpr -> bool
	
	val sgn_map : (tsc_id -> tsc_id) -> (tmfid -> tdim) option ->
	texpr -> texpr
	val star_map : (tsc_id -> tsc_id) -> (tmfid -> tdim) option ->
	texpr -> texpr
	val sig_map : (tmfid -> tbsexpr) -> texpr -> texpr
	val equal : in_class: bool -> base_clock: texpr option ->
	(tmfid -> tmfid -> bool) -> texpr -> texpr -> bool
	val expr2sgns : texpr -> tsc_id list
	val expr2vals : texpr -> tsc_id list
	
	val cll2vals : tsc_call -> tsc_id list
	val blck2vals : (tsc_exp * tsc_call) list -> tsc_id list
	
	val expr2typ : texpr -> ttype
end
(* end signature *)

=

struct
	
	let mk_e lbl t e =
		{ etyp = Some t; expr = e; elbl = lbl }
	
	let mk_present lbl s = mk_e lbl t_bool (Present s)
	let mk_timestamp lbl s = mk_e lbl t_time (Timestamp s)
	let mk_value lbl t s = mk_e lbl t (Value(s))
	let mk_sig_value lbl s xs i = mk_e lbl (sig2valtyp xs) (SigVal(s, xs, i))
	let mk_dotdot lbl t s xs i = mk_e lbl t (DotDot(s, xs, i))
	
	let mk_then lbl typ e1 e2 =
		mk_e lbl typ (If([Then(lbl, e1,[ExprStmt(lbl, e2)])]))
	let mk_else lbl typ e =
		mk_e lbl typ (If([Else(lbl,[ExprStmt(lbl, e)])]))
	let mk_if lbl typ e1 e2 =
		match e1.expr, e2.expr with
		| If(e1'), If(e2') -> mk_e lbl typ (If(e1'@e2'))
		| _ -> Err.intern "expr_if"
	let mk_cond lbl typ s e1 e2 =
		mk_if lbl typ (mk_then lbl typ (mk_present lbl s) e1) (mk_else lbl typ e2)
	
	let mk_edot lbl t e1 e2 = mk_e lbl t (Dot(e1, e2))
	let mk_ecall lbl t n p = mk_e lbl t (Call (n, p))
	let mk_inf lbl t n e1 e2 = mk_edot lbl t e1 (mk_ecall lbl t n (Some[e2]))
	let mk_pre lbl t n e1 = mk_edot lbl t e1 (mk_ecall lbl t n (Some []))
	
	let mk_not lbl e = mk_pre lbl t_bool id_op_not e
	let mk_or lbl e1 e2 = mk_inf lbl t_bool id_op_log_or e1 e2
	let mk_and lbl e1 e2 = mk_inf lbl t_bool id_op_log_and e1 e2
	let mk_equal lbl e1 e2 = mk_inf lbl t_bool id_op_equal e1 e2
	let mk_not_equal lbl e1 e2 = mk_inf lbl t_bool id_op_not_equal e1 e2
	
	let is_null = function { expr = NullObj } -> true | _ -> false
	(* -------------------------------------------------------------------------
	_map f1 f2 e ... return a copy of expr ''e'', apply function ''f'' to
	encoding of signal entities
	------------------------------------------------------------------------- *)
	let sgn_map b f g e =
		let rec sgn_map e =
			let e = match g with
				| None -> e
				| Some g ->
						( match some e.etyp "sgn_map" with
							| Array(t, DimLen 1, DimVar id) ->
									{ e with etyp = Some (Array(t, DimLen 1, g id)) }
							| Array(t, DimVar id1, DimVar id2) ->
									{ e with etyp = Some (Array(t, g id1, g id2)) }
							| _ -> e
						)
			in
			match e.expr with
			| Present(s) ->
					{ e with expr = Present(f s) }
			| Timestamp(s) ->
					{ e with expr = Timestamp(f s) }
			| SigVal(sv, xs, i) ->
					if b then (
						match xs.rct_sgndcl with
						| None -> Err.intern "sgn_map"
						| Some dcl ->
								let dcl' = { dcl with sgn = f dcl.sgn } in
								let xs' = { xs with rct_sgndcl = Some dcl' } in
								{ e with expr = SigVal(f sv, xs', i) }
					) else (
						{ e with expr = SigVal(f sv, xs, i) }
					)
			| Value(s) ->
					{ e with expr = Value(f s) }
			| DotDot(sv, xs, i) ->
					if b then (
						match xs.rct_sgndcl with
						| None -> Err.intern "sgn_map"
						| Some dcl ->
								let dcl' = { dcl with sgn = f dcl.sgn } in
								let xs' = { xs with rct_sgndcl = Some dcl' } in
								{ e with expr = DotDot(f sv, xs', i) }
					) else (
						{ e with expr = DotDot(f sv, xs, i) }
					)
			| Slice r ->
					{ e with expr = Slice { lower = sgn_map r.lower;
								upper = sgn_map r.upper }}
			| Dot(e1, e2) ->
					{ e with expr = Dot(sgn_map e1, sgn_map e2) }
			| Call(mf, None) ->
					e
			| Call(mf, Some el) ->
					{ e with expr = Call(mf, Some (sgn_map_l el)) }
			| Assign(mf, ADim1 i, a, e1) ->
					{ e with expr = Assign(mf, ADim1 (sgn_map i), a, sgn_map e1) }
			| Assign(mf, ADim2(x, y), a, e1) ->
					{ e with expr = Assign(mf, ADim2(sgn_map x, sgn_map y), a, sgn_map e1) }
			| Assign(mf, NoArr, a, e1) ->
					{ e with expr = Assign(mf, NoArr, a, sgn_map e1) }
			| ClassCall(c, x, None) ->
					{ e with expr = ClassCall(c, x, None) }
			| ClassCall(c, x, Some el) ->
					{ e with expr = ClassCall(c, x, Some (sgn_map_l el)) }
			| New(t, el) ->
					{ e with expr = New(t, sgn_map_l el) }
			| DeltaT | This | NullObj | Literal _ | IncrDecr _ | ArrayLiteral _ ->
					e
			| Cast(t, e1) ->
					{ e with expr = Cast(t, sgn_map e1) }
			| If(thenl) ->
					{ e with expr = If(List.map then_sgn_map thenl) }
			| SuperConstr _ ->
					Err.intern "expr_sgn_map: SuperConstr"
			| ThisConstr _ ->
					Err.intern "expr_sgn_map: ThisConstr"
			| Super _ ->
					Err.intern "expr_sgn_map: Super"
			| Instant ->
					Err.intern "expr_sgn_map: Instant"
			| Var _ ->
					Err.intern "expr_sgn_map: Var"
			| Offset _ ->
					Err.intern "expr_sgn_map: Offset"
		
		and then_sgn_map =
			function
			| Then(lbl1, e1,[ExprStmt(lbl2, e2)]) ->
					Then(lbl1, sgn_map e1,[ExprStmt(lbl2, sgn_map e2)])
			| Else(lbl1,[ExprStmt(lbl2, e1)]) ->
					Else(lbl1,[ExprStmt(lbl2, sgn_map e1)])
			| _ ->
					Err.intern "expr_then_sgn_map"
		
		and sgn_map_l el = List.map sgn_map el in
		sgn_map e
	
	let star_map f g e = sgn_map false f g e
	let sgn_map f g e = sgn_map true f g e
	
	let sig_map f e =
		let rec sig_map e =
			match e.expr with
			| Present _
			| Timestamp _
			| SigVal _
			| Value _
			| DotDot _ ->
					e
			| Slice r ->
					{ e with expr = Slice { lower = sig_map r.lower; upper = sig_map r.upper }}
			| Dot(e1, e2) ->
					{ e with expr = Dot(sig_map e1, sig_map e2) }
			| Call(mf, None) ->
					( try { e with expr = f mf }
					with Not_found ->
							e
					)
			| Call(mf, Some el) ->
					{ e with expr = Call(mf, Some (sig_map_l el)) }
			| Assign(mf, ADim1 i, a, e1) ->
					{ e with expr = Assign(mf, ADim1 (sig_map i), a, sig_map e1) }
			| Assign(mf, ADim2(x, y), a, e1) ->
					{ e with expr = Assign(mf, ADim2(sig_map x, sig_map y), a, sig_map e1) }
			| Assign(mf, NoArr, a, e1) ->
					{ e with expr = Assign(mf, NoArr, a, sig_map e1) }
			| ClassCall(c, x, None) ->
					{ e with expr = ClassCall(c, x, None) }
			| ClassCall(c, x, Some el) ->
					{ e with expr = ClassCall(c, x, Some (sig_map_l el)) }
			| New(t, el) ->
					{ e with expr = New(t, sig_map_l el) }
			| DeltaT | This | NullObj | Literal _ | IncrDecr _ | ArrayLiteral _ ->
					e
			| Cast(t, e1) ->
					{ e with expr = Cast(t, sig_map e1) }
			| If(thenl) ->
					{ e with expr = If(List.map then_sig_map thenl) }
			| SuperConstr _ ->
					Err.intern "expr_sig_map: SuperConstr"
			| ThisConstr _ ->
					Err.intern "expr_sig_map: ThisConstr"
			| Super _ ->
					Err.intern "expr_sig_map: Super"
			| Instant ->
					Err.intern "expr_sig_map: Instant"
			| Var _ ->
					Err.intern "expr_sig_map: Var"
			| Offset _ ->
					Err.intern "expr_sig_map: Offset"
		
		and then_sig_map = function
			| Then(lbl1, e1,[ExprStmt(lbl2, e2)]) ->
					Then(lbl1, sig_map e1,[ExprStmt(lbl2, sig_map e2)])
			| Else(lbl1,[ExprStmt(lbl2, e1)]) ->
					Else(lbl1,[ExprStmt(lbl2, sig_map e1)])
			| _ ->
					Err.intern "expr_then_sig_map"
		
		and sig_map_l el = List.map sig_map el in
		sig_map e
	
	(* -------------------------------------------------------------------------
	... checks whether expressions ''e1'' ''e2'' are equal.
	parameters:
	in_class: to see whether This is equal or not
	eq : checks equality of id's provided by the using
	environment
	------------------------------------------------------------------------- *)
	let equal ~in_class ~base_clock eq e1 e2 =
		let rec expr_eq e1 e2 =
			match e1.expr, e2.expr with
			| e11, Dot(e21,{ expr = Call(op, Some [e22]) })
			when ( match base_clock with
				| None -> false
				| Some bc -> op = id_op_downspl && expr_eq bc e22
			) ->
					expr_eq e1 e21
			| Dot(e11, e12), Dot(e21, e22) ->
					expr_eq e11 e21 && expr_eq e12 e22
			| Call(mf1, None), Call(mf2, None) ->
					eq mf1 mf2
			| Call(mf1, Some el1), Call(mf2, Some el2) ->
					eq mf1 mf2 && expr_eq_l el1 el2
			| ClassCall(c1, x1, None), ClassCall(c2, x2, None) ->
					c1 = c2 && x1 = x2
			| ClassCall(c1, x1, Some el1), ClassCall(c2, x2, Some el2) ->
					c1 = c2 && x1 = x2 && expr_eq_l el1 el2
			| New(t1, el1), New(t2, el2) ->
					t1 = t2 && expr_eq_l el1 el2
			| If(thenl1), If(thenl2) ->
					expr_then_eq_l thenl1 thenl2
			| DeltaT, DeltaT ->
					true
			| This, This ->
					in_class
			| Slice r1, Slice r2 ->
					expr_eq r1.lower r2.lower && expr_eq r1.upper r2.upper
			| Present(_), _ | Timestamp(_), _ | Value(_), _ | SigVal(_), _ | DotDot(_), _
			| _, Present(_) | _, Timestamp(_) | _, Value(_) | _, SigVal(_) | _, DotDot(_)
			| Var(_), _ | _, Var(_) | Offset(_), _ | _, Offset(_) ->
					Err.intern "expr_eq: rct"
			| ThisConstr(_), _ | _, ThisConstr(_) | SuperConstr(_), _
			| _, SuperConstr(_) | IncrDecr(_, _), _ | _, IncrDecr(_, _)
			| Assign(_, _, _, _), _ | _, Assign(_, _, _, _) ->
					Err.intern "expr_eq: not allowed"
			| e1, e2 ->
					e1 = e2 (* ConfObject, NullObj, Literal, StringLiteral *)
		
		and expr_then_eq e1 e2 =
			match e1, e2 with
			| Then(_, e11,[ExprStmt(_, e12)]), Then(_, e21,[ExprStmt(_, e22)]) ->
					expr_eq e11 e21 && expr_eq e12 e22
			| Else(_,[ExprStmt(_, e12)]), Else(_,[ExprStmt(_, e22)]) ->
					expr_eq e12 e22
			| _ ->
					Err.intern "expr_then_map"
		
		and expr_eq_l el1 el2 =
			List.fold_right
				( fun b l -> b && l
				) ( List.map2 expr_eq el1 el2 ) true
		
		and expr_then_eq_l el1 el2 =
			List.fold_right
				(fun b l -> b || l
				) (List.map2 expr_then_eq el1 el2) false
		in
		expr_eq e1 e2
	
	(* -------------------------------------------------------------------------
	expr2sgns e ... return a list of signal entities of expr ''e''
	------------------------------------------------------------------------- *)
	let expr2sgnvals ~to_val e =
		let rec expr2sgns e =
			match e.expr with
			| Present(s)
			| Timestamp(s) -> if to_val then [] else [s]
			| SigVal(sv, _, 0) -> [sv]
			| Value(sv) when Sc.is_aux sv -> [sv]
			(* pre's of signals are not considered *)
			| Dot(_,{ expr = Call(op, Some []) }) when op = id_op_pre -> []
			| DotDot(sv, _, 0) -> [sv]
			| Slice r -> exprl2sgns [r.lower; r.upper]
			| Dot(e1, e2) -> exprl2sgns [e1; e2]
			| Call(_, Some el) -> exprl2sgns el
			| ClassCall(_, _, Some el) -> exprl2sgns el
			| New(_, el) -> exprl2sgns el
			| If(thenl) -> expr_thenl2sgns thenl
			| _ -> []
		
		and exprl2sgns el =
			List.fold_left (fun l e -> l@(expr2sgns e)) [] el
		
		and expr_thenl2sgns thenl =
			List.fold_left (fun l e -> l@(expr_then2sgns e)) [] thenl
		
		and expr_then2sgns = function
			| Then(_, e1,[ExprStmt(_, e2)]) -> (expr2sgns e1)@(expr2sgns e2)
			| Else(_,[ExprStmt(_, e)]) -> expr2sgns e
			| _ -> Err.intern "expr2sgns"
		in
		expr2sgns e
	
	let expr2sgns e = expr2sgnvals ~to_val: false e
	let expr2vals e = expr2sgnvals ~to_val: true e
	
	let rec blck2vals cc = List.fold_left (fun l (_, cll) -> (cll2vals cll)@l) [] cc
	and cll2vals cll =
		match cll with
		| ActPrm(_, _, e, _, _) -> expr2vals e
		| ActInit _ -> []
		| ActAssgn(_, _, e, cc, _) -> (expr2vals e)@(blck2vals cc)
		| ActAssgnInBlck(_, _, e, _) -> expr2vals e
		| ActStmt(_, e, _) -> expr2vals e
		| ActProd(_, _, e1, e2, cc, _) -> (expr2vals e1) @(expr2vals e2)
				@(blck2vals cc)
		| ActSigInit _ -> []
		| ActEmit(_, _, _, _, Rct e, cc, _, _) -> (rctexp2vals e)@(blck2vals cc)
		| ActEmit(_, _, _, _, Val e, cc, _, _) -> (expr2vals e)@(blck2vals cc)
		| ActBlck(cc) -> blck2vals cc
		| ActSigPres _
		| ActSigVal _
		| ActSigIn _
		| ActSigOut _ -> []
		| ActBool(_, _, e, _) -> expr2vals e
		| ActTimeInit _ -> []
		| ActTimeAdd _ -> []
		| ActTimeCond(_, _, _, e, _) -> expr2vals e
		| ActDbg _ -> []
	
	let expr2typ e = some e.etyp "expr2typ"
	
end
(* end module Expr *)

let set_timestamp xs = xs.trs_sdecl.sig_tmstmp <- true

let is_principal_signal xs =
	match xs.trs_principal with
	| Principal _ -> false
	| _ -> true

let mem_add xs m =
	let sgndcl = some xs.rct_sgndcl "mem_add" in
	match sgndcl.mem with
	| None -> xs.rct_sgndcl <- Some { sgndcl with mem = Some m }
	| Some _ -> ()

let mk_sgn_dcl xs s f alpha =
	xs.rct_sgndcl <- Some { sgn = s; buf =[]; frq = f; mem = None; aa = alpha }

let cp_sgn_dcl xs xs' =
	try
		(some xs'.rct_sgndcl "cp_sgn_dcl:xs'").buf <- (some xs.rct_sgndcl "cp_sgn_dcl:xs").buf;
	with _ -> ();
			xs.rct_sgndcl <- xs'.rct_sgndcl;
			xs.trs_ro <- xs'.trs_ro

let rec add_buf_dim_h bl (b, n) =
	match bl, b with
	| [], b -> [b, n]
	| [Arbitrary, _], _ -> [b, n]
	| _, Arbitrary -> bl
	| _, DimRef r -> add_buf_dim_h bl (!(!r), n)
	| _, b -> (b, n):: bl
and add_buf_dim xs bl =
	let d = match (sig2prcsig xs).rct_sgndcl with
		| None -> Err.intern"sig2buf"
		| Some d -> d in
	List.iter (fun b -> d.buf <- add_buf_dim_h d.buf b) bl

let ro2cid ro = type2id ro.trs_type

let ro_lbll2str ro lbll =
	lst_sep2str (Err.cid_lbl2str (type2id ro.trs_type)) " [[&&]] " lbll

let check_labels ro lbs = Data_depend.check_labels (type2id ro.trs_type) lbs

let sgn2sig s xsl =
	let s = Sc.sgnorval2sgn s in
	let find xs = (some (sig2prcsig xs).rct_sgndcl "sgn2sig").sgn = s in
	List.find find xsl

let sig_find xsl id =
	sig2prcsig (List.find (fun xs -> xs.trs_sid = id) xsl)

let sig_try_find xsl id =
	try
		sig2prcsig (List.find (fun xs -> xs.trs_sid = id) xsl)
	with Not_found ->
			Err.intern ("sig_try_find "^(mf2str id))

let find_variable id dl =
	List.find
		( fun d -> match d with
					| LclFld(_, x) -> id = x.p_name
					| _ -> false
		) dl

(* -------------------------------------------------------------------------
checks whether an expr is a flow
used in pre() and current() to decide whether memories
- can be shared [returns true]
- or not [returns false]
------------------------------------------------------------------------- *)

(* ========================================================================
DATA STRUCTURES
========================================================================= *)
(* --------------------------------------------------------------------------
IHERITED ATTRIBUTES

global information stored while translating a component
- ro : reactive object ("this")
- id2sgn : id -> sgn, for jumps
-------------------------------------------------------------------------- *)
type env =
	{ cid : tclass;
		ro : trctobj;
		flds : tsc_id list;
		globs : tsc_id list;
		mutable state2sgn : (tmfid * tsc_id) list;
	}

let new_env cid ro =
	{ cid = cid;
		ro = ro;
		flds = [];
		globs = [];
		state2sgn = [];
	}

let jump_try_find rho st =
	try List.assoc st rho.state2sgn
	with Not_found ->
			Err.msg (Err.StateNotDefined(rho.cid, mf2lbl st, st))

let all_jumps rho =
	List.map (fun (_, s) -> S s) rho.state2sgn
(* --------------------------------------------------------------------------
GLOBAL SYNTHESIZED ENVIRONMENT

global information stored while translating a component / method
- sigs : declared signals
- frq_ex : expression -> frequency
- maps external expressions (ext id's replaced by sgn's)
to a frequency, is bijective
- reg_ex : expression -> register (similar)
- defs : wire / register / action equations
- dcls : internal and local declarations
- alts : alternative registers / actions
- axms : reactive axioms
- prps : reactive propositions

rs - stack of such environments
(for inline translationn of methods)
r - reference to the top element of the stack
rb - reference to the base element of a stack

be careful: subtle construction works only since because all
the mutable lists behave like push - only stacks
i.e. popping environments just gets rid of what
has been pushed within a stack element of rs
-------------------------------------------------------------------------- *)
(* absolute base clock *)
let f_tick = { ff = Sc.tick; cd = v_true; bf = None }
let f_tick' = { ff = Sc.tick'; cd = v_true; bf = None }

type rct_env =
	{ alpha : tsc_id;
		bfrq : tsc_frq;
		mutable sigs : trctsig list;
		mutable frq_ex : (texpr * tsc_frq) list;
		mutable arw_ex : ((tsc_id * tsc_id) * (tsc_id * tsc_id)) list;
		mutable defs : tsc_ctrl list;
		mutable alts : (tsc_id list * tsc_id list) list;
		mutable dpds : (tsc_id * tsc_id list) list;
		mutable dcls : tsc_lcl_dcl list;
		mutable axms : (trctspec * trctobj) list;
		mutable prps : (trctspec * trctobj) list;
	}

let rb = ref
		{ alpha = Sc.alpha; bfrq = f_tick;
			sigs =[]; frq_ex =[]; arw_ex =[];
			defs =[]; alts =[]; dpds =[]; dcls =[]; axms =[]; prps =[]
		}
let rs = ref [!rb]
let r = ref !rb

let reset_rctenv alpha =
	let ne =
		{ alpha = Sc.alpha; bfrq = f_tick;
			sigs =[]; frq_ex =[]; arw_ex =[];
			defs =[]; alts =[]; dpds =[]; dcls =[]; axms =[]; prps =[]
		}
	in
	rs := [ne];
	r := ne;
	rb := ne

let push_new_rctenv alpha f =
	let re =
		{ alpha = alpha; bfrq = f;
			sigs = !rb.sigs; frq_ex = !rb.frq_ex; arw_ex = !rb.arw_ex;
			defs =[]; alts =[]; dpds =[]; dcls =[]; axms =[]; prps =[]
		}
	in
	rs := re::!rs;
	r := re

let push_new_rctenv4node alpha f =
	let re =
		{ alpha = alpha; bfrq = f;
			sigs = []; frq_ex =[]; arw_ex =[];
			defs =[]; alts =[]; dpds =[]; dcls =[]; axms =[]; prps =[]
		}
	in
	rs := re::!rs;
	r := re

let pop_rctenv () =
	if List.length !rs > 1 then (
		rs := List.tl !rs;
		r := List.hd !rs
	) else
		Err.intern "pop_rctenv"

let amalgamate_rctenv () =
	if List.length !rs = 2 then (
		r :=
		{ !r with defs = !rb.defs @ !r.defs;
			alts = !rb.alts @ !r.alts;
			dcls = !rb.dcls @ !r.dcls;
			axms = !rb.axms @ !r.axms;
			prps = !rb.prps @ !r.prps;
		}
	) else
		Err.intern "amalgamate_rctenv"

(* internal and local declarations of fields and signals *)
let add_sig xs = !r.sigs <- xs::!r.sigs
let add_field_dcl s x = !r.dcls <- (LclFld(s, x))::!r.dcls
let add_internal_sig_dcl s t o = !r.dcls <- (IntSig(s, t, o))::!r.dcls
let add_local_sig_dcl xs = !r.dcls <- (LclSig(xs))::!r.dcls

let clocks_equal_h id id' =
	try
		let xs1 = sig_find !r.sigs id
		and xs2 = sig_find !r.sigs id' in
		sig2sname xs1 = sig2sname xs2
	with Not_found -> id = id'

let are_clocks_equal e1 e2 =
	Expr.equal ~in_class: true ~base_clock: None clocks_equal_h e1 e2

let are_clocks_equal_for_node_call bc e1 e2 =
	Expr.equal ~in_class: true ~base_clock: bc clocks_equal_h e1 e2

let are_clocks_equal_for_signal_bus ro1 ro2 e1 e2 =
	let equal_ids id1 id2 =
		try
			let xs1 = sig_find (ro2sigs ro1) id1
			and xs2 = sig_find (ro2sigs ro2) id2 in
			sig2sname xs1 = sig2sname xs2
		with Not_found -> id1 = id2
	in
	Expr.equal ~in_class: false ~base_clock: None equal_ids e1 e2

let frq_add e f =
	!r.frq_ex <- (e, f)::!r.frq_ex

let frq_find e =
	if e.expr = l_true then !r.bfrq else
		let _, f =
			List.find
				( fun (e', _) -> are_clocks_equal e e'
				) !r.frq_ex
		in
		f

(* functions related to definitions *)
let def_ext k xs e =
	let s = sig2sgn xs in
	let d =
		{ sc_knd = k;
			sc_sgn = s;
			sc_def = e;
			sc_act = None;
			sc_sig = Some xs;
			sc_occ = 0;
		}
	in
	if debug_level DbgRctEqu then print_rctdef k d;
	!r.defs <- d::!r.defs

let def_add k x e =
	let d =
		{ sc_knd = k;
			sc_sgn = x;
			sc_def = e;
			sc_act = None;
			sc_sig = None;
			sc_occ = 0;
		}
	in
	if debug_level DbgRctEqu then print_rctdef k d;
	!r.defs <- d:: !r.defs

let def_or k x e =
	try
		let d = List.find (fun d -> d.sc_sgn = x) !r.defs in
		d.sc_def <- OOr[d.sc_def; e];
		if debug_level DbgRctEqu then (
			pnT 2; print_sgn x; ps " += "; print_rctexp e
		);
	with Not_found ->
			let d =
				{ sc_knd = k;
					sc_sgn = x;
					sc_def = e;
					sc_act = None;
					sc_sig = None;
					sc_occ = 0;
				}
			in
			if debug_level DbgRctEqu then print_rctdef k d;
			!r.defs <- d:: !r.defs

let def_act k x e a =
	let c, a =
		match a with
		| ActBlck([c, a]) -> c, a
		| _ -> TT, a in
	let d =
		{ sc_knd = k;
			sc_sgn = x;
			sc_def = if c = TT then e else AAnd[e; c];
			sc_act = Some a;
			sc_sig = None;
			sc_occ = 0;
		} in
	if debug_level DbgRctEqu then print_rctdef k d;
	!r.defs <- d:: !r.defs

let def_iter f = List.iter f !r.defs

(* debug definitions *)
let mk_dbg lbl e rho =
	if lbl = nolbl || se.target_sys <> Simulation
	then []
	else
		let d = Sc.next_dbg() in
		def_act Dbg d e (ActDbg([lbl], rho.ro));
		[d]

(* access functions *)
let all_mems() =
	(List.filter (fun d -> d.sc_knd = Mem || d.sc_knd = Nxt) !r.defs)
let all_dbgs() =
	List.filter (fun d -> d.sc_knd = Dbg) !r.defs

let def_get x =
	List.find (fun d -> d.sc_sgn = x) !r.defs

(* Storing alternative actions and registers *)
let rec alts_add_l_h l1 = function
	| [] -> []
	| [[]] -> []
	| [l2] -> [l1, l2]
	| l2:: ll -> (l1, l2):: (alts_add_l_h l1 ll)
and alts_add_l = function
	| [] -> []
	| [l] -> []
	| []:: ll -> alts_add_l ll
	| l1:: ll -> List.rev_append (alts_add_l_h l1 ll) (alts_add_l ll)

let alts_add l1 l2 =
	if not (l1 = [] || l2 = [])
	then !r.alts <- (l1, l2):: (!r.alts)

(* -------------------------------------------------------------------------
projections to data and signal expressions

Invariant: all expressions are either "data" expressions or
"reactive" expressions. In case of Booleans an expression may have
either form depending on whether the context is "reactive" or not.
------------------------------------------------------------------------- *)
let vexp2expr = function
	| Val x -> x
	| Rct x -> pn (); Err.intern ("vexp2expr: "^(rctexp2str x))

let vexp2rct = function
	| Rct x -> x
	| Val x -> pn (); Err.intern ("vexp2rct: "^(expr2str x))

(* -------------------------------------------------------------------------
auxiliary: make a signal or expression causal
------------------------------------------------------------------------- *)
let sgn2cc s = CC s
let rec exp2cc alpha = function
	| FF | TT -> FF
	| S s -> if s = alpha then FF else CC s
	| CC s -> if s = alpha then FF else CC s
	| NNot e -> exp2cc alpha e
	| AAnd el -> exps2cc alpha el
	| OOr el -> exps2cc alpha el
and exps2cc alpha el = OOr(List.rev_map (exp2cc alpha) el)

(* -------------------------------------------------------------------------
ENVIRONMENT

global data:

tick : basic periodic signal
alpha : global alpha
beta : global beta
tau : global tau

context

- is_appl : = true <=> reduction in context of application,
CC(x) -> FF

------------------------------------------------------------------------- *)
(* modes of operation
Appl - application
Class ro - component of reactive object "ro" *)

type mode_t = Appl of trctobj list | Class of trctobj	| Meth of trctobj | Node of trctobj

type cntxt_t = { mutable mode : mode_t; }

let cntxt = { mode = Appl[]; }

let cntxt_is_appl () = match cntxt.mode with Appl _ -> true | _ -> false
let cntxt_is_class () = match cntxt.mode with Class _ -> true | _ -> false
let cntxt_is_meth () = match cntxt.mode with Meth _ -> true | _ -> false
let cntxt_is_node () = match cntxt.mode with Node _ -> true | _ -> false

(* --------------------------------------------------------------------------
Reduce reactive expressions
-------------------------------------------------------------------------- *)

module RctExp :

sig

	val reduce : tsc_exp -> tsc_exp

end

=

struct
	let rec red_list ff tt = function
		| [] -> []
		| e:: el ->
				let e = reduce e
				and el = red_list ff tt el in
				if e = tt || el = [tt] then [tt] else
				if e = ff then el else e:: el
	and red_or el =
		match red_list FF TT el with
		| [] -> FF
		| [TT] -> TT
		| [e] -> e
		| [x; NNot y] -> if x = y then TT else OOr[x; NNot y]
		| [NNot x; y] -> if x = y then TT else OOr[NNot x; y]
		| [x; y] -> if x = y then x else OOr[x; y]
		| el -> OOr el
	and red_and el =
		match red_list TT FF el with
		| [] -> TT
		| [FF] -> FF
		| [e] -> e
		| [S x; NNot(S y)] when Sc.is_reg x && y = Sc.beta -> FF
		| [NNot(S x); S y] when x = Sc.beta && Sc.is_reg y -> FF
		| [x; NNot y] -> if x = y then FF else AAnd[x; NNot y]
		| [NNot x; y] -> if x = y then FF else AAnd[NNot x; y]
		| [S x; S y] ->
				if ( x = Sc.beta && Sc.is_reg y && cntxt_is_appl() )
				then S y
				else if Sc.is_reg x && y = Sc.beta && cntxt_is_appl()
				then S x
				else AAnd[S x; S y]
		| [x; y] -> if x = y then x else AAnd[x; y]
		| el -> AAnd el
	and red_not e =
		match reduce e with
		| TT -> FF
		| FF -> TT
		| NNot e -> e
		| e -> NNot e
	and reduce = function
		| NNot e -> red_not e
		| OOr el -> red_or el
		| AAnd el -> red_and el
		| S x when x = Sc.tick -> TT
		| S x when x = Sc.tau -> FF
		| CC x -> if cntxt_is_appl() then FF else CC x
		| e -> e
	and rctexp_red_l el = List.rev_map reduce el
end
(* =========================================================================
SYNCHRONOUS AUTOMATA
========================================================================= *)

(* -------------------------------------------------------------------------
TYPES

syd_t : type of synthesised information for declarations
sye_t : type of synthesised information for expressions
sya_t : type of synthesised information for statements

translation invariant for all types:
the inherited attribute "alpha" and the synthesised attribute "omga"
are used as external interfaces for the control flow.

This may only be a "fictituous" control flow as in case of expressions
where only causality is considered.

The "locals" in all types are used for reincarnation. Only those signals
are condsidered local which must be reincarnated in case.
(see reincarnation)

------------------------------------------------------------------------- *)
type syd_t =
	{ syd_fmls: tsc_formal list;    (* formal parameters               *)
		syd_glbs: tsc_id list;        (* global signal                   *)
		syd_alfa: tsc_id list;        (* local signals to be splitted    *)
		syd_acts: tsc_id list;        (* local action triggers           *)
		syd_caus: tsc_exp;            (* "causal" omega                  *)
	}

type sye_t =
	{ sye_xe : texpr;          (* syntactic expression                *)
		sye_val : tsc_vexp;       (* value                               *)
		sye_freq: tsc_frq;        (* clock, base clock if statement      *)
		sye_alfa: tsc_id list;    (* local signals to be splitted        *)
		sye_blck: (tsc_exp * tsc_call) list;            (* action block    *)
		sye_acts: tsc_id list;    (* local action triggers               *)
	}

type sya_t =
	{ sya_alfa: tsc_id list;   (* local signals to be splitted         *)
		sya_acts: tsc_id list;   (* local action triggers                *)
		sya_regs: tsc_id list;   (* registers                            *)
		sya_omga: tsc_exp;       (* omega                                *)
		sya_taus: tsc_exp list;  (* disjunction of preemption conditions *)
		sya_ctrl: tsc_exp list;  (* control signals or registers         *)
		sya_inst: tsc_id;        (* 0 - definitely instanteneous,
		1 - eventually instanteneous
		2 - terminates at later instant
		3 - non - terminating *)
		sya_splt: bool           (* flag for local declaration           *)
	}

(* =========================================================================
FREQUENCIES
========================================================================= *)
(* ------------------------------------------------------------------------
each frequency is specified by

ff - the frequency signal.
cd - syntactic denotation for a frequency.
bf - base frequency

We have the following functions:

- frq_eq : (f1, f2) -> f1 equal to f2
-> Error message else

- frq_eq_l same on lists

- frq_new_param ( c: syntactic_exp, e: value_expression) -> frq
called if the user specifies parameters with a clock
for periodic signals

generates a new frequency only if no entry is found for the key "e"
in "frq_ex".
(We have a bijection between frequencies and their id (.ff) )
minimises the number of frequency id's

- frq_new ( c: syntactic_exp, e: value_expression, alpha, delta, tau)
-> (frq, omega, actions, locals)

as frq_new_param, however is called if a "real"clock is constructed,
for principal signals, locally declared signal, and for implicit clocks
(defined by "at")

has more side effects in that local signals (for reincarnation), actions
etc. are generated

------------------------------------------------------------------------- *)
let frq_eq rho lbl op f1 f2 =
	if f1.ff = f2 .ff
	then f1
	else
		Err.msg (Err.ClocksInconsistent(op, rho.cid, lbl))

let rec frq_eq_l rho lbl op cl =
	match cl with
	| [] -> Err.intern "frq_eq_l"
	| [f] -> f
	| f:: l -> frq_eq rho lbl op f (frq_eq_l rho lbl op l)

(* parameter clocks
i.e. clocks introduced for signal parameters
the clocks depend on the structure of expressions only.

frq_new_param - defines a new parameter clock depending on the syntactic
expression "e" and its parameter clock "f" *)

let frq_new_param e bf =
	let f = { ff = Sc.next_inp(); cd = e; bf = Some bf } in
	frq_add e f;
	f

(* semantic operators to determine the parameter clock of an expression *)

let expr2bf rho = !r.bfrq

let expr_dot2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for '.' at" f1 f2

let expr_fid2frq lbl id rho =
	try
		let xs = sig_find !r.sigs id in
		sig2frq xs
	with Not_found ->
			!r.bfrq

let expr_and2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for && at" f1 f2

let expr_or2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for || at" f1 f2

let expr_eq2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for == at" f1 f2

let expr_not_eq2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for != at" f1 f2

let expr_fby2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for -> at" f1 f2

let expr_bin_op2frq lbl f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	frq_eq rho lbl "for a binary operator at" f1 f2

let expr_when2frq lbl cc f1 f2 rho =
	let f1 = f1 rho and f2 = f2 rho in
	match cc with
	| None -> !r.bfrq
	| Some e ->
			try frq_find e
			with Not_found ->
					let bf = frq_eq rho lbl "for 'when' at" f1 f2 in
					frq_new_param e bf

let expr_current2frq lbl f rho =
	let f = f rho in
	match f.bf with
	| None -> Err.msg (Err.CurrentOnBaseClock(rho.cid, lbl))
	| Some bf -> bf

let expr_pre2frq lbl f1 rho =
	f1 rho

let expr_un_op2frq lbl f1 rho = f1 rho

let expr_call2frq lbl fl rho =
	let fl = List.map (fun f -> f rho) fl in
	frq_eq_l rho lbl "for a call at" fl

let expr_if2frq lbl fl rho =
	let fl = List.rev_map (fun f -> f rho) fl in
	frq_eq_l rho lbl "for 'if' at" fl

let expr_then2frq lbl f1 f2 rho =
	match f1 with
	| None -> f2 rho
	| Some f1 ->
			let f1 = f1 rho and f2 = f2 rho in
			frq_eq rho lbl "for 'then' at" f1 f2

(* creates "real" clocks, i.e. the syntactic clock expressions are
semantically interpreted *)

let frq_new xe e rho =
	let ff = Sc.next_frq() and bff = e.sye_freq.ff in
	let acts =
		match e.sye_val with
		| Rct FF ->
				def_add Eqn ff FF;
				[]
		| Rct TT ->
				def_add Eqn ff (S bff);
				[]
		| Rct e' ->
				if e.sye_blck = [] then (
					def_add Eqn ff (AAnd[S bff; e']);
					[]
				) else (
					let a = Sc.next_act() in
					def_act Eqn a (S bff) (ActBlck(e.sye_blck));
					def_add Eqn ff (AAnd[S bff; e']);
					[a]
				)
		| Val e' ->
				let a = Sc.next_act() and d = Sc.next_delta() in
				if e.sye_blck = [] then (
					def_act Eqn a (S bff) (ActBool([xe.elbl], d, e', rho.ro))
				) else (
					def_act Eqn a (S bff)
						(ActBlck(e.sye_blck@[TT, ActBool([xe.elbl], d, e', rho.ro)]))
				);
				let ff = Sc.next_frq() in
				def_add Eqn ff (AAnd[S bff; S d]);
				[a]
	in
	let f = { ff = ff; cd = xe; bf = Some e.sye_freq } in
	frq_add xe f;
	f, e.sye_alfa, e.sye_acts@acts

(* =========================================================================
DECLARATIONS

types of declarations

parameters for class generation:

- signals
- flows

are used for fields and parameters of constructors.
Fields are always principal, parameters may be principal, then they
can used only in a constructor, or related to a field. In the latter
case the parameter refers to the respective field as principal signal,
and the internal signals of the parameter is that of the field.
Other constructor parameters are not considered in the reactive part.

parameters for method generation:

- signals
- flows
- variables

used for method parameters. Here all signals and flows are prinicipal.

principals for application generation:

- signals
- flows

for principals we have to generate initial values and frequencies, and
to allocate memory for the reactive interpretation of Boolean signals and
flows.

parameters:
lbl - source label
xs - syntactic reactive signal
f - clock expression
ro - "this" pointer
alpha -
kappa - causality (used to trace causality in the clock
expressions, see syd_dep_declarations)
========================================================================= *)
let syd_parameter x rho alpha mu tau kappa =
	let s = Sc.next_var() in add_field_dcl s x.prm_ent;
	{ syd_fmls = [VarPrm { x with prm_sgn = Some s }];
		syd_alfa = [];
		syd_glbs = [];
		syd_acts = [];
		syd_caus = kappa;
	}

let syd_variable x init rho alpha mu tau kappa =
	let lbl = x.p_lbl in
	let s = Sc.next_var() in add_field_dcl s x;
	let alfa, acts, kappa' =
		match init with
		| None ->
				let a = Sc.next_act() in
				def_act Eqn a (OOr[S alpha; kappa])
					(ActInit([lbl], s, x.p_type, rho.ro));
				[a],[a], CC a
		| Some e ->
				let e = e rho alpha mu tau in
				match e.sye_val with
				| Rct v ->
						def_add Eqn s v;
						[],[], FF
				| Val v ->
						let a = Sc.next_act() in
						def_act Eqn a (S alpha) (ActAssgn([lbl], s, v,[], rho.ro));
						[a],[a], CC a
	in
	{ syd_fmls = [];
		syd_alfa = alfa;
		syd_glbs = [];
		syd_acts = acts;
		syd_caus = kappa';
	}

let syd_param_signal xs f rho alpha mu tau kappa =
	(* add the declaration to the environment *)
	add_sig xs;
	(* compute the semantics of the flow declaration *)
	if is_principal_signal xs then (
		let t = sig2typ xs and e = sig2clck xs in
		(* compute the parameter frequency *)
		let f = if e.expr = l_true then !r.bfrq else
				try frq_find e
				with Not_found ->
						let f = f rho in
						frq_new_param e f
		in
		(* set the semantics of the parameter declaration *)
		let s = if is_sensor_typ t then Sc.next_inp() else Sc.next_ext() in
		mk_sgn_dcl xs s f (if cntxt_is_node() then mu else alpha);
		if debug_level DbgRctDecl then (
			pn(); ps "parameter"; ps (sig2str xs)
		);
		(* set the semantics of the parameter declaration *)
		if is_signal_typ t then (
			if is_delayed_typ t
			then def_ext Mem xs FF
			else def_ext Eqn xs FF;
		);
		if sig2valtyp xs <> Null && cntxt_is_class() then (
			if is_sensor_typ t then (
				let a = Sc.next_act() in
				def_act Eqn a FF (ActSigIn([nolbl], xs, rho.ro));
			) else (
				let a1 = Sc.next_act() and a2 = Sc.next_act() in
				def_act Eqn a1 FF (ActSigIn([nolbl], xs, rho.ro));
				def_act Eqn a2 FF (ActSigOut([nolbl], xs, rho.ro));
			);
		);
		{ syd_fmls = [SigPrm xs];
			syd_alfa = [];
			syd_glbs = ( if sig2valtyp xs = Null
					then [s]
					else [s; Sc.sgn2val s]
				);
			syd_acts = [];
			syd_caus = kappa;
		}
	) else (
		(* the internal interpretation of a parameter and its respective
		field is the same. *)
		let xs' = sig2prcsig xs in
		cp_sgn_dcl xs xs';
		(* If a field is initialized by a constructor parameter,
		the clocks of a constructor parameter and the field must
		be equal. *)
		if debug_level DbgParamList then (
			let psig = sig2str xs'
			and asig = sig2str xs in
			pn(); ps "Clock Check for Parameters: ";
			pnT 3; ps " formal "; ps asig; ps " at "; print_expr (sig2clck xs);
			pnT 3; ps " field  "; ps psig; ps " at "; print_expr (sig2clck xs');
		);
		let c = sig2clck xs and c' = sig2clck xs' in
		if not (are_clocks_equal c c') then (
			let psg = sig2str xs
			and asg = sig2str xs' in
			Err.msg (Err.InconsistentClocks(c2str rho.cid, asg, psg))
		);
		{ syd_fmls = [SigPrm xs];
			syd_alfa = [];
			syd_glbs = [];
			syd_acts = [];
			syd_caus = kappa;
		}
	)

let syd_allocate_bool_mem xs rho alpha =
	let s = sig2sgn xs and t = sig2typ xs in
	let sv = Sc.sgn2val s in
	if is_sensor_typ t then (
		def_add Mem sv (S sv)
	) else (
		let m = Sc.next_mem() in
		def_add Mem m (S sv);
		if is_delayed_typ t
		then def_add Mem sv (AAnd[NNot(S s); S m])
		else def_add Eqn sv (AAnd[NNot(S s); S m])
	)

let syd_lcl_signal xs f rho alpha mu tau kappa =
	let lbl = sig2pos xs in
	(* add the signal declaration to the environment *)
	add_sig xs;
	(* compute the semantics of the signal declaration *)
	let t = sig2typ xs and vt = sig2valtyp xs in
	(* compute the frequency *)
	let f, alfa1, acts1 =
		let xc = sig2clck xs in
		try
			let f = frq_find xc in
			f,[],[]
		with Not_found ->
				let f = f rho !r.bfrq.ff mu tau in
				frq_new xc f rho in
	let s = Sc.next_lcl() in
	if debug_level DbgRctDecl then (
		pn(); ps "local "; ps (sig2str xs)
	);
	mk_sgn_dcl xs s f (if cntxt_is_node() then mu else alpha);
	add_local_sig_dcl xs;
	(* set the semantics of the parameter declaration *)
	if is_delayed_typ t
	then def_ext Mem xs FF
	else def_ext Eqn xs FF;
	(* define the initial value *)
	let alfa2, acts2, kappa' =
		if vt = Null || vt = t_bool then [],[], kappa else (
			let a = Sc.next_act() in
			def_act Eqn a (OOr[S alpha; kappa]) (ActSigInit([lbl], Sc.sgn2val s, xs, rho.ro));
			[a],[a], CC a
		)
	in
	(* allocate memory for the reactive interpretation of Boolean signals *)
	if vt = t_bool then syd_allocate_bool_mem xs rho alpha;
	{ syd_fmls = [];
		syd_alfa = s:: (Sc.sgn2val s):: alfa1@alfa2;
		syd_glbs = (if vt = Null then [s] else [s; Sc.sgn2val s]);
		syd_acts = acts1@acts2;
		syd_caus = kappa';
	}

let syd_prcp_signal xs f rho alpha mu tau kappa =
	if debug_level DbgRctDecl then (
		pn(); ps "principal "; ps (sig2str xs)
	);
	let lbl = sig2pos xs in
	(* add the signal declaration to the environment *)
	add_sig xs;
	(* compute the semantics of the signal declaration *)
	if is_principal_signal xs then (
		let t = sig2typ xs and typ = sig2valtyp xs in
		let is_sensor = is_sensor_typ t in
		(* compute the frequency *)
		let f, alfa1, acts1 =
			let xc = sig2clck xs in
			if is_sensor && xc.expr <> l_true then (
				Err.msg (Err.InputSignalNotOnBaseClock(rho.cid, lbl, sig2id xs))
			);
			try
				let f = frq_find xc in
				f,[],[]
			with Not_found ->
					let f = f rho !r.bfrq.ff mu tau in
					frq_new xc f rho
		in
		let s = if is_sensor then Sc.next_inp() else Sc.next_ext() in
		mk_sgn_dcl xs s f alpha;
		if not is_sensor then (
			if is_delayed_typ t
			then def_ext Mem xs FF
			else def_ext Eqn xs FF
		);
		(* allocate memory for the reactive interpretation of Boolean signals *)
		if typ = t_bool then syd_allocate_bool_mem xs rho alpha;
		{ syd_fmls = [];
			syd_alfa = alfa1;
			syd_glbs = ( if sig2valtyp xs = Null
					then [s]
					else [s; Sc.sgn2val s]
				);
			syd_acts = acts1;
			syd_caus = kappa;
		}
	) else (
		(* check clock equality with principal signal *)
		let xs' = sig2prcsig xs in
		if debug_level DbgParamList then (
			let psig = sig2str xs'
			and asig = sig2str xs in
			pn(); ps "Clock Check for Constructor Calls: ";
			pnT 3; ps " formal "; ps asig; ps " at "; print_expr (sig2clck xs);
			pnT 3; ps " actual "; ps psig; ps " at "; print_expr (sig2clck xs')
		);
		let c = sig2clck xs and c' = sig2clck xs' in
		if not (are_clocks_equal_for_signal_bus (sig2ro xs) (sig2ro xs') c c') then (
			let cid = ro2cid (sig2ro xs)
			and psg = sig2str xs
			and asg = sig2str xs' in
			Err.msg
				(Err.InconsistentClocksInConstructorCall(cid, lbl, asg, psg))
		);
		{ syd_fmls = [];
			syd_alfa = [];
			syd_glbs = [];
			syd_acts = [];
			syd_caus = kappa;
		}
	)

(* get all the declarations keeping the order *)
let rec syd_lcl_declarations dl rho alpha mu tau kappa =
	match dl with
	| [] ->
			{ syd_fmls = [];
				syd_alfa = [];
				syd_glbs = [];
				syd_acts = [];
				syd_caus = kappa;
			}
	| d:: dl ->
			let d = d rho alpha mu tau kappa in
			let dl = syd_lcl_declarations dl rho alpha mu tau d.syd_caus in
			{ syd_fmls = d.syd_fmls@dl.syd_fmls;
				syd_alfa = d.syd_alfa@dl.syd_alfa;
				syd_glbs = d.syd_glbs@dl.syd_glbs;
				syd_acts = d.syd_acts@dl.syd_acts;
				syd_caus = dl.syd_caus;
			}

let syd_declarations dl rho alpha mu tau =
	syd_lcl_declarations dl rho alpha mu tau FF

(* =========================================================================
TRANSLATION OF EXPRESSIONS

parameters:
chi - activation condition
kappa - for causality flow
ro - "this" pointer

Typical evaluation of expressions is of the follwing form:
- all partially evaluated subexpressions are provided with the proper
parameters. note that the control is encoded via the "alpha" - parameter
- then the synthesised information is computed. There are two cases:
-- reactive context: in case of Boolean s only
rective expressions are generated
-- otherwise : data expressions are generated
according to demand, either reactive equations are generated, or data
actions with respective triggers.

Via "kappa" and "sya_caus" we trace causalities in terms. With "kappa"
we inherit a causality trace , with "sya_caus" we pass it on.
========================================================================= *)

(* for data expressions:
if the context is reactive, a Boolean trigger signal is generated
otherwise the data expression is just recomposed *)

let rec sye_exp_l el rho chi mu tau =
	match el with
	| [] ->
			[],[],[],[]
	| e:: el ->
			let e' = e rho chi mu tau in
			let e'', alfa, acts, cc = sye_exp_l el rho chi mu tau in
			e':: e'', e'.sye_alfa@alfa, e'.sye_acts@acts, e'.sye_blck@cc

let sye_exp xe op el rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let el = List.rev_map (fun e -> e false) el in
	let el, alfa1, acts1, cc1 = sye_exp_l el rho chi mu tau in
	let c =
		if el = []
		then !r.bfrq
		else frq_eq_l rho lbl "for an operator" (List.rev_map (fun e -> e.sye_freq) el) in
	let xe = op (List.rev_map (fun e -> vexp2expr e.sye_val ) el) in
	let v, alfa2, cc2 =
		if typ = t_bool && rct then (
			let d = Sc.next_delta () in
			Rct (S d),[d],[TT, ActBool([lbl], d, xe, rho.ro)]
		) else (
			Val (Expr.mk_e lbl typ xe.expr),[],[]
		)
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = c;
		sye_alfa = alfa1@alfa2;
		sye_blck = cc1@cc2;
		sye_acts = acts1;
	}

let sye_array_literal xe rct rho chi mu tau =
	{ sye_xe = xe;
		sye_val = Val xe;
		sye_freq = !r.bfrq;
		sye_alfa = [];
		sye_blck = [];
		sye_acts = [];
	}

let sye_diagonal xe e rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	( match typ with
		| Array(_, d1, d2) when dim2value d1 = dim2value d2 -> ()
		| _ -> Err.msg(Err.DiagonalOperator(ro2cid rho.ro, lbl))
	);
	let e = e false rho chi mu tau in
	let v = vexp2expr e.sye_val in
	{ sye_xe = xe;
		sye_val = Val (Expr.mk_pre lbl typ id_op_diagonal v);
		sye_freq = e.sye_freq;
		sye_alfa = e.sye_alfa;
		sye_blck = e.sye_blck;
		sye_acts = e.sye_acts;
	}

(* Two cases:
- pure (None) = presence of a signal
-- reactive : the internal name of the signal
-- otherwise: the data expression of the presence signal
- valued (Some key) -
-- reactive : a Boolean trigger signal is generated
-- otherwise: the data expression of the valued signal

clocks:
- (delayed) signals = tick (not yet efficient for local)
- flows
-- value: frequency as declared
-- presence: one step down in the frequency stack
*)

(* The offset is the number of pre's applied to a term.
* If offset = 0 no memory is nedded. Otherwise we have to define memory.
*
* Unfortunately, this cannot be restricted to the sye_pre operator.
* The problem is that we cannot distinguish between signals
* and identifiers on the syntactic level within flow context. Resolution
* takes place only on the sementic level when the declarations are
* evaluated. Hence memory has to be introduced at different places.
*
* Compare olso: react2sc.ml
*)

let sye_signal xe xs sel offset rct rho chi mu tau =
	let lbl = xe.elbl and typ = sig2valtyp xs
	and f = sig2frq xs and s = sig2sgn xs in
	let v, alfa, acts =
		match sel with
		| None ->
				let sv = Sc.sgn2val s in
				if typ = t_bool && rct then (
					(* the action is used for error handling
					* It stores the position of the error
					* The action is a placeholder. It is used only until causality
					* check takes place. Then the action is substituted by the
					* signal. This has to be handled very carefully in context
					* where an explicit check of Rct(S _) is used
					* cf. sye_pre *)
					let a = Sc.next_act() in
					def_act Eqn a (S sv) (ActSigVal([lbl], sv, xs, rho.ro));
					Rct (S a),[a],[a]
				) else (
					add_buf_dim xs [DimLen 1, offset];  (* XXX *)
					Val (Expr.mk_sig_value lbl sv xs offset),[],[]
				)
		| Some sel ->
				if sel = id_op_timestamp then set_timestamp xs;
				if sel = id_op_present && rct then (
					let a = Sc.next_act() in
					def_act Eqn a (S s) (ActSigPres([lbl], s, xs, rho.ro));
					Rct(S a),[a],[a]
				) else if sel = id_op_present then (
					Val (Expr.mk_present lbl s),[],[]
				) else if sel = id_op_value then (
					let sv = Sc.sgn2val s in
					if typ = t_bool && rct then (
						let a = Sc.next_act() in
						def_act Eqn a (S sv) (ActSigVal([lbl], sv, xs, rho.ro));
						Rct (S a),[a],[a]
					) else (
						Val (Expr.mk_sig_value lbl (Sc.sgn2val s) xs 0),[],[]
					)
				) else if sel = id_op_timestamp then (
					set_timestamp xs;
					Val (Expr.mk_timestamp lbl s),[],[]
				) else if sel = id_op_dotdot then (
					match Expr.expr2typ xe with
					| Array(_, DimLen 1, d) ->
							add_buf_dim xs [d, offset];   (* XXX *)
							let sv = Sc.sgn2val s in
							Val (Expr.mk_dotdot lbl (Expr.expr2typ xe) sv xs offset),[],[]
					| _ -> Err.intern "sye_signal:2"
				) else (
					Err.intern "sye_signal:3"
				)
	and c = if sel = None then f else !r.bfrq in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = c;
		sye_alfa = alfa;
		sye_blck = [];
		sye_acts = acts;
	}

let sye_local_field xe d buf_size rct rho chi mu tau =
	let lbl = xe.elbl in
	match d with
	| LclFld(s, x) ->
			let v, alfas, acts, ca =
				if x.p_type = t_bool && rct then (
					let d = Sc.next_delta () in
					Rct (S d),[],[],
					[TT, ActBool([lbl], d, Expr.mk_value lbl x.p_type s, rho.ro)]
				) else (
					if buf_size = 0 then (
						Val (Expr.mk_value lbl x.p_type s),[],[],[]
					) else (
						let m = Sc.next_pre() in
						add_internal_sig_dcl m x.p_type buf_size;
						let a1 = Sc.next_act() in
						def_act Nxt a1 (S chi) (ActAssgn([lbl], m, Expr.mk_value lbl x.p_type s,[], rho.ro));
						Val (Expr.mk_value lbl x.p_type m),[m; a1],[a1],
						[S mu, ActInit([lbl], m, x.p_type, rho.ro)]
					)
				)
			in
			{ sye_xe = xe;
				sye_val = v;
				sye_freq = !r.bfrq;
				sye_alfa = alfas;
				sye_blck = ca;
				sye_acts = acts;
			}
	| _ -> Err.intern "sye_variable"

let sye_field xe id buf_size rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let v, alfas, acts, ca =
		if typ = t_bool && rct then (
			let d = Sc.next_delta () in
			Rct (S d),[],[],
			[TT, ActBool([lbl], d, Expr.mk_ecall lbl typ id None, rho.ro)]
		) else (
			if buf_size = 0 then (
				Val (Expr.mk_ecall lbl typ id None),[],[],[]
			) else (
				let m = Sc.next_pre() in
				add_internal_sig_dcl m typ buf_size;
				let a1 = Sc.next_act() in
				def_act Nxt a1 (S chi) (ActAssgn([lbl], m, Expr.mk_ecall lbl typ id None,[], rho.ro));
				Val (Expr.mk_value lbl typ m),[m; a1],[a1],
				[S mu, ActInit([lbl], m, typ, rho.ro)]
			)
		)
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = !r.bfrq;
		sye_alfa = alfas;
		sye_blck = ca;
		sye_acts = acts;
	}

let sye_identifier xe id sel offset rct rho chi mu tau =
	try
		let xs = sig_find !r.sigs id in
		sye_signal xe xs sel offset rct rho chi mu tau
	with Not_found ->
			try
				let d = find_variable id !r.dcls in
				sye_local_field xe d offset rct rho chi mu tau
			with Not_found ->
					sye_field xe id offset rct rho chi mu tau

let sye_deltat xe vrt rct rho chi mu tau =
	{ sye_xe = xe;
		sye_val = Val vrt;
		sye_freq = !r.bfrq;
		sye_alfa = [];
		sye_blck = [];
		sye_acts = [];
	}

(* the operator is used for expression of type time. The parameter
"timestamp" is of type "tsc_id option":
None - no timestamp stored
Some s - timestamp stored in s
*)

let sye_time xe timestamp e rct rho chi mu tau =
	let lbl = xe.elbl in
	match timestamp with
	| None -> Err.intern "sye_time"
	| Some sv ->
			let e = e false rho chi mu tau in
			match e.sye_val with
			| Rct _ -> Err.intern "sye_time"
			| Val v -> (* set the timing condition: s + e *)
					let d = Sc.next_delta() and a = Sc.next_act() in
					def_act Eqn a (S chi)
						(ActBlck(e.sye_blck@[TT, ActTimeCond([lbl], d, sv, v, rho.ro)]));
					{ sye_xe = xe;
						sye_val = Rct (S d);
						sye_freq = !r.bfrq;
						sye_alfa = a:: e.sye_alfa;
						sye_blck = [];
						sye_acts = a:: e.sye_acts;
					}

let sye_true xe rct rho chi mu tau =
	let v = if rct then Rct TT else Val v_true in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = !r.bfrq;
		sye_alfa = [];
		sye_blck = [];
		sye_acts = [];
	}

let sye_false xe rct rho chi mu tau =
	let v = if rct then Rct FF else Val v_false in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = !r.bfrq;
		sye_alfa = [];
		sye_blck = [];
		sye_acts = [];
	}

let sye_not xe e rct rho chi mu tau =
	let lbl = xe.elbl in
	let e = e rct rho chi mu tau in
	let v = match e.sye_val with
		| Rct FF -> Rct TT
		| Rct TT -> Rct FF
		| Rct v -> Rct (NNot v)
		| Val v -> Val (Expr.mk_not lbl v) in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = e.sye_freq;
		sye_alfa = e.sye_alfa;
		sye_blck = e.sye_blck;
		sye_acts = e.sye_acts;
	}

let sye_and xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl in
	let e1 = e1 rct rho chi mu tau in
	(* evaluation of "e2" causally depends on that of "e1" *)
	let e2 = e2 rct rho chi mu tau in
	let v = match e1.sye_val , e2.sye_val with
		| Rct FF, Rct _ -> Rct FF
		| Rct _ , Rct FF -> Rct FF
		| Rct TT, Rct v2 -> Rct v2
		| Rct v1, Rct TT -> Rct v1
		| Rct v1, Rct v2 -> Rct (AAnd[v1; v2])
		| Val v1, Val v2 -> Val (Expr.mk_and lbl v1 v2)
		| _ -> Err.intern "sye_and"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for && at" e1.sye_freq e2.sye_freq;
		sye_alfa = e1.sye_alfa@e2.sye_alfa;
		sye_blck = e1.sye_blck@e2.sye_blck;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

let sye_or xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl in
	let e1 = e1 rct rho chi mu tau in
	(* evaluation of "e2" causally depends on that of "e1" *)
	let e2 = e2 rct rho chi mu tau in
	let v = match e1.sye_val , e2.sye_val with
		| Rct TT, Rct _ -> Rct TT
		| Rct _ , Rct TT -> Rct TT
		| Rct FF, Rct v2 -> Rct v2
		| Rct v1, Rct FF -> Rct v1
		| Rct v1, Rct v2 -> Rct (OOr[v1; v2])
		| Val v1, Val v2 -> Val (Expr.mk_or lbl v1 v2)
		| _ -> Err.intern "sye_or"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for || at" e1.sye_freq e2.sye_freq;
		sye_alfa = e1.sye_alfa@e2.sye_alfa;
		sye_blck = e1.sye_blck@e2.sye_blck;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

let sye_equal xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl in
	let e1 = e1 rct rho chi mu tau in
	(* evaluation of "e2" causally depends on that of "e1" *)
	let e2 = e2 rct rho chi mu tau in
	let f1 = e1.sye_freq and f2 = e2.sye_freq in
	let v, alfa, ca =
		match e1.sye_val , e2.sye_val with
		| Rct v1, Rct v2 ->
				Rct (OOr[AAnd[v1; v2]; NNot(OOr[v1; v2])]),[],[]
		| Val v1, Val v2 ->
				if rct then (
					let d = Sc.next_delta () in
					Rct (S d),[d],
					[TT, ActBool([lbl], d, Expr.mk_equal lbl v1 v2, rho.ro)]
				) else (
					Val (Expr.mk_equal lbl v1 v2),[],[]
				)
		| _ -> Err.intern "sye_equal"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for == at" f1 f2;
		sye_alfa = e1.sye_alfa@e2.sye_alfa@alfa;
		sye_blck = e1.sye_blck@e2.sye_blck@ca;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

let sye_not_equal xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl in
	let e1 = e1 rct rho chi mu tau in
	(* evaluation of "e2" causally depends on that of "e1" *)
	let e2 = e2 rct rho chi mu tau in
	let f1 = e1.sye_freq and f2 = e2.sye_freq in
	let v, alfa, ca =
		match e1.sye_val , e2.sye_val with
		| Rct v1, Rct v2 ->
				Rct (OOr[AAnd[v1; v2]; NNot(OOr[v1; v2])]),[],[]
		| Val v1, Val v2 ->
				if rct then (
					let d = Sc.next_delta () in
					Rct (S d),[d],
					[TT, ActBool([lbl], d, Expr.mk_not_equal lbl v1 v2, rho.ro)]
				) else (
					Val (Expr.mk_not_equal lbl v1 v2),[],[]
				)
		| _ -> Err.intern "sye_equal"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for != at" f1 f2;
		sye_alfa = e1.sye_alfa@e2.sye_alfa@alfa;
		sye_blck = e1.sye_blck@e2.sye_blck@ca;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

(* conditional is split into three parts:
sye_expr_then - for a then branch
sye_expr_else - for the else branch
sye_exp_if - for the recursion on many then's

in case of the reactive interpretation, we use the "tau" attribute
to transfer the "not condition true" to the next branch
frequencies of "e1" and "e2" should be equal *)

let sye_exp_then lbl xe e1 e2 rct rho chi mu tau =
	let typ = Expr.expr2typ xe in
	let e1 = e1 rct rho chi mu tau in
	(* evaluation of "e2" causally depends on that of "e1" *)
	let e2 = e2 rct rho chi mu tau in
	let v, cond = match e1.sye_val , e2.sye_val with
		| Rct v1, Rct v2 -> Rct v2,[v1]
		| Val v1, Val v2 -> Val (Expr.mk_then lbl typ v1 v2),[]
		| _ -> Err.intern "sye_exp_then"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for 'then' at" e1.sye_freq e2.sye_freq;
		sye_alfa = e1.sye_alfa@e2.sye_alfa;
		sye_blck = e1.sye_blck@e2.sye_blck;
		sye_acts = e1.sye_acts@e2.sye_acts;
	},
	cond

let sye_exp_else lbl xe e rct rho chi mu tau =
	let typ = Expr.expr2typ xe in
	let e = e rct rho chi mu tau in
	let v = match e.sye_val with
		| Rct v -> Rct v
		| Val v -> Val (Expr.mk_else lbl typ v)
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = e.sye_freq;
		sye_alfa = e.sye_alfa;
		sye_blck = e.sye_blck;
		sye_acts = e.sye_acts;
	},
	[]

let rec sye_exp_if xe ce rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	match ce with
	| [] -> Err.intern "sye_exp_if: []"
	| [e] -> let e, _ = e rct rho chi mu tau in e
	| e:: ce ->
			let e1, cond = e rct rho chi mu tau in
			let e2 = sye_exp_if xe ce rct rho chi mu tau in
			let v, alfa = match e1.sye_val , e2.sye_val with
				| Rct v1, Rct v2 ->
						let y = Sc.next_gamma() in
						def_add Eqn y (OOr cond);
						Rct (OOr[AAnd[S y; v1]; AAnd[NNot(S y); v2]]),[y]
				| Val v1, Val v2 ->
						Val (Expr.mk_if lbl typ v1 v2),[]
				| _ -> Err.intern "sye_exp_if"
			in
			alts_add (List.filter Sc.is_act e1.sye_alfa)
				(List.filter Sc.is_act e2.sye_alfa);
			{ sye_xe = xe;
				sye_val = v;
				sye_freq = frq_eq rho lbl "for 'if' at" e1.sye_freq e2.sye_freq;
				sye_alfa = alfa@e1.sye_alfa@e2.sye_alfa;
				sye_blck = e1.sye_blck@e2.sye_blck;
				sye_acts = e1.sye_acts@e2.sye_acts;
			}

(* We make a distinction between Nxt equations and Mem equations.
* Values for Nxt equations can only occur in right hand sides of Eqn
* equations, hence we do not need to introduce a copy m__n (see. util_gen
* and gen_rct_code), and a recopy m = m__n. For Mem equations the left
* hand side may occur on the right hand side of Mem equations, hence here
* the __n etc. is needed.
* We restrict the Nxt equations and the corresponding handling to Valued
* expressions only. For reactive expressions we use mem equations. The reason
* is that for the more efficient jmp - bit and expr - bit style the copying
* m = m__n is efficiently done wordwise, and that it is difficult to single
* out those equations which would qualify as Nxt equations (there is the
* particular point that in the expr_if statement we would have to distinguish
* the case m and m__n which implies that the interface between reactive
* expressions and data expressions has to be changed). However, this
* implies that at several places care has to be taken. we discriminite not
* only Nxt equations but have a category of "pre" signals as well. One should
* be careful that mem and pre signals are handled uniformly except for a few
* well defined places
*)

let sye_pre xe e buf_size rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let e = e rct rho chi mu tau in
	let f = e.sye_freq in
	let v, alfa, acts, ca =
		match e.sye_val with
		| Rct(S a) when Sc.is_act a ->
		(* at this point we use that the only action in this context
		* is of type ActSigVal which is used to mark positions of
		* where the value of boolean signals is used, cf. sye_signal.
		* This, of course, is a dangerous invariant, however
		* the cheapest envisible way to deal with this problem.
		* Here we have to know the signal to allocate memory globally
		*)
				let act = try some (def_get a).sc_act "sye_pre:1"
					with _ -> Err.intern "sye_pre:2"
				in
				( match act with
					| ActSigVal(_, sv, xs, _) ->
							( match sig2mem xs with
								| None ->
										let m = Sc.next_mem() in mem_add xs m;
										def_add Mem m
											(OOr[AAnd[S f.ff; S sv]; AAnd[NNot(S f.ff); S m]]);
										Rct (AAnd[NNot(S (sig2alpha xs)); S m]),[m],[],[]
								| Some m -> Rct (S m),[],[],[]
							)
					| _ -> Err.intern "sye_pre:3"
				)
		| Rct v ->
				let m = Sc.next_mem() in
				def_add Mem m (AAnd[S chi; OOr[AAnd[S f.ff; v];
						AAnd[NNot(S f.ff); S m]]]);
				let a = Sc.next_act() in
				def_act Eqn a (AAnd[S chi; S f.ff]) (ActBlck(e.sye_blck));
				Rct (AAnd[NNot(S mu); S m]),[m],[],[]
		| Val v ->
				let m = if typ = t_bool then Sc.next_mem() else Sc.next_pre() in
				add_internal_sig_dcl m typ buf_size;
				let a1 = Sc.next_act() in
				def_act Nxt a1 (AAnd[S chi; S f.ff])
					(ActAssgn([lbl], m, v, e.sye_blck, rho.ro));
				Val (Expr.mk_value lbl typ m),[m; a1],[a1],
				[S mu, ActInit([lbl], m, typ, rho.ro)]
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = f;
		sye_alfa = alfa@e.sye_alfa;
		sye_blck = ca;
		sye_acts = acts@e.sye_acts;
	}

(* value is that of e1 at the first "tick" of the clock, and that of
e2 otherwise.
frequencies of "e1" and "e2" should be equal *)

let sye_arrow xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let e1 = e1 rct rho chi mu tau in
	let e2 = e2 rct rho chi mu tau in
	let v1 = e1.sye_val and f1 = e1.sye_freq
	and v2 = e2.sye_val and f2 = e2.sye_freq in
	let (fa, fb), alfa =
		try (List.assoc (f1.ff, mu) !r.arw_ex),[]
		with Not_found ->
		(* "fh" is a control bit to distinguish between the first
		"tick" of a frequency and later ones within a "mode" mu.
		It is reset to "false" if the mode signal mu is present. *)
				let g = Sc.next_gamma()
				and fa = Sc.next_gamma()
				and fb = Sc.next_gamma()
				and fh = Sc.next_reg() in
				def_add Eqn g (OOr[S mu; S fh]);
				def_add Eqn fa (AAnd[S f1.ff; S g]);
				def_add Eqn fb (AAnd[S f1.ff; NNot(S g)]);
				def_add Mem fh (AAnd[NNot(S tau); NNot (S fa); S g]);
				!r.arw_ex <- ((f1.ff, mu), (fa, fb))::!r.arw_ex;
				(fa, fb),[g; fa; fb; fh]
	in
	let v, ca =
		match v1, v2 with
		| Rct v1', Rct v2' ->
		(* "downsampling" is conjunction with "fa" and "fb" *)
				let v = Rct (OOr[AAnd[S fa; v1']; AAnd[S fb; v2']]) in
				v,[]
		| Val v1', Val v2' ->
		(* same with actions, notice the flow of control *)
				let s = Sc.next_aux() in add_internal_sig_dcl s typ 0;
				(* a local data signal is defined. It is triggered to have
				the value of "e1" in case of "fa", and that of "e2"
				otherwise *)
				Val (Expr.mk_value lbl typ s),
				[S fa, ActAssgnInBlck([lbl], s, v1', rho.ro);
				S fb, ActAssgnInBlck([lbl], s, v2', rho.ro)]
		| _ -> Err.intern "sye_arrow: rct"
	in
	(* actions in the branches are known to be alternative *)
	alts_add e1.sye_acts e2.sye_acts;
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for -> at" f1 f2;
		sye_alfa = alfa@e1.sye_alfa@e2.sye_alfa;
		sye_blck = e1.sye_blck@e2.sye_blck@ca;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

let sye_arrow xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let e1 = e1 rct rho chi mu tau in
	let e2 = e2 rct rho chi mu tau in
	let v1 = e1.sye_val and f1 = e1.sye_freq
	and v2 = e2.sye_val and f2 = e2.sye_freq in
	let (fa, fb), alfa =
		try (List.assoc (f1.ff, mu) !r.arw_ex),[]
		with Not_found ->
		(* "fh" is a control bit to distinguish between the first
		"tick" of a frequency and later ones within a "mode" mu.
		It is reset to "false" if the mode signal mu is present. *)
				let g = Sc.next_gamma()
				and fa = Sc.next_cond()
				and fb = match v1 with
					| Rct _ -> Sc.next_gamma()
					| Val _ -> Sc.next_cond()
				and fh = Sc.next_reg() in
				def_add Eqn g (OOr[S mu; S fh]);
				def_add Eqn fa (AAnd[S f1.ff; S g]);
				def_add Eqn fb (AAnd[S f1.ff; NNot(S g)]);
				def_add Mem fh (AAnd[NNot(S tau); NNot (S fa); S g]);
				!r.arw_ex <- ((f1.ff, mu), (fa, fb))::!r.arw_ex;
				(fa, fb),[g; fa; fb; fh]
	in
	let v =
		match v1, v2 with
		| Rct v1', Rct v2' ->
		(* "downsampling" is conjunction with "fa" and "fb" *)
				let v = Rct (OOr[AAnd[S fa; v1']; AAnd[S fb; v2']]) in
				v
		| Val v1', Val v2' ->
				Val (Expr.mk_cond lbl typ fa v1' v2')
		| _ -> Err.intern "sye_arrow: rct"
	in
	(* actions in the branches are known to be alternative *)
	alts_add e1.sye_acts e2.sye_acts;
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for -> at" f1 f2;
		sye_alfa = alfa@e1.sye_alfa@e2.sye_alfa;
		sye_blck = e1.sye_blck@e2.sye_blck;
		sye_acts = e1.sye_acts@e2.sye_acts;
	}

(* upsampling: changes value at a clock tick and keeps it *)

let sye_current xe e rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let e = e rct rho chi mu tau in
	let f = e.sye_freq in
	let bf =
		match f.bf with
		| None -> Err.msg(Err.CurrentOnBaseClock(rho.cid, lbl))
		| Some bf -> bf
	in
	let v, alfa, ca =
		match e.sye_val with
		| Rct v ->  (* in "s" the new value is computed, and stored in "m" *)
				let s = Sc.next_aux() and m = Sc.next_pre() in
				def_add Eqn s (AAnd[S chi; OOr[AAnd[S f.ff; v];
						AAnd[NNot(OOr[S mu; S f.ff]); S m]]]);
				def_add Mem m (S s);
				Rct(S s),[m; s],[]
		| Val v ->
				let sv = Sc.next_aux() in add_internal_sig_dcl sv typ 0;
				Val (Expr.mk_value lbl typ sv),[],
				[S f.ff, ActAssgnInBlck([lbl], sv, v, rho.ro)]
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = bf;
		sye_alfa = alfa@e.sye_alfa;
		sye_blck = e.sye_blck@ca;
		sye_acts = e.sye_acts;
	}

(* downsampling defines a new clock (via "xc" and "e2"), and computes
the value of "e1" on this clock. "e2" is evaluated in a reactive context
(hence "e2 true"), i.e. frequencies are always "reactive".
frequencies of "e1" and "e2" should be equal *)

let sye_when xe xc e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl in
	if xc = v_true then Err.msg(Err.WhenOnTrue(rho.cid, lbl)) else
	if xc = v_false then Err.msg(Err.WhenOnFalse(rho.cid, lbl)) else
		(* generate the new frequency *)
		let f, alfa, acts =
			try frq_find xc,[],[]
			with Not_found ->
					let e2 = e2 true rho chi mu tau in
					frq_new xc e2 rho
		in
		let e1 = e1 rct rho chi mu tau in
		let f1 = e1.sye_freq in
		let _ = frq_eq rho lbl "for 'when' at" f1 (some f.bf "sye_when") in
		{ sye_xe = xe;
			sye_val = e1.sye_val;
			sye_freq = f;
			sye_alfa = alfa@e1.sye_alfa;
			sye_blck =
				( if e1.sye_blck = []
					then []
					else [S f.ff, ActBlck(e1.sye_blck)]
				);
			sye_acts = e1.sye_acts@acts;
		}

let sye_product xe e1 e2 rct rho chi mu tau =
	let lbl = xe.elbl and typ = Expr.expr2typ xe in
	let e1 = e1 rct rho chi mu tau in
	let e2 = e2 rct rho chi mu tau in
	let f1 = e1.sye_freq and f2 = e2.sye_freq in
	let v1 = e1.sye_val and v2 = e2.sye_val in
	let v, a =
		match v1, v2 with
		| Val v1', Val v2' ->
				let acc = Sc.next_aux() in add_internal_sig_dcl acc typ 0;
				let a = Sc.next_act() in
				def_act Eqn a (AAnd[S chi; S f1.ff])
					(ActProd([lbl], acc, v1', v2', e1.sye_blck@e2.sye_blck, rho.ro));
				Val (Expr.mk_value lbl typ acc), a
		| Rct _, _ | _, Rct _ ->
				Err.intern "sye_scalprod"
	in
	{ sye_xe = xe;
		sye_val = v;
		sye_freq = frq_eq rho lbl "for '*' at" f1 f2;
		sye_alfa = a:: e1.sye_alfa@e2.sye_alfa;
		sye_blck = [];
		sye_acts = a:: e1.sye_acts@e2.sye_acts;
	}

(* =========================================================================
TRANSLATION OF STATEMENTS

parameters:
lbl - source label,
rho - context
alpha - start
beta - run
tau - preemption
chi - stack of clocks (Not yet used)
mu - needed for proper initialisation of local signals,
pre, and arrow

========================================================================= *)

(* -------------------------------------------------------------------------
auxiliary: some foldings used several times
------------------------------------------------------------------------- *)
let all_alfa pl = List.fold_left (fun l p -> l@p.sya_alfa) [] pl
let all_acts pl = List.fold_left (fun l p -> l@p.sya_acts) [] pl
let all_regs pl = List.fold_left (fun l p -> l@p.sya_regs) [] pl
let all_ctrl pl = List.fold_left (fun l p -> p.sya_ctrl@l) [] pl
let all_splt pl = List.fold_left (fun l p -> l || p.sya_splt) false pl

(* -------------------------------------------------------------------------
REINCARNATION

The general idea is that a split of reactive equations into "alpha"
and "beta" reactive equations are needed. To minimize effort,
we only synthesise a list of only those signals which are known that
they may react in the first instant. The attribute is "sya_alfa".

There are two functionalities:

- splitting : an expression / equation etc is split into the "alpha" and
"beta" part
- starring : a "local" signal "S s" is replaced by the disjunction
"OOr[S s*;S s]"

Which of the functions is chosen deopends on the kind of signal:

registers, global signals are starred if in "sya_alfa"
other signals are split.

------------------------------------------------------------------------- *)
(* signals which do not need to be split/reincarnated *)
let is_beta_sgn x =
	Sc.is_beta x || Sc.is_reg_or_mem_or_pre x || Sc.is_eta x
let is_inp_sgn x =
	Sc.is_inp x || Sc.is_delta x

let sya_absplit alpha beta tau globs p =
	(* distinguish between ActInit actions and others, needed for
	proper causality when reincarnating valued signals *)
	let inits = ref [] and others = ref [] in
	(* cross reference list for reincarnations *)
	let sgn2star = ref [] in
	let rec star x =
		let x' = Sc.sgnorval2sgn x in
		try let x'' = List.assoc x' !sgn2star in
			if Sc.is_val x then Sc.sgn2val x'' else x''
		with Not_found ->
				let x'' = Sc.next_sgn x' in
				sgn2star := (x', x'')::!sgn2star;
				if Sc.is_val x then Sc.sgn2val x'' else x''
	and star_sgn x = if is_beta_sgn x || is_inp_sgn x then x else
		if List.mem x p.sya_alfa then star x else
			x
	and exp_split = function
		| S x ->
				if x = Sc.tick then (TT, TT) else
				if x = Sc.tick' then (S x, S x) else
				if x = alpha then (S x, FF) else
				if x = beta then (S x, S x) else
				if x = tau then (S x, S x) else
				if is_beta_sgn x then (FF , S x) else
				if List.mem x globs then (S x, S x) else
				if not (List.mem x p.sya_alfa) then (FF, S x) else
					S (star x), S x
		| CC x ->
				if x = Sc.tick then Err.intern "exp_split" else
				if x = Sc.tick' then Err.intern "exp_split" else
				if x = alpha then (CC x, FF) else
				if x = beta then (CC x, CC x) else
				if x = tau then (CC x, CC x) else
				if is_beta_sgn x then (FF, CC x) else
				if List.mem x globs then (CC x, CC x) else
				if not (List.mem x p.sya_alfa) then (FF, CC x) else
					( CC (star x), CC x)
		| NNot e -> let (a, b) = exp_split e in (NNot a, NNot b)
		| AAnd el -> let (a, b) = exp_split_l el in (AAnd a, AAnd b)
		| OOr el -> let (a, b) = exp_split_l el in (OOr a, OOr b)
		| e -> (e, e)
	and exp_split_l el = List.split (List.map exp_split el)
	and exp_star = function
		| S x ->
				if is_beta_sgn x || is_inp_sgn x || x = alpha then S x else
				if not (List.mem x p.sya_alfa) then S x else
					OOr[S (star x); S x]
		| CC x ->
				if is_beta_sgn x || is_inp_sgn x || x = alpha then CC x else
				if not (List.mem x p.sya_alfa) then CC x else
					OOr[CC (star x); CC x]
		| NNot e -> NNot (exp_star e)
		| AAnd el -> AAnd (exp_star_l el)
		| OOr el -> OOr (exp_star_l el)
		| e -> e
	and exp_star_l el = List.map exp_star el
	and star_def x =
		if is_inp_sgn x || Sc.is_beta x then () else
			try
				let d = def_get x in
				let e = RctExp.reduce (exp_star d.sc_def) in
				if debug_level DbgRctStar then (
					pn(); pnT 2; ps "star "; print_sgn x;
					pnT 3; ps "to_star = "; print_rctexp d.sc_def;
					pnT 3; ps "starred = "; print_rctexp e
				);
				d.sc_def <- e
			with Not_found -> ()  (* just a hack, status of states *)
	and split_def d =
		let x = d.sc_sgn in
		let e', e'' = exp_split d.sc_def in
		let e' = RctExp.reduce e'
		and e'' = RctExp.reduce e'' in
		if debug_level DbgRctSplit then (
			pn(); pnT 1; print_sgn x;
			pnT 3; ps "to_split = "; print_rctexp d.sc_def;
			pnT 4; ps "split";
			pnT 5; print_rctexp e';
			pnT 5; print_rctexp e''
		);
		if Sc.is_reg_or_mem_or_pre x || Sc.is_dbg x then (
			d.sc_def <- match e', e'' with
			| FF, _ -> e''
			| _ , FF -> e'
			| _ -> OOr[e'; e'']
		) else (
			( match d.sc_act with
				| None ->
						def_add d.sc_knd (star x) e'
				| Some c ->
						if e'' = FF then d.sc_act <- None;
						let x' = star x in
						if e' = FF then (
							def_add d.sc_knd x' (FF)
						) else (
							List.iter
								( fun (a, x'') ->
											if List.mem x'' (Expr.cll2vals c) then (
												let a' = star a in
												if debug_level DbgRctSplit then (
													pnT 5; print_sgn x; ps " < "; print_sgn a'
												);
												try let d' = def_get a' in
													d'.sc_def <- OOr[d'.sc_def; CC x];
													alts_add [x; a] [a']
												with Not_found -> Err.intern "split_def"
											)
								) !inits;
							def_act d.sc_knd x' (OOr[e'; CC x]) (star_call c)
						)
			);
			d.sc_def <- e''
		)
	and star_expr e = Expr.star_map star_sgn None e
	and star_blck bl =
		List.map (fun (e, c) -> fst(exp_split e), star_call c) bl
	and star_vexp = function
		| Rct e -> Rct (exp_star e)
		| Val e -> Val (star_expr e)
	and star_call = function
		| ActPrm (lbl, id, e, id', ro) -> ActPrm (lbl, id, star_expr e, id', ro)
		| ActInit (lbl, x, t, ro) -> ActInit(lbl, x, t, ro)
		| ActAssgn (lbl, s, e, cc, ro) -> ActAssgn (lbl, star_sgn s, star_expr e, star_blck cc, ro)
		| ActAssgnInBlck(lbl, s, e, ro) -> ActAssgnInBlck(lbl, star_sgn s, star_expr e, ro)
		| ActStmt (lbl, e, ro) -> ActStmt(lbl, star_expr e, ro)
		| ActProd (lbl, a, e1, e2, cc, ro) -> ActProd(lbl, a, star_expr e1, star_expr e2, star_blck cc, ro)
		| ActBlck(acts) -> ActBlck(star_blck acts)
		| ActSigInit(lbl, sv, xs, ro) -> ActSigInit(lbl, star_sgn sv, xs, ro)
		| ActEmit(lbl, f, sv, i, e, cc, xs, ro) -> ActEmit(lbl, f, star_sgn sv, i, star_vexp e, star_blck cc, xs, ro)
		| ActSigPres(lbl, s, xs, ro) -> ActSigPres (lbl, star_sgn s, xs, ro)
		| ActSigVal(lbl, s, xs, ro) -> ActSigVal(lbl, star_sgn s, xs, ro)
		| ActSigIn(lbl, xs, ro) -> ActSigIn (lbl, xs, ro)
		| ActSigOut(lbl, xs, ro) -> ActSigOut (lbl, xs, ro)
		| ActBool(lbl, b, e, ro) -> ActBool(lbl, star b, star_expr e, ro)
		| ActTimeInit (lbl, s, ro) -> ActTimeInit (lbl, s, ro)
		| ActTimeAdd (lbl, s, ro) -> ActTimeAdd (lbl, s, ro)
		| ActTimeCond (lbl, b, s, e, ro) -> ActTimeCond (lbl, star_sgn b, s, star_expr e, ro)
		| ActDbg (l, ro) -> ActDbg (l, ro)
	and star_freq f =
		{ ff = star_sgn f.ff;
			cd = f.cd;
			bf = match f.bf with
				| None -> None
				| Some f -> Some (star_freq f)
		}
	and star_dcl = function
		| IntSig(s, t, n) ->
				if List.mem s p.sya_alfa
				then !r.dcls <- IntSig(star_sgn s, t, n):: !r.dcls
		| d -> ()
	and star_alpha_sgn x = if Sc.is_reg_or_mem_or_pre x then x else star x
	and star_add l x = if List.mem x p.sya_alfa then (star x):: x:: l else x:: l
	and filter l = List.fold_left
			( fun l x ->
						if Sc.is_act x && (List.mem x p.sya_alfa)
						then (star_sgn x):: l
						else l
			) [] l
	in
	(* reactive equations for local signals are split or starred *)
	if debug_level DbgRctStar then (
		pnT 2; ps "alpha "; print_sgn alpha;
		pnT 2; ps "beta  "; print_sgn beta;
		pnT 2 ; ps "globals";
		List.iter (fun x -> pnT 7; print_sgn x) globs;
		pnT 2 ; ps "locals" ;
		List.iter (fun x -> pnT 7; print_sgn x) p.sya_alfa;
	);
	List.iter
		( fun x ->
					if not(is_inp_sgn x || Sc.is_beta x || Sc.is_eta x) then (
						try
							let d = def_get x in
							if Sc.is_act x then (
								match d.sc_act with
								| Some(ActSigInit(_, s, _, _))
								-> split_def d;
										inits := (x, s)::!inits;
								| _ -> others := d::!others
							) else (
								others := d::!others
							)
						with Not_found -> ()
					)
		) p.sya_alfa;
	if debug_level DbgRctStar then (
		pn(); pn(); ps "inits" ;
		List.iter (fun (x, _) -> pnT 7; print_sgn x) !inits;
		pn(); pn(); ps "splittings"
	);
	List.iter split_def !others;
	(* reactive equations for global signals are starred *)
	List.iter star_def globs;
	List.iter star_dcl !r.dcls;
	List.iter ( fun (l1, l2) -> alts_add (filter l1) (filter l2)) !r.alts;
	{ sya_alfa = List.rev_map star_alpha_sgn p.sya_alfa;
		sya_acts = List.fold_left star_add [] p.sya_acts;
		sya_regs = p.sya_regs;
		sya_omga = p.sya_omga;
		sya_taus = [];
		sya_ctrl = p.sya_ctrl;
		sya_inst = (if p.sya_inst = 1 then 2 else p.sya_inst);
		sya_splt = p.sya_splt;
	}

(* -------------------------------------------------------------------------
processing labels

label_suffix - given a label sequence, the result is the longest suffix of
specified labels (reverses the list of labels)
seq_xtnd - given two label sequences, the result is None if the sequences
are incomparable, otherwise it yields the longest sequence.
Note that this operates on suffixes. The idea is to generate
the longest suffix. This reflects the fact that label sequences
are built from the leaves of a "label tree". Hence we work
on the reversed sequences.
seq_insert - inserts a sequence in a list of sequences; if the given
sequence is longer or equal than a sequence of the list,
the latter is replaced by the former. If it shorter nothing
happens. If it is incomparable it it inserted.

------------------------------------------------------------------------- *)
let label_sequences = ref []

let label_suffix xl =
	let list_r = ref [] in
	let rec suffix_h =
		function
		| [] -> ()
		| x:: xl -> if is_int_lbl x then list_r:=[] else list_r:= x::!list_r;
				suffix_h xl
	in
	suffix_h xl;
	!list_r

let seq_xtnd s1 s2 =
	let rec seq_xtnd_h xl yl =
		match xl, yl with
		| [],[] -> Some s1
		| [], _ -> Some s2
		| _,[] -> Some s1
		| x:: xl, y:: yl -> if x <> y then None else seq_xtnd_h xl yl
	in
	seq_xtnd_h s1 s2

let rec seq_insert y =
	function (* ordered list by head of sequences *)
	| [] -> if y = [] then [] else [y]
	| x:: xl ->
			match seq_xtnd x y with
			| None ->
					if x = [] then [] else
					if List.hd x < List.hd y then x:: (seq_insert y xl)
					else if List.hd x = List.hd y then x:: y:: xl else
						y:: x:: xl
			| Some z -> z:: xl

let add_label_seq lbl =
	let sf = label_suffix lbl in
	if sf = [] then () else
		label_sequences := seq_insert sf !label_sequences

(* -------------------------------------------------------------------------
TRANSLATION
------------------------------------------------------------------------- *)
let sya_skip rho alpha beta tau chi mu =
	{ sya_alfa = [];
		sya_acts = [];
		sya_regs = [];
		sya_omga = S alpha;
		sya_taus = [];
		sya_ctrl = [];
		sya_inst = 0;
		sya_splt = false;
	}

(* emittance of signals. For flows equations, clocks are checked. *)
let sya_update ~is_floweq lbl id index e rho alpha beta tau chi mu =
	let xs = sig_try_find !r.sigs id in
	let s = sig2sgn xs and f = sig2frq xs
	and t = sig2valtyp xs in
	if not (is_floweq || (sig2clck xs).expr = v_true.expr) then (
		Err.msg(Err.EmitNotOnBaseClock(rho.cid, id, lbl))
	);
	let trg, alfa = if is_floweq then (
			let trg = Sc.next_gamma() in
			def_add Eqn trg (AAnd[S f.ff; S alpha]);
			trg,[trg]
		) else (
			alpha,[]
		) in
	let e = e rho trg mu tau in
	if is_floweq then (
		ignore (frq_eq rho lbl "for :=" f e.sye_freq);
	);
	let sv = Sc.sgn2val s and a = Sc.next_act() in
	(* emmittance depends on the kind of signals. *)
	let acts =
		if is_delayed_typ (sig2typ xs) then (
			match e.sye_val with
			| Rct _ ->
					def_act Mem a (S trg) (ActEmit([lbl], is_floweq, sv, index, e.sye_val,[], xs, rho.ro));
					def_or Mem s (S trg);
					if e.sye_blck = [] then [] else (
						let a1 = Sc.next_act() in
						def_act Mem a1 (S trg) (ActBlck(e.sye_blck));
						[a1]
					)
			| Val _ ->
					def_act Mem a (S trg) (ActEmit([lbl], is_floweq, sv, index, e.sye_val, e.sye_blck, xs, rho.ro));
					def_or Mem s (OOr[S trg; CC a]);
					[]
		) else (
			match e.sye_val with
			| Rct _ ->
					def_act Eqn a (S trg) (ActEmit([lbl], is_floweq, sv, index, e.sye_val,[], xs, rho.ro));
					def_or Eqn s (S a);
					if e.sye_blck = [] then [] else (
						let a1 = Sc.next_act() in
						def_act Eqn a1 (S trg) (ActBlck(e.sye_blck));
						[a1]
					)
			| Val _ ->
					def_act Eqn a (S trg) (ActEmit([lbl], is_floweq, sv, index, e.sye_val, e.sye_blck, xs, rho.ro));
					def_or Eqn s (S a);
					[]
		);
	in
	(* pure emits cannot be labelled *)
	if t = Null then () else add_label_seq [lbl];
	let d = mk_dbg lbl (S trg) rho in
	{ sya_alfa = a:: d@alfa@acts@e.sye_alfa;
		sya_acts = a:: acts@e.sye_acts;
		sya_regs = [];
		sya_omga = OOr[S alpha; CC a];
		sya_taus = [];
		sya_ctrl = [];
		sya_inst = 0;
		sya_splt = false;
	}

(* data expression *)

let sya_rct_expr lbl e rho alpha beta tau chi mu =
	add_label_seq [lbl];
	let e = e rho alpha mu tau in
	let a = Sc.next_act() in
	def_act Eqn a (S alpha) (ActStmt([lbl], vexp2expr e.sye_val , rho.ro));
	let d = mk_dbg lbl (S alpha) rho in
	{ sya_alfa = a:: d@e.sye_alfa;
		sya_acts = a:: e.sye_acts;
		sya_regs = [];
		sya_omga = OOr[S alpha; CC a];
		sya_taus = [];
		sya_ctrl = [];
		sya_inst = 0;
		sya_splt = false;
	}

(* sequential composition.
Second sya starts when the first terminates.
Note that an auxiliary signal is introduced since every
sya terminates with a fresh termination signal *)

let sya_seq lbl p q rho alpha beta tau chi mu =
	let p = p rho alpha beta tau chi mu in
	if p.sya_inst > 2
	then Err.msg (Err.UnusedCode(rho.cid, lbl))
	else (
		let alpha', alfa =
			match p.sya_omga with
			| S s -> s,[]
			| _ -> let o = Sc.next_gamma() in
					def_add Eqn o p.sya_omga;
					o,[o]
		in
		let q = q rho alpha' beta tau chi mu in
		alts_add p.sya_regs q.sya_regs;
		(* local signals are only those which are possibly in the
		first instant = inst < 2 *)
		{ sya_alfa = p.sya_alfa@( if p.sya_inst < 2 then alfa@q.sya_alfa else [] );
			sya_acts = p.sya_acts@q.sya_acts;
			sya_regs = p.sya_regs@q.sya_regs;
			sya_omga = q.sya_omga;
			sya_taus = [];
			sya_ctrl = p.sya_ctrl@q.sya_ctrl;
			sya_inst = max p.sya_inst q.sya_inst;
			(* for reincarnation: split in loop only needed if the
			composition is possibly instantaneous *)
			sya_splt = (p.sya_splt && q.sya_inst < 2) || (p.sya_inst < 2 && q.sya_splt)
		}
	)

(* parallel composition
straightforward except for termination condition. We use the following
(for two branches):
omega = p.omega x q.omega + (p.omga + not p.ctrl) x (q.omga + not q.ctrl)
where x.ctrl represents the disjunction of control registers.
Advantage: grows with factor 4 for tuples.

There is a special case for instanteneous termination, we were use
alpha for termination. However we gurantee causality in that
a new omega is introduced such that all "p.omega" are causally before
this omega *)

let sya_par lbl pl rho alpha beta tau chi mu =
	let pl = List.map (fun p -> p rho alpha beta tau chi mu ) pl in
	match pl with
	| [] -> sya_skip rho alpha beta tau chi mu
	| [p] -> p
	| _ ->
	(* defining a control signal "disjunction of registers"  *)
			let ctrl =
				List.rev_map
					( fun p ->
								match p.sya_ctrl with
								| [] -> FF
								| [e] -> e
								| _ ->
										let eta = Sc.next_eta() in
										def_add Eqn eta (RctExp.reduce(OOr p.sya_ctrl));
										S eta
					) pl in
			let eta = match ctrl with
				| [] -> FF
				| [e] -> e
				| _ ->
						let eta = Sc.next_eta() in
						def_add Eqn eta (RctExp.reduce(OOr ctrl));
						S eta in
			(* instants *)
			let maxmum, minmum =
				List.fold_left
					( fun (x, y) p -> max x p.sya_inst, min y p.sya_inst
					) (0, 3) pl in
			(* termination *)
			let omgas =
				List.rev_map
					( fun p -> if p.sya_inst < 1 && maxmum > 1 then FF else p.sya_omga
					) pl in
			let cc_omgas = List.rev_map (exp2cc alpha) omgas in
			let or_not_merge bl1 bl2 =
				List.map2
					( fun x y ->
								match x, y with
								| FF, TT -> FF
								| FF, FF -> TT
								| FF, _ -> NNot y
								| _ , TT -> x
								| _ -> OOr[x; NNot y]
					) bl1 bl2 in
			let omga =
				(* instanteneous termination, keeps track of causality *)
				if maxmum = 0 then OOr((S alpha):: cc_omgas) else
					(* no termination, keep only track of causality *)
					if maxmum > 2 then OOr cc_omgas else
						(* not necessarily instanteneous termination *)
						if minmum < 1
						then
							AAnd(eta:: (or_not_merge omgas ctrl))
						else
							OOr[AAnd omgas; AAnd(eta:: (or_not_merge omgas ctrl))]
			in
			{ sya_alfa = all_alfa pl;
				sya_acts = all_acts pl;
				sya_regs = all_regs pl;
				sya_omga = omga;
				sya_taus = [];
				sya_ctrl = ctrl;
				sya_inst = maxmum;
				sya_splt = minmum <= 1 && maxmum = 2 || all_splt pl
			}

(* loop construct
reincarnation may be needed if there is a local definition
that is instantenous in that the block is reentered when
reentering the loop *)

let sya_loop lbl ?(is_instantaneous = false) p rho alpha beta tau chi mu =
	let g = Sc.next_gamma() in
	let p = p rho g beta tau chi mu in
	(* instanteneous loop if inst < 2 *)
	if p.sya_inst < 2 && not is_instantaneous
	then Err.msg (Err.InstantLoop(rho.cid, lbl));
	if p.sya_inst > 2
	then Err.msg (Err.UnusedCode(rho.cid, lbl));
	def_add Eqn g (OOr[S alpha; p.sya_omga]);
	let split = p.sya_splt && p.sya_alfa <> [] in
	let p =
		if split
		then sya_absplit g beta tau (rho.flds@rho.globs) p
		else p
	in
	{ sya_alfa = g:: p.sya_alfa;
		sya_acts = p.sya_acts;
		sya_regs = p.sya_regs;
		sya_omga = FF;
		sya_taus = [];
		sya_ctrl = p.sya_ctrl;
		sya_inst = 3;
		sya_splt = not split;
	}

(* then branch of a conditonal *)

let sya_then lbl e p rho alpha beta tau chi mu =
	(* expression, statement *)
	let e = e rho alpha mu tau in
	match e.sye_val with
	| Rct v ->
			let y = Sc.next_gamma() in
			let acts =
				if e.sye_blck = [] then [] else (
					let a = Sc.next_act() in
					def_act Eqn a (S alpha) (ActBlck(e.sye_blck));
					[a]
				)
			in
			def_add Eqn y (OOr[AAnd[S alpha; v]]);
			let p = p rho y beta tau chi mu in
			let d = mk_dbg lbl (S y) rho in
			{ sya_alfa = y:: d@acts@e.sye_alfa@p.sya_alfa;
				sya_acts = e.sye_acts@p.sya_acts;
				sya_regs = p.sya_regs;
				sya_omga = p.sya_omga;
				sya_taus = [S y];
				sya_ctrl = p.sya_ctrl;
				sya_inst = p.sya_inst;
				sya_splt = p.sya_splt;
			}
	| Val _ -> Err.intern "sya_then"

(* conditonal.
Two usages:
- if statement
- when in cancel statement
Reminder: Note that the tau's can be used only for the same level *)

let rec sya_cond ~is_if cpl rho alpha beta tau chi mu =
	(* is_if - true: if statement, false: when list
	cpl - then - list *)
	match cpl with
	| [] -> sya_skip rho alpha beta tau chi mu
	| [p] -> p rho alpha beta tau chi mu
	| p:: ce ->
			let p = p rho alpha beta tau chi mu in
			let n = Sc.next_gamma() in
			def_add Eqn n (AAnd[S alpha; NNot(OOr p.sya_taus)]);
			let q = sya_cond ~is_if: is_if ce rho n beta tau chi mu in
			(* registers and actions in different branches are alternative *)
			alts_add p.sya_regs q.sya_regs;
			alts_add (List.filter Sc.is_act p.sya_alfa)
				(List.filter Sc.is_act q.sya_alfa);
			(* Compute the instants:
			both branches are instanteneous -> 0
			only one branch instanteneous -> 1
			otherwise -> minimum
			*)
			let maxmum = max p.sya_inst q.sya_inst
			and minmum = min p.sya_inst q.sya_inst in
			let inst = if minmum < 1 && maxmum > 0 then 1 else minmum in
			(* optimization only valid for if statement since
			else branch is guranteed *)
			let omga =
				if is_if && maxmum = 0 then (
					OOr[S alpha; exps2cc alpha [p.sya_omga; q.sya_omga]]
				) else if minmum > 2 then (
					exps2cc alpha [p.sya_omga; q.sya_omga]
				) else (
					OOr[p.sya_omga; q.sya_omga]
				) in
			{ sya_alfa = [n]@p.sya_alfa@q.sya_alfa;
				sya_acts = p.sya_acts@q.sya_acts;
				sya_regs = p.sya_regs@q.sya_regs;
				sya_omga = omga;
				sya_taus = p.sya_taus@q.sya_taus;
				sya_ctrl = p.sya_ctrl@q.sya_ctrl;
				sya_inst = inst;
				sya_splt = p.sya_splt || q.sya_splt
			}

(* next *)

let sya_next lbl rho alpha beta tau chi mu =
	(* Note that the register has to be in control till the next beta *)
	let h = Sc.next_reg() in
	def_add Mem h (AAnd[NNot(S tau); OOr[S alpha; AAnd[S h; NNot(S beta)]]]);
	let d = mk_dbg lbl (AAnd[NNot (S tau); OOr[S alpha; AAnd[S h; NNot(S beta)]]]) rho in
	{ sya_alfa = h:: d;
		sya_acts = [];
		sya_regs = [h];
		sya_omga = AAnd[S h; S beta];
		sya_taus = [];
		sya_ctrl = [S h];
		sya_inst = 2;
		sya_splt = false;
	}

(* halt *)

let sya_halt lbl rho alpha beta tau chi mu =
	(* Note that the register has to be in control till the next beta *)
	let h = Sc.next_reg() in
	def_add Mem h (AAnd[NNot(S tau); OOr[S alpha; AAnd[S h]]]);
	let d = mk_dbg lbl (AAnd[NNot(S tau); OOr[S alpha; AAnd[S h]]]) rho in
	{ sya_alfa = h:: d;
		sya_acts = [];
		sya_regs = [h];
		sya_omga = FF;
		sya_taus = [];
		sya_ctrl = [S h];
		sya_inst = 3;
		sya_splt = false;
	}

(* cancel
for alternatives: strongly or not, later instants or all instants *)

let sya_cancel lbl ~strongly ~next ~split ~timing
		p cpl rho alpha beta tau chi mu =
	(* strongly - false: weak preemption, true: strong preemption
	next - false: immediately, true: from next instant on
	split - to be splitted (for instance for cyclic states)
	has_timestamp - false: No timing condition, true: timing condition
	p - body, cpl - when list,
	the system signals:
	y - trigger for the when construct
	t - preemption signal. New tau if strongly, new otherwise
	x - true if some preemption condition is true
	alpha' - new alpha if strongly, true only if alpha and all
	preemption conditions are false
	beta' - new beta if strongly, true only if beta and all
	preemption conditions are false
	*)
	let next = next || timing in
	let y = Sc.next_gamma()
	and tau' = Sc.next_tau()
	and alpha' = if strongly && not next then Sc.next_gamma() else alpha
	and beta' = if strongly then Sc.next_beta() else beta in
	let p = p rho alpha' beta' tau' chi mu in
	(* This is a tricky bit:
	The exit list represents the causal condition that all actions
	in the body take place before the action in the when condition list.
	There are two cases to consider:
	next = true: then only "beta" - actions should be causally before the
	the actions in the when condition list.
	next = false: nothing needs to be done
	*)
	let exit =
		if strongly || p.sya_acts = [] then FF else
		if next then (
			let exit =
				List.filter
					( fun s -> not(List.mem s p.sya_alfa)
					) p.sya_acts in
			OOr(List.rev_map sgn2cc exit)
		) else (
			OOr(List.rev_map sgn2cc p.sya_acts)
		)
	and ctrl = match p.sya_ctrl with
		| [] -> FF
		| [e] -> e
		| _ ->
				let eta = Sc.next_gamma() in
				def_add Eqn eta (OOr p.sya_ctrl);
				S eta in
	(* init a time stamp variable and increase *)
	let timestamp, alfas, acts =
		if timing then (
			let x = Sc.next_aux() in add_internal_sig_dcl x t_time 0;
			let a1 = Sc.next_act() in
			def_act Eqn a1 (S alpha') (ActTimeInit([lbl], x, rho.ro));
			let a2 = Sc.next_act() in
			def_act Eqn a2 ctrl (ActTimeAdd([lbl], x, rho.ro));
			Some x,[a1],[a1; a2]
		) else (
			None,[],[]
		) in
	(* defining the conditions for condition for evaluating the branches,
	either at alpha or at beta if in "control" *)
	let time_acts = OOr(List.rev_map sgn2cc acts) in
	if next
	then def_add Eqn y (OOr([AAnd[ctrl; S beta]; exit; time_acts]))
	else def_add Eqn y (OOr[S alpha; AAnd[ctrl; S beta]; exit; time_acts]);
	let cpl = cpl timestamp rho y beta tau chi mu in
	(* defining the conditions for preemption *)
	let esc, alfa = match cpl.sya_taus with
		| [] -> FF,[]
		| [S x] -> S x,[]
		| _ ->
				let x = Sc.next_tau() in
				def_add Eqn x (OOr cpl.sya_taus);
				S x,[x] in
	def_add Eqn tau' (OOr[S tau; esc]);
	(* define alpha' and beta' for strong preemption *)
	if strongly then (
		if not next then def_add Eqn alpha' (AAnd[S alpha; NNot(esc)]);
		def_add Eqn beta' (AAnd[S beta; NNot(esc)])
	);
	(* registers in the body and the branches are disjoint since
	sequentially evaluated. Actions are alternative only if
	preemption is strong *)
	alts_add p.sya_regs cpl.sya_regs;
	if strongly then alts_add p.sya_acts cpl.sya_acts;
	(* cancel terminates if the body terminates or the exit statement *)
	let inst =
		if p.sya_inst < 2 then p.sya_inst else
		if next
		then ( if cpl.sya_inst > 1 then min p.sya_inst cpl.sya_inst else 2 )
		else ( if cpl.sya_inst < 2 then 1 else min p.sya_inst cpl.sya_inst )
	in
	(* We have to split some of the signals introduced here:
	alpha', tau', and *)
	let split = next || split in
	( fun p -> if split
				then sya_absplit alpha beta tau (rho.flds@rho.globs) p
				else p
	) { sya_alfa =
				p.sya_alfa@alfas
				@(if strongly && not next then [alpha'] else [])
				@(if next then [tau'] else alfa@[y; tau']@cpl.sya_alfa);
			sya_acts = acts@p.sya_acts@cpl.sya_acts;
			sya_regs = p.sya_regs@cpl.sya_regs;
			sya_omga = OOr[p.sya_omga; cpl.sya_omga];
			sya_taus = [];
			sya_ctrl = ctrl:: cpl.sya_ctrl;
			sya_inst = inst;
			sya_splt = p.sya_splt && not split;
		}

(* activate: immediate or next *)

let sya_activate lbl next p e rho alpha beta tau chi mu =
	(* activate p when e
	if next then activation only in the next instant.
	otherwise wait till condition becomes true *)
	let e = e true rho beta mu tau in
	match vexp2rct e.sye_val with
	| FF -> Err.msg (Err.NeverActivated(rho.cid, lbl))
	| TT -> p rho alpha beta tau chi mu
	| v ->
			let beta' = Sc.next_beta() in
			let alpha', regs, alfa, acts =
				if e.sye_blck = [] then (
					if next then (
						def_add Eqn beta' (AAnd[S beta; v]);
						alpha,[],[],[]
					) else (
						let g = Sc.next_gamma() and h = Sc.next_reg() in
						def_add Eqn g (AAnd[v; OOr[S alpha; S h]]);
						def_add Mem h (AAnd[NNot(S g); OOr[S alpha; S h]]);
						def_add Eqn beta' (AAnd[S beta; v]);
						g,[h],[h],[]
					)
				) else (
					if next then (
						if e.sye_blck = [] then (
							alpha,[],[],[]
						) else (
							let a = Sc.next_act() in
							def_act Eqn a (S beta) (ActBlck(e.sye_blck));
							alpha,[],[a],[a]
						)
					) else (
						let g = Sc.next_gamma() and h = Sc.next_reg() in
						def_add Eqn g (AAnd[v; OOr[S alpha; S h]]);
						def_add Mem h (AAnd[NNot(S g); OOr[S alpha; S h]]);
						def_add Eqn beta' (AAnd[S beta; v]);
						if e.sye_blck = [] then (
							g,[h],[h],[]
						) else (
							let a = Sc.next_act() in
							def_act Eqn a (OOr[S alpha; S h; S beta])
								(ActBlck(e.sye_blck));
							g,[h],[a; h],[a]
						)
					)
				)
			in
			let p = p rho alpha' beta' tau chi mu in
			alts_add regs p.sya_regs;
			{ sya_alfa = (if next then [] else alfa)@e.sye_alfa@p.sya_alfa;
				sya_acts = e.sye_acts@p.sya_acts;
				sya_regs = regs@p.sya_regs;
				sya_omga = p.sya_omga;
				sya_taus = [] ;
				sya_ctrl = p.sya_ctrl;
				sya_inst = p.sya_inst;
				sya_splt = p.sya_splt
			}

(* do_at : resumes execution at this point (complimentary to "jump to")
several kinds of jumps:
- at beta
- at a label / state. *)

let sya_do_at lbl st p rho alpha beta tau chi mu =
	let j = jump_try_find rho st in
	def_or Eqn j FF;
	let p = p rho j beta tau chi mu in
	let d = mk_dbg lbl (OOr[S j; AAnd[OOr p.sya_ctrl; NNot(OOr (all_jumps rho))]]) rho in
	{ sya_alfa = j:: d@p.sya_alfa;
		sya_acts = p.sya_acts;
		sya_regs = p.sya_regs;
		sya_omga = p.sya_omga;
		sya_taus = [];
		sya_ctrl = p.sya_ctrl;
		sya_inst = p.sya_inst;
		sya_splt = false;
	}

(* jump_to : complimentary to "do_at" (for goto programming/automata) *)

let sya_jump_to lbl st rho alpha beta tau chi mu =
	let j = jump_try_find rho st in
	def_or Eqn j (S alpha);
	(* never terminates, but keeps causality relation *)
	{ sya_alfa = [];
		sya_acts = [];
		sya_regs = [];
		sya_omga = FF;
		sya_taus = [];
		sya_ctrl = [];
		sya_inst = 3;
		sya_splt = false;
	}

let sya_automaton lbl ~init ~stpl rho alpha beta tau chi mu =
	let p = init rho alpha beta tau chi mu in
	let pl = List.map (fun (_, p) -> p rho alpha beta tau chi mu ) stpl in
	let regs_l = List.rev_map (fun p -> p.sya_regs) pl in
	List.iter (fun (x, y) -> alts_add x y) (alts_add_l regs_l);
	let acts_l = List.map (fun p -> List.filter Sc.is_act p.sya_alfa) pl in
	List.iter (fun (x, y) -> alts_add x y) (alts_add_l acts_l);
	let minmum = List.fold_left (fun x p -> min x p.sya_inst) 2 pl in
	let alfas =
		List.fold_left2
			( fun l (is_init, _) p ->
						if is_init then p.sya_alfa@l else l
			) [] stpl pl in
	{ sya_alfa = all_alfa (p:: pl);
		sya_acts = all_acts (p:: pl);
		sya_regs = all_regs (p:: pl);
		sya_omga = List.fold_left (fun l p -> OOr[l; p.sya_omga]) FF pl;
		sya_taus = [];
		sya_ctrl = all_ctrl pl;
		sya_inst = minmum;
		sya_splt = all_splt pl;
	}

(* local declarations *)

let sya_local ~is_flwctxt lbl d p rho alpha beta tau chi mu =
	let mu' = if is_flwctxt then mu else alpha in
	let d = d rho mu' mu' tau FF in
	let y, alfa =
		match d.syd_caus with
		| FF -> alpha,[]
		| e ->
				let y = Sc.next_gamma() in
				def_add Eqn y (OOr[S alpha; e]);
				y,[y]
	in
	let rho = { rho with globs = d.syd_glbs@rho.globs } in
	let p = p rho y beta tau chi mu in
	{ sya_alfa = alfa@d.syd_alfa@p.sya_alfa;
		sya_acts = d.syd_acts@p.sya_acts;
		sya_regs = p.sya_regs;
		sya_omga = p.sya_omga;
		sya_taus = [];
		sya_ctrl = p.sya_ctrl;
		sya_inst = p.sya_inst;
		sya_splt = true;
	}

(* setting a flow context
next: true - during
next: false - sustain
makes mu for each flow context unique
needed for proper initialisation of
local signals, pre, arrow, and current
*)
let sya_set_flow_context ~next p rho alpha beta tau chi mu =
	let mu =
		if next then (
			let mu = Sc.next_mem() in
			def_add Mem mu (S alpha);
			mu
		) else (
			let mu = Sc.next_gamma() in
			def_add Eqn mu (S alpha);
			mu
		)
	in
	let p = p rho alpha beta tau chi mu in
	{ sya_alfa = if next then p.sya_alfa else mu:: p.sya_alfa;
		sya_acts = p.sya_acts;
		sya_regs = p.sya_regs;
		sya_omga = p.sya_omga;
		sya_taus = p.sya_taus;
		sya_ctrl = p.sya_ctrl;
		sya_inst = p.sya_inst;
		sya_splt = p.sya_splt;
	}

(* -------------------------------------------------------------------------
SEMANTIC ANALYSIS
------------------------------------------------------------------------- *)

(* --------------------------------------------------------------------------
Auxiliary Data structures:

IntSet - Sets of ordered integers
-------------------------------------------------------------------------- *)
module OrderedInt =
struct
	type t = int
	let compare = compare
end

module IntSet = Set.Make(OrderedInt)

type search_tbl = (int, IntSet.t) Hashtbl.t

(* data structures for semantic analysis *)
let sgn2regs_tbl = Hashtbl.create 1991
let eqns_tbl = Hashtbl.create 1991
let alts_tbl = Hashtbl.create 1991

let eqn_add d =
	try
		let d' = Hashtbl.find eqns_tbl d.sc_sgn in
		d'.sc_def <- OOr[d'.sc_def; d.sc_def]
	with Not_found ->
			Hashtbl.add eqns_tbl d.sc_sgn d

let eqn_or x e =
	try
		let d = Hashtbl.find eqns_tbl x in
		d.sc_def <- OOr[d.sc_def; e]
	with Not_found ->
			Err.intern ("eqn_or: "^(sgn2str x))

let eqn_find x =
	let d = Hashtbl.find eqns_tbl x in
	if d.sc_knd = Eqn then d else (raise Not_found)

let mem_find x =
	let d = Hashtbl.find eqns_tbl x in
	if d.sc_knd = Mem || d.sc_knd = Nxt then d else (raise Not_found)

let eqn_try_find x =
	try Hashtbl.find eqns_tbl x
	with Not_found ->
			Err.intern ("eqn_try_find: "^(sgn2str x))

let eqn_remove x =
	try Hashtbl.remove eqns_tbl x
	with Not_found ->
			Err.intern ("eqn_remove: "^(sgn2str x))

let eqn_iter f =
	Hashtbl.iter f eqns_tbl

let sp_add_singleton x y =
	if x = y then () else
		let x, y = if x <= y then x, y else y, x in
		try
			let t = Hashtbl.find alts_tbl x in
			Hashtbl.replace alts_tbl x (IntSet.add y t)
		with Not_found ->
				Hashtbl.add alts_tbl x (IntSet.singleton y)

let sp_add_list l x =
	List.iter (sp_add_singleton x) l

let sp_add (l1, l2) =
	List.iter (sp_add_list l2) l1

let alts_tbl2list () =
	let l = ref [] in
	Hashtbl.iter
		( fun x y -> l := ([x], IntSet.elements y) :: (!l)
		) alts_tbl;
	!l

let is_alt x y =
	let x, y = if x <= y then x, y else y, x in
	try IntSet.mem y (Hashtbl.find alts_tbl x)
	with Not_found -> false

let sgn2regs_tbl2list () =
	let l = ref [] in
	Hashtbl.iter
		( fun x y -> if Sc.is_act x then l:= (x, y)::!l
		) sgn2regs_tbl;
	!l

let print_all_defs () =
	pn(); ps "all definitions";
	def_iter (fun d -> (print_rctdef Eqn) d);
	pn(); ps "all memories";
	def_iter (fun d -> (print_rctdef Mem) d);
	pn(); ps "all pres";
	def_iter (fun d -> (print_rctdef Nxt) d)

(* -------------------------------------------------------------------------
CHECK ACTIONS FOR CONFLICTS

the idea is as follows;
every execution depends either on an "alpha" or a "register". If actions
depend on registers which are known to be "disjoint" due to the program
structure (stored in "alts") then the actions cannot take place at the
same instant. "alpha", of course, is "disjoint with any register since both
cannot be true at the same instant.

We use the follwing functions:

rctexp2regs e - computes the set of registers occurring
in the term "e' , plus "alpha" if it occurs

sya_mutually_disjoint_regs l1 l2 - checks whether all the registers in
"l1" and "l2" are disjoint according
to "alts", resp. whether one is
"alpha" and the other not

sya_exclusive_actions l - checks whether all the actions in the list
are known to be disjoint due to program structure
(information stored in "alts"), or
have mutually disjoint sets of registers

Note: Remeber the invariant that the translation scheme should
not generate loops on signals that are not cut by registers

------------------------------------------------------------------------- *)
let rec sorted_merge x y =
	match x, y with
	| [],[] -> []
	| [], _ -> y
	| _,[] -> x
	| a:: x, b:: y ->
			if a = b then a:: (sorted_merge x y) else
			if a < b then a:: (sorted_merge x (b:: y)) else
				b:: (sorted_merge (a:: x) y)
and sorted_n_merge_h l =
	match l with
	| [] -> [[]]
	| [x] -> [x]
	| [x; y] -> [sorted_merge x y]
	| x:: y:: l -> sorted_n_merge_h ((sorted_merge x y):: (sorted_n_merge_h l))
and sorted_n_merge l =
	match sorted_n_merge_h l with
	| [x] -> x
	| _ -> Err.intern "sorted_n_merge"

let rec rctexp2regs =
	let rctexps2regs el = sorted_n_merge (List.map rctexp2regs el)
	and sgn2reg = function
		| S x ->
				if Sc.is_reg x then [x] else
				if x = !r.alpha then [x] else []
		| _ -> []
	and find_regs x =
		let regs = Hashtbl.find sgn2regs_tbl x in
		if not (cntxt_is_class()) then regs else
			sorted_n_merge
				( List.rev_map
						( fun x -> if Sc.is_reg x || x = !r.alpha
									then [x]
									else rctexp2regs (S x)
						) regs
				)
	in function
	| S 1 -> []
	| S x ->
			if Sc.is_reg x then [x] else
			if x = !r.alpha then [x] else
			if Sc.is_gamma x || Sc.is_beta x || Sc.is_jump x || Sc.is_act x
			then (
				try find_regs x
				with Not_found ->
						let d = eqn_try_find x in
						let regs = rctexp2regs d.sc_def in
						Hashtbl.add sgn2regs_tbl x regs;
						regs
			) else
				[]
	| CC x -> []
	| AAnd el ->
			let regs = sorted_n_merge (List.rev_map sgn2reg el) in
			if regs = [] then rctexps2regs el else regs
	| OOr el -> rctexps2regs el
	| NNot e -> []
	| e -> []

let rec mutually_disjoint_regs l1 l2 =
	match l1 with
	| [] -> true
	| x:: l1 ->
			if List.fold_left
				( fun b y ->
							if x = y then false else
								b && ( is_alt x y
									|| x = !r.alpha && not (y = !r.alpha)
									|| y = !r.alpha && not (x = !r.alpha)
								)
				) true l2
			then mutually_disjoint_regs l1 l2
			else false

let rec sya_exclusive_actions a1 a2 =
	if is_alt a1 a2
	then true
	else
		let regs1 = rctexp2regs(S a1) and regs2 = rctexp2regs(S a2) in
		if cntxt_is_appl()
		then
			regs1 = [!r.alpha] && not(List.mem !r.alpha regs2)
			|| not(List.mem !r.alpha regs1) && regs2 = [!r.alpha]
		else
		if mutually_disjoint_regs regs1 regs2 then (
			sp_add_singleton a1 a2;
			true
		) else (
			false
		);;

(* -------------------------------------------------------------------------
HANDLING PRECEDENCES

We have three generators for precedences:
- the reactive equations
- the actions
- precedences caused by precedence declarations
------------------------------------------------------------------------- *)

(* auxiliaries *)
let tsort = ref (Tsort.init 1)  (* for sorting signals *)

let set_pre_post x y = Tsort.set_pre_post !tsort x y
let prec_pres_post l y = List.iter (fun x -> set_pre_post x y) l

let sgn2occ s =
	try
		let d = Hashtbl.find eqns_tbl s in
		d.sc_occ <- d.sc_occ + 1;
	with Not_found -> ()

(* precedences for reactive equations
note that signals that occur in the action part
should not be substituted, hence add 2 to the number
occurrences *)
let exclude s =
	Sc.is_reg_or_mem_or_pre s || Sc.is_extval s || Sc.is_inpval s || Sc.is_lclval s

let cll2precs a k c =
	let rec blck2precs cc =
		List.iter
			( fun (e, c) ->
						let sl = rctexp2sgns e in
						if cntxt_is_appl() then sgns2occs sl;
						prec_pres_post sl a;
						cll2precs c
			) cc
	and sgns2occs sl = () (* List.iter sgn2occ sl *)
	and cll2precs = function
		| ActPrm(_, _, e, _, _) ->
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				prec_pres_post sl a
		| ActInit(_) -> ()
		| ActAssgn(_, s, e, cc, _) ->
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				if not (Sc.is_mem s || Sc.is_pre s) then (
					prec_pres_post sl a
				);
				blck2precs cc
		| ActAssgnInBlck(_, s, e, _) ->
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				if not (Sc.is_mem s || Sc.is_pre s) then (
					prec_pres_post sl a
				)
		| ActStmt(_, e, _) ->
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				prec_pres_post sl a
		| ActProd(_, s, e1, e2, cc, _) ->
				set_pre_post a s;
				let sl = Expr.expr2sgns e1 @ Expr.expr2sgns e2
				in
				if cntxt_is_appl() then sgns2occs sl;
				prec_pres_post sl a;
				blck2precs cc
		| ActSigInit(_) -> ()
		| ActEmit(_, _, sv, _, Rct e, cc, _, _) ->
				let sl = rctexp2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				if k = Eqn && not(Sc.is_mem sv) || k = Mem && not(exclude sv) then (
					prec_pres_post sl a
				);
				blck2precs cc
		| ActEmit(_, _, sv, _, Val e, cc, _, _) ->
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				if k = Eqn && not(Sc.is_mem sv) || k = Mem && not(exclude sv) then (
					prec_pres_post sl a
				);
				blck2precs cc
		| ActBlck(cc) -> blck2precs cc
		| ActSigPres _
		| ActSigVal _
		| ActSigIn _
		| ActSigOut _ -> ()
		| ActBool(_, b, e, _) ->
				set_pre_post a b;
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				prec_pres_post sl a
		| ActTimeInit(_, s, _) -> ()
		| ActTimeAdd(_, s, _) -> set_pre_post a s
		| ActTimeCond(_, b, s, e, _) ->
				set_pre_post a b;
				set_pre_post s a;
				let sl = Expr.expr2sgns e in
				if cntxt_is_appl() then sgns2occs sl;
				prec_pres_post sl a
		| ActDbg _ -> ()
	in
	cll2precs c

let sgn2assgn = ref []

let sgn2assg_add s d =
	let rec sgn2assg_add_h s d l =
		match l with
		| [] -> [(s,[d])]
		| (s', dl):: l when s = s' -> (s', d:: dl):: l
		| x:: l -> x:: (sgn2assg_add_h s d l)
	in
	sgn2assgn := sgn2assg_add_h s d !sgn2assgn

let rct_def2prec d =
	let pres = rctexp2sgns d.sc_def in
	if pres = []
	then Tsort.set_node !tsort d.sc_sgn
	else prec_pres_post pres d.sc_sgn;
	match d.sc_act with
	| None -> ()
	| Some c ->
			cll2precs d.sc_sgn d.sc_knd c;
			( match c with
				| ActEmit(_, _, _, _, Rct _, _, xs, _) ->
						sgn2assg_add(Sc.sgn2val(sig2sgn xs)) d
				| _ -> ()
			)

let rct_defs2precs () =
	sgn2assgn := [];
	def_iter
		( fun d ->
					match d.sc_knd with
					| Eqn -> rct_def2prec d
					| Mem -> if not (exclude d.sc_sgn)
							then rct_def2prec d
					| _ -> ()
		)

(* precedences generated from precedence declarations
done by function gen_data_dependencies gets four parameters:
- class id
- all action calls
- a callback function to generate precedences
- a callback function to list potential time races

There are two modes: for classes and applications.
in the latter only precendences for valued signal emittance
are generated.

In both modes potential time races are generated simultaneously *)

type depend_t =
	{ mutable data : tsc_ctrl list;
		mutable left : (tsc_ctrl * tsc_id) list;
		mutable right : (tsc_ctrl * tsc_id list) list;
	}

let sya_depend =
	{ data = [];
		left = [];
		right = [];
	};;

let sya_prepare_clls_for_dependency_analysis_in_class () =
	sya_depend.data <- [];
	sya_depend.left <- [];
	sya_depend.right <- [];
	def_iter
		( fun d ->
					match d.sc_act with
					| None -> ()
					| Some c ->
							( match c with
								| ActDbg _
								| ActSigPres _
								| ActSigVal _
								| ActInit _ ->
										()
								| ActEmit(_, _, sv, _, Val e, _, _, _) when Expr.is_null e ->
										()
								| ActEmit(_, _, sv, _, Rct e, cc, xs, ro) ->
										sya_depend.data <- d:: sya_depend.data;
										sya_depend.left <- (d, sv):: sya_depend.left;
										let vals = (rctexp2vals e)@(Expr.blck2vals) cc in
										if vals <> []
										then
											sya_depend.right <- (d, vals):: sya_depend.right
								| ActEmit(lbl, _, sv, _, Val e, cc, xs, ro) ->
										sya_depend.data <- d:: sya_depend.data;
										sya_depend.left <- (d, sv):: sya_depend.left;
										let vals = (Expr.expr2vals e)@(Expr.blck2vals cc) in
										if vals <> []
										then
											sya_depend.right <- (d, vals):: sya_depend.right
								| ActSigInit(_, sv, _, _) ->
										sya_depend.left <- (d, sv):: sya_depend.left;
								| ActSigIn(_, xs, _) ->
										sya_depend.left <-
										(d, Sc.sgn2val(sig2sgn xs)):: sya_depend.left;
								| ActSigOut(_, xs, _) ->
										sya_depend.right <-
										(d,[Sc.sgn2val(sig2sgn xs)]):: sya_depend.right;
								| _ ->
										sya_depend.data <- d:: sya_depend.data;
										let vals = Expr.cll2vals c in
										if vals <> []
										then
											sya_depend.right <- (d, vals):: sya_depend.right
							)
		)

let sya_prepare_clls_for_dependency_analysis_in_appl () =
	sya_depend.data <- [];
	sya_depend.left <- [];
	sya_depend.right <- [];
	def_iter
		( fun d ->
					match d.sc_act with
					| None -> ()
					| Some c ->
							( match c with
								| ActEmit(_, _, sv, _, Val e, cc, _, _) when Expr.is_null e ->
										()
								| ActEmit(_, _, sv, _, _, _, _, _) ->
										sya_depend.data <- d:: sya_depend.data
								| ActSigIn(_, xs, _) ->
										sya_depend.right <-
										(d,[Sc.sgn2val(sig2sgn xs)]):: sya_depend.right
								| ActSigOut(_, xs, _) ->
										sya_depend.left <-
										(d, Sc.sgn2val(sig2sgn xs)):: sya_depend.left
								| _ -> ()
							)
		)

let data_set_pre_post_class d1 d2 =
	let a1 = d1.sc_sgn and a2 = d2.sc_sgn in
	let is_inout = function
		| Some(ActSigIn _) -> true
		| Some(ActSigOut _) -> true
		| _ -> false
	in
	if not (sya_exclusive_actions a1 a2)
	|| is_inout d1.sc_act && not(is_inout d2.sc_act)
	|| is_inout d2.sc_act && not(is_inout d1.sc_act) then (
		if debug_level DbgRctCausality then (
			pn(); ps "precedence "; print_sgn a1; ps " < "; print_sgn a2
		);
		eqn_or a2 (CC a1);
		set_pre_post a1 a2
	)

let data_set_pre_post_appl a1 a2 =
	eqn_or a2 (CC a1);
	set_pre_post a1 a2

let rec sya_signal_depend left =
	match left with
	| [] -> ()
	| (d1, sv):: left' ->
			List.iter
				( fun (d2, svl) ->
							if List.mem sv svl then (
								match cntxt.mode with
								| Meth _ | Node _ -> ()
								| Class _ -> data_set_pre_post_class d1 d2
								| Appl _ -> data_set_pre_post_appl d1.sc_sgn d2.sc_sgn
							)
				) sya_depend.right;
			sya_signal_depend left'

let sya_pot_time_races = ref []

let add_pot_time_race_class (d1, se1) (d2, se2) =
	let a1 = d1.sc_sgn and a2 = d2.sc_sgn in
	if not (sya_exclusive_actions a1 a2)
	then sya_pot_time_races := ((a1, se1), (a2, se2))::!sya_pot_time_races

let add_pot_time_race_appl (a1, se1) (a2, se2) =
	if not (sya_exclusive_actions a1 a2)
	then sya_pot_time_races := ((a1, se1), (a2, se2))::!sya_pot_time_races

let gen_precedences_and_pot_time_races () =
	sya_pot_time_races := [];
	rct_defs2precs();
	match cntxt.mode with
	| Meth ro | Node ro ->
			()
	| Class ro ->
			sya_prepare_clls_for_dependency_analysis_in_class ();
			sya_signal_depend sya_depend.left;
			Data_depend.for_class (ro2cid ro) sya_depend.data
				data_set_pre_post_class add_pot_time_race_class
	| Appl rtl ->
			sya_prepare_clls_for_dependency_analysis_in_appl ();
			sya_signal_depend sya_depend.left;
			Data_depend.for_appl rtl sya_depend.data
				data_set_pre_post_appl add_pot_time_race_appl

(* -------------------------------------------------------------------------
check for timeraces

For each data action or emittance of a valued signal / flow,
we check whether the actions are disjoint in time or in precedence.
Those which are neither lead to an error message.
------------------------------------------------------------------------- *)

(* checking for time races of data actions *)
let check_for_time_races () =
	List.iter
		(fun ((a1, f), (a2, g)) ->
					if not(Tsort.is_pre_post !tsort a1 a2
							|| Tsort.is_pre_post !tsort a2 a1) then (
						match cntxt.mode with
						| Meth ro
						| Node ro -> ()
						| Class ro ->
								let cid = ro2cid ro in
								( match f, g with
									| PrecEnt4Class f, PrecEnt4Class g ->
											let f = Data_depend.sign2str f
											and g = Data_depend.sign2str g in
											Err.msg(Err.TimeRaceInClass(cid, f, g))
									| _ -> Err.intern "check_for_time_races:class"
								)
						| Appl _ ->
								( match f, g with
									| PrecEnt4Appl(ro1, sid1), PrecEnt4Appl(ro2, sid2) ->
											Err.msg (Err.MultEmitInAppl(obj2str ro1, sid1,
														obj2str ro2, sid2))
									| _ -> Err.intern "check_for_time_races:appl."
								)
					)
		) !sya_pot_time_races

(* checking for multiple emits *)
let indices_are_disjoint i1 i2 =
	let expr2const = Util_parse.prs_expr2const in
	match i1.expr, i2.expr with
	| Slice r1, Slice r2 ->
			expr2const r2.upper < expr2const r1.lower
			|| expr2const r1.upper < expr2const r2.lower
	| _, Slice r2 ->
			expr2const r2.upper < expr2const i1 or
			expr2const i1 < expr2const r2.lower
	| Slice r1, _ ->
			expr2const i2 < expr2const r1.lower or
			expr2const r1.upper < expr2const i2
	| _, _ ->
			expr2const i2 < expr2const i1 || expr2const i1 < expr2const i2

let rec list_of_indices_are_disjoint il1 il2 =
	match il1, il2 with
	| [i1],[i2] ->
			indices_are_disjoint i1 i2
	| i1:: il1, i2:: il2 ->
			indices_are_disjoint i1 i2 && list_of_indices_are_disjoint il1 il2
	| _ -> false

let mult_emit d1 d2 =
	match d1.sc_act, d2.sc_act with
	| Some(ActEmit(_, _, _, _, Val e, _, _, _)), _ when Expr.is_null e ->
			()
	| _, Some(ActEmit(_, _, _, _, Val e, _, _, _)) when Expr.is_null e ->
			()
	| Some(ActEmit(_, _, sv1, _, _, _, _, _)), Some(ActEmit(_, _, sv2, _, _, _, _, _))
	when ( sv1 <> sv2
		|| sya_exclusive_actions d1.sc_sgn d2.sc_sgn
		|| Tsort.is_pre_post !tsort d1.sc_sgn d2.sc_sgn
		|| Tsort.is_pre_post !tsort d2.sc_sgn d1.sc_sgn ) ->
			()
	| Some(ActEmit(lbl1, _, sv1, i1, _, _, xs1, ro)), Some(ActEmit(lbl2, _, sv2, i2, _, _, _, _)) ->
			if debug_level DbgRctCausality then (
				print_all_defs();
				pn(); ps "multi_emit: "; ps (sgn2str sv1);
				ps ":"; print_sgn d1.sc_sgn;
				ps "    "; ps (sgn2str sv2);
				ps ":"; print_sgn d2.sc_sgn
			);
			if list_of_indices_are_disjoint i1 i2 then () else
			if lbl1 = lbl2
			then
				Err.msg(
						Err.MultEmitInClassSame(sig2id xs1, ro_lbll2str ro lbl1))
			else
				Err.msg(Err.MultEmitInClass(sig2id xs1, ro_lbll2str ro lbl1,
							ro_lbll2str ro lbl2))
	| Some(ActSigInit _), _ | _, Some(ActSigInit _)
	| Some(ActSigIn _), _ | _, Some(ActSigIn _)
	| Some(ActSigOut _), _ | _, Some(ActSigOut _) ->
			()
	| _ -> Err.intern "mult_emit: no assignments"

let check_for_mult_emits_in_class () =
	let rec mult_emit_h2 (d, sv) = function
		| [] -> ()
		| (d', sv'):: l ->
				if sv = sv' then mult_emit d d';
				mult_emit_h2 (d, sv) l
	and mult_emit_h1 = function
		| [] -> ()
		| (d, sv):: l ->
				mult_emit_h2 (d, sv) l;
				mult_emit_h1 l
	in
	mult_emit_h1 sya_depend.left

(* -------------------------------------------------------------------------
AGGREGATION of emits for Boolean Valued Signals
------------------------------------------------------------------------- *)
let rec all_posts d1 = function
	| [] -> []
	| d2:: dl ->
			if Tsort.is_pre_post !tsort d1.sc_sgn d2.sc_sgn
			then d2.sc_def:: (all_posts d1 dl)
			else all_posts d1 dl

let aggregate_bool_emits_h (sv, clls) =
	let cll2rctexp d =
		match d.sc_act with
		| Some (ActEmit(_, _, _, _, Rct v, _, _, _)) ->
				AAnd[d.sc_def; NNot(OOr(all_posts d clls)); v]
		| _ ->
				Err.intern "aggregate_boolean_emits"
	in
	eqn_or sv (OOr(List.map cll2rctexp clls))

let aggregate_bool_emits() =
	if cntxt_is_appl() then (
		List.iter aggregate_bool_emits_h !sgn2assgn
	)
(* -------------------------------------------------------------------------
CAUSALITY CHECK

- generates precedences etc.
- sorts : if cycle -> error messages
- checks potential time races for real time races
(i.e. not real if there are precedences induced by the program structure
if real time races -> error message
- yields the sorted list
------------------------------------------------------------------------- *)
let sgn2sname s =
	match cntxt.mode with
	| Appl ro_l ->
			let rec find =
				function
				| [] -> Err.intern ("sgn2sname: "^(i2s s))
				| ro:: ro_l -> try sgn2sig s (ro2sigs ro)
						with Not_found -> find ro_l
			in
			"Emit or flow equation of signal\n    "^
			sig2str (find ro_l)
	| Class ro | Meth ro | Node ro ->
			"Emit or flow equation of signal\n    "^
			(obj2str ro)^"."^(sig2str (sgn2sig s !r.sigs))

let act2errstr a =
	let a' = match (eqn_try_find a).sc_act with
		| None -> pn(); print_sgn a; Err.intern "act2errstr"
		| Some a' -> a'
	in
	match a' with
	| ActPrm(lbl, _, _, x, ro) ->
			(obj2str ro)^"\n     Initialision of parameter "^(mf2str x.p_name)
			^" at position:"^(ro_lbll2str ro lbl)
	| ActInit _
	| ActAssgn _
	| ActAssgnInBlck _ -> ""
	| ActStmt(lbl, _, ro) -> (obj2str ro)^"\n     Expression at position:"^(ro_lbll2str ro lbl)
	| ActSigInit (_, _, _, _) -> ""
	| ActEmit(lbl, f, sv, _, _, _, xs, ro) ->
			if Sc.is_extval sv || Sc.is_lclval sv  (* for values *) then (
				( if f (* is flow equation *) then
						(obj2str ro)^"."^(mf2str (sig2id xs))^" := (...)  at"
					else
						"emit "^(obj2str ro)^"."^(mf2str (sig2id xs))^"(...)  at"
				)^(ro_lbll2str ro lbl)
			) else (
				""
			)
	| ActSigPres(lbl, s, xs, ro) ->
			if Sc.is_extval s || Sc.is_lclval s  (* for values *) then (
				"? "^(obj2str ro)^"."^(mf2str (sig2id xs))^"   at"
				^(ro_lbll2str ro lbl)
			) else (
				""
			)
	| ActSigVal(lbl, s, xs, ro) ->
			if Sc.is_extval s || Sc.is_lclval s  (* for values *) then (
				"$ "^(obj2str ro)^"."^(mf2str (sig2id xs))^"   at"
				^(ro_lbll2str ro lbl)
			) else
				""
	| ActSigIn _
	| ActSigOut _ -> ""
	| ActBool(lbl, _, e, ro) -> (obj2str ro)^"\n     Expression at position:"^(ro_lbll2str ro lbl)
	| ActTimeInit _
	| ActTimeAdd _ -> ""
	| ActTimeCond(lbl, _, _, e, ro) -> (obj2str ro)^"\n     Expression at position:"^(ro_lbll2str ro lbl)
	| _ -> ""

let rec cycle2extnames = function
	| [] -> []
	| x:: l ->
			if Sc.is_act x
			then (act2errstr x):: (cycle2extnames l)
			else cycle2extnames l

let precedes x y =
	try List.mem y (rctexp2sgns (def_get (Sc.sgnorval2sgn x)).sc_def)
	with Not_found -> false

let rec shortcut_h x = function
	| [] -> []
	| y:: l ->
			if Sc.is_inp y || Sc.is_delta y then shortcut_h x l else
			if precedes x y then [y] else
				y:: (shortcut_h x l)
and shortcut = function
	| [] -> []
	| x:: l ->
			if Sc.is_inp x || Sc.is_delta x then shortcut l else
				let l = shortcut_h x (List.rev l) in
				x:: (shortcut (List.rev l))

let rec is_cycle_h a = function
	| [] -> precedes a a
	| [y] -> precedes y a
	| [x; y] -> precedes x y
	| x:: y:: l -> precedes x y && is_cycle (y:: l)
and is_cycle = function
	| [] -> false
	| a:: l -> is_cycle_h a (a:: l);;

let rec causality_reduce_h l1 = function
	| [] -> l1
	| y:: l2 -> if is_cycle (y:: l1) then y:: l1 else causality_reduce_h (y:: l1) l2
and causality_reduce l =
	match shortcut l with
	| [] -> []
	| x:: l1 -> if precedes x x then [x] else causality_reduce_h [x] l1

(* the main procedure *)
let causality_check () =
	( match cntxt.mode with
		| Meth ro -> ()
		| Node ro -> ()
		| Class ro -> check_labels ro !label_sequences
		| Appl _ -> ()
	);
	tsort := Tsort.init 1000;
	gen_precedences_and_pot_time_races();
	match Tsort.sort !tsort with
	| Tsort.Cycle cl ->
			let cl = causality_reduce cl in
			if debug_level DbgRctCausality then (
				print_all_defs();
				pn(); ps "causality cycle";
				print_sgn_l cl
			);
			let msg_l = cycle2extnames cl in
			( match msg_l with
				| [] -> Err.intern "internal causality cycle"
				| _ -> Err.msg (Err.CausalityCycle(msg_l))
			)
	| Tsort.Sorted sl ->
			check_for_time_races();
			if cntxt_is_class() then check_for_mult_emits_in_class();
			sl
	| _ -> Err.intern "causality_check"

(* -------------------------------------------------------------------------
REDUCTION of reactive equations

signals are (so far) substituted in terms if
- the defining equation consists of a singleton
- the defining equation consists of a "NNot (S x)"
------------------------------------------------------------------------- *)
let sgn2occ n s =
	try
		let d = Hashtbl.find eqns_tbl s in
		d.sc_occ <- d.sc_occ + n;
	with Not_found ->
			()

let rec rctexp2occs =
	let rctexpl2occs el = List.iter rctexp2occs el in
	function
	| TT
	| FF
	| S 1 -> ()
	| S x ->
			if Sc.is_extval x || Sc.is_inp x
			then ()
			else sgn2occ 1 x
	| CC x ->
			if cntxt_is_appl()
			then ()
			else sgn2occ 1 x
	| AAnd el -> rctexpl2occs el
	| OOr el -> rctexpl2occs el
	| NNot e -> rctexp2occs e

let rctexp2occs e =
	rctexp2occs e

let rec cll2occs =
	let expr2occs e = List.iter (sgn2occ 2) (Expr.expr2sgns e) in
	let blck2occs cc =
		List.iter
			( fun (e, cll) ->
						rctexp2occs e;
						cll2occs cll
			) cc in
	function
	| ActPrm(_, _, e, _, _) ->
			expr2occs e
	| ActInit _ ->
			()
	| ActAssgn(_, s, e, cc, _) ->
			expr2occs e;
			blck2occs cc
	| ActAssgnInBlck(_, s, e, _) ->
			expr2occs e
	| ActStmt(_, e, _) ->
			expr2occs e
	| ActProd(_, _, e1, e2, cc, _) ->
			expr2occs e1;
			expr2occs e2;
			blck2occs cc
	| ActSigInit _ ->
			()
	| ActEmit(_, _, s, _, Rct e, cc, _, _) ->
			rctexp2occs e;
			blck2occs cc
	| ActEmit(_, _, s, _, Val e, cc, _, _) ->
			expr2occs e;
			blck2occs cc
	| ActBlck(cc) ->
			blck2occs cc
	| ActSigPres _
	| ActSigVal _
	| ActSigIn _
	| ActSigOut _ ->
			()
	| ActBool(_, b, e, _) ->
			expr2occs e
	| ActTimeInit _
	| ActTimeAdd _ ->
			()
	| ActTimeCond(_, b, _, e, _) ->
			expr2occs e
	| ActDbg _ ->
			()

let eqn2occs s d =
	if Sc.is_dbg s then () else (
		rctexp2occs d.sc_def;
		match d.sc_act with
		| None -> ()
		| Some a -> cll2occs a
	)

let eqns2occs () =
	Hashtbl.iter eqn2occs eqns_tbl

let sgn_subst x =
	try let d = eqn_find x in
		match RctExp.reduce d.sc_def with
		| FF ->
				FF
		| TT ->
				TT
		| S y ->
				if cntxt_is_appl() && Sc.is_act x then S y else
				if Sc.is_reg y then (sgn2occ 1 y; S x) else
					S y
		| CC y ->
				CC y
		| e ->
				if cntxt_is_appl() && (Sc.is_act x || d.sc_occ <= 1)
				then e
				else S x
	with Not_found -> S x

let rec rctexp_subst e =
	match e with
	| S x ->
			if ( cntxt_is_appl() && not (Sc.is_cond x)
				|| not ( Sc.is_inp x || Sc.is_act x || Sc.is_extval x || (Sc.is_lclval x && Sc.is_val x )))
			then sgn_subst x
			else S x
	| NNot e -> NNot (rctexp_subst e)
	| AAnd el -> AAnd (rctexp_subst_l el)
	| OOr el -> OOr (rctexp_subst_l el)
	| e -> e
and rctexp_subst_l el = List.rev_map rctexp_subst el

let eqns_list sorted =
	List.rev
		( List.fold_left
				( fun l x ->
							try (eqn_find x):: l
							with Not_found -> l
				) [] sorted
		)

let rec call_subst =
	let blck2subst cc =
		List.map
			( fun (e, c) ->
						RctExp.reduce (rctexp_subst e), call_subst c
			) cc
	in
	function
	| ActEmit(lbl, f, sv, i, Rct e, cc, xs, ro) ->
			let e = RctExp.reduce (rctexp_subst e)
			and cc = blck2subst cc in
			if cntxt_is_appl()
			then ActBlck(cc)
			else ActEmit(lbl, f, sv, i, Rct e, cc, xs, ro)
	| ActEmit(lbl, f, sv, i, Val e, cc, xs, ro) ->
			let cc = blck2subst cc in
			ActEmit(lbl, f, sv, i, Val e, cc, xs, ro)
	| ActProd(lbl, acc, e1, e2, cc, ro) ->
			let cc = blck2subst cc in
			ActProd(lbl, acc, e1, e2, cc, ro)
	| ActBlck(cc) ->
			ActBlck(blck2subst cc)
	| ActAssgn(lbl, s, e, cc, ro) ->
			ActAssgn(lbl, s, e, blck2subst cc, ro)
	| ActAssgnInBlck(lbl, s, e, ro) ->
			ActAssgnInBlck(lbl, s, e, ro)
	| cll ->
			cll

let eqns_list_and_reduce sorted =
	List.rev
		( List.fold_left
				( fun l x ->
							try
								( let d = eqn_find x in
									let e = RctExp.reduce (rctexp_subst d.sc_def) in
									let e = match cntxt.mode with
										| Appl _ ->
												( match e with
													| OOr[S a; S r] when a = Sc.alpha && Sc.is_reg r ->
															let d' = mem_find r in
															let e' = RctExp.reduce(rctexp_subst d'.sc_def) in
															if e' = S x then (
																d'.sc_def <- TT;
																TT
															) else (
																d'.sc_def <- e';
																e
															)
													| OOr[S r; S a] when a = Sc.alpha && Sc.is_reg r ->
															let d' = mem_find r in
															let e' = RctExp.reduce(rctexp_subst d'.sc_def) in
															if e' = S x then (
																d'.sc_def <- TT;
																TT
															) else (
																d'.sc_def <- e';
																e
															)
													| _ -> e
												)
										| _ -> e
									in
									if debug_level DbgRctReduce then (
										let d' = rctexp_subst d.sc_def in
										pn(); print_sgn x;
										pnT 3; ps " orig  : "; print_rctexp d.sc_def;
										pnT 3; ps " subst : "; print_rctexp d';
										pnT 3; ps "  red  : "; print_rctexp e
									);
									d.sc_def <- e;
									( match d.sc_act with
										| None -> ()
										| Some cll -> d.sc_act <- Some (call_subst cll)
									);
									if Sc.is_act x then (
										match d.sc_act with
										| Some (ActBlck ([])) -> l
										| Some (ActSigInit _)
										| Some (ActInit _) when cntxt_is_appl() && d.sc_def = S Sc.alpha -> l
										| Some (ActSigPres _) when cntxt_is_appl() -> l
										| Some (ActSigVal _) when cntxt_is_appl() -> l
										| Some (ActEmit(_, _, _, _, Val e, _, _, _)) when cntxt_is_appl() && Expr.is_null e -> l
										| Some (ActEmit(_, _, _, _, Rct _, _, _, _)) when cntxt_is_appl() -> l
										| _ when cntxt_is_appl() && d.sc_def = FF -> l
										| _ -> d:: l
									) else (
										match d.sc_def with
										| FF | TT ->
												if Sc.is_extval x || (Sc.is_lclval x && Sc.is_val x)
												then d:: l
												else l
										| S y ->
												if Sc.is_extval x || (Sc.is_lclval x && Sc.is_val x)
												|| Sc.is_reg y || Sc.is_cond x
												then d:: l else l
										| _ ->
												if Sc.is_cond x || Sc.is_extval x
												|| (Sc.is_lclval x && Sc.is_val x)
												then d:: l
												else if cntxt_is_appl() && d.sc_occ <= 1
												then l
												else d:: l
									)
								)     (* in methods a signal may not be declared *)
							with Not_found -> l
				) [] sorted
		)

let def_reduce d =
	let d' = rctexp_subst d.sc_def in
	let e = RctExp.reduce d' in
	if debug_level DbgRctReduce then (
		pn(); print_sgn d.sc_sgn;
		pnT 3; ps " orig  : "; print_rctexp d.sc_def;
		pnT 3; ps " subst : "; print_rctexp d';
		pnT 3; ps "  red  : "; print_rctexp e
	);
	d.sc_def <- e;
	d

let mems_reduce knd =
	let r = ref [] in
	let mem_reduce s d =
		if d.sc_knd = knd then
			if d.sc_occ <= 0 && not(Sc.is_act d.sc_sgn || Sc.is_extval d.sc_sgn) then (
			(* skip *)
			) else (
				let d = def_reduce d in
				if Sc.is_act d.sc_sgn && d.sc_def = FF then (
				(* skip *)
				) else (
					( match d.sc_act with
						| None -> ()
						| Some cll -> d.sc_act <- Some (call_subst cll)
					);
					r := d::!r
				)
			)
	in
	Hashtbl.iter mem_reduce eqns_tbl;
	!r

let dbgs_reduce () =
	let r = ref [] in
	let dbg_reduce s d =
		if d.sc_knd = Dbg
		then r := (def_reduce d)::!r
		else ()
	in
	Hashtbl.iter dbg_reduce eqns_tbl;
	!r

(* -------------------------------------------------------------------------
main function for semantic analysis
-------------------------------------------------------------------------- *)
let reset_tables4analysis () =
	Hashtbl.clear eqns_tbl;
	List.iter eqn_add !r.defs;
	Hashtbl.clear alts_tbl;
	List.iter sp_add !r.alts;
	Hashtbl.clear sgn2regs_tbl;
	List.iter (fun (x, y) -> Hashtbl.add sgn2regs_tbl x y) !r.dpds

let check_causality_and_reduce () =
	reset_tables4analysis();
	let sorted = causality_check () in
	aggregate_bool_emits();
	let eqns =
		match cntxt.mode with
		| Class _
		| Meth _
		| Node _ -> eqns_list sorted
		| _ -> eqns2occs ();
				eqns_list_and_reduce sorted in
	let mems =
		match cntxt.mode with
		| Class _
		| Meth _
		| Node _ -> all_mems()
		| _ -> (mems_reduce Mem)@(mems_reduce Nxt)
	and dbgs =
		match cntxt.mode with
		| Class _
		| Meth _
		| Node _ -> all_dbgs()
		| _ -> dbgs_reduce() in
	eqns, mems, dbgs

(* -------------------------------------------------------------------------
SYNCHRONOUS COMPONENTS
-------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------
TRANSLATION of a method
------------------------------------------------------------------------- *)
let sc_method ~is_node dl p rho =
	if is_node then (
		push_new_rctenv Sc.alpha' f_tick'
	) else (
		push_new_rctenv Sc.alpha f_tick
	);
	(* translate declarations and body *)
	cntxt.mode <- if is_node then Node rho.ro else Meth rho.ro;
	let dl = dl rho Sc.alpha' Sc.mu' Sc.tau' in
	let rho = { rho with globs = dl.syd_glbs } in
	let p = p rho Sc.alpha' Sc.beta' Sc.tau' [] Sc.mu' in
	(* set "alpha" for semantic analysis *)
	(* context needs to be reset since it may change translating p *)
	cntxt.mode <- if is_node then Node rho.ro else Meth rho.ro;
	let (eqns, mems, dbgs) = check_causality_and_reduce ()
	and dcls = !r.dcls
	and alts = alts_tbl2list()
	and dpds = sgn2regs_tbl2list() in
	pop_rctenv();
	{ sc_ro = rho.ro;
		(* keeps the order of declarations, cf syd_dep_declarations *)
		sc_dcls = dcls;
		sc_flds = [];
		sc_fmls = dl.syd_fmls;
		sc_alfa = dl.syd_alfa@p.sya_alfa;
		sc_acts = dl.syd_acts@p.sya_acts;
		sc_regs = p.sya_regs;
		sc_alts = alts;
		sc_dpds = dpds;
		sc_omga = p.sya_omga;
		sc_ctrl = p.sya_ctrl;
		sc_inst = p.sya_inst;
		sc_splt = p.sya_splt;
		sc_defs = eqns@mems@dbgs;
		sc_axms = [];
		sc_prps = [];
	}
(*
let sc_node dl p rho =
push_new_node_env Sc.alpha';
(* translate declarations and body *)
let dl = dl rho Sc.alpha' Sc.mu' in
let p = p rho Sc.alpha' Sc.beta' Sc.tau' Sc.tau' [] Sc.mu' in
(* set "alpha" for semantic analysis *)
cntxt.mode <- Meth rho.ro;
let (eqns, mems, dbgs) = check_causality_and_reduce ()
and dcls = !r.dcls
and alts = alts_tbl2list()
and dpds = sgn2regs_tbl2list() in
pop_rctenv();
{ sc_ro = rho.ro;
(* keeps the order of declarations, cf syd_dep_declarations *)
sc_dcls = !r.dcls;
sc_flds = [];
sc_fmls = dl.syd_fmls;
sc_alfa = dl.syd_alfa@p.sya_alfa;
sc_acts = dl.syd_acts@p.sya_acts;
sc_regs = p.sya_regs;
sc_alts = alts_tbl2list();
sc_dpds = sgn2regs_tbl2list();
sc_omga = p.sya_omga;
sc_ctrl = p.sya_ctrl;
sc_inst = p.sya_inst;
sc_splt = p.sya_splt;
sc_defs = eqns@mems@dbgs;
}
*)
(* -------------------------------------------------------------------------
TRANSLATION of a component
------------------------------------------------------------------------- *)
let sc_reset () =
	Sc.reset_sgn_counters();
	reset_rctenv()

let sc_class ro ~fields ~params p =
	sc_reset ();
	cntxt.mode <- Class ro;
	label_sequences := [];
	(* translate declarations and body *)
	let rho = new_env (ro2cid ro) ro in
	let prcps = fields rho Sc.alpha Sc.alpha Sc.tau in
	push_new_rctenv Sc.alpha f_tick;
	let parms = params rho Sc.alpha Sc.alpha Sc.tau in
	let rho =
		{ rho with globs = parms.syd_glbs;
			flds = prcps.syd_glbs
		} in
	let p = p rho Sc.alpha Sc.beta Sc.tau [] Sc.alpha in
	cntxt.mode <- Class ro;
	amalgamate_rctenv();
	let (eqns, mems, dbgs) = check_causality_and_reduce () in
	{ sc_ro = ro;
		(* keeps the order of declarations, cf syd_dep_declarations *)
		sc_dcls = !r.dcls;
		sc_flds = prcps.syd_fmls;
		sc_fmls = parms.syd_fmls;
		sc_alfa = prcps.syd_alfa@parms.syd_alfa@p.sya_alfa;
		sc_acts = prcps.syd_acts@parms.syd_acts@p.sya_acts;
		sc_regs = p.sya_regs;
		sc_alts = alts_tbl2list();
		sc_dpds = sgn2regs_tbl2list();
		sc_omga = p.sya_omga;
		sc_ctrl = p.sya_ctrl;
		sc_inst = p.sya_inst;
		sc_splt = p.sya_splt;
		sc_defs = eqns@mems@dbgs;
		sc_axms = (i_ast (ro2cid ro)).axioms;
		sc_prps = (i_ast (ro2cid ro)).props;
	}

(* -------------------------------------------------------------------------
LINKING
- links reactive methods and reactive objects

------------------------------------------------------------------------- *)

(* copy a reactive signal to a reactive signal. Used in the reactive
part only *)

let cp_sgn_tbl = Hashtbl.create 1991
let cp_sig_tbl = Hashtbl.create 23
let cp_dim_map_l = ref []
let cp_fml2expr_l = ref []

let cp_reset () =
	Hashtbl.clear cp_sgn_tbl;
	Hashtbl.clear cp_sig_tbl;
	cp_dim_map_l := [];
	cp_fml2expr_l := []

let cp_sgn s =
	let s' = Sc.sgnorval2sgn s in
	try
		let s'' = Hashtbl.find cp_sgn_tbl s' in
		if Sc.is_val s then Sc.sgn2val s'' else s''
	with Not_found ->
			let s'' = Sc.next_sgn s' in
			if debug_level DbgRctCopy then (
				pn(); ps "copy_sgn: "; print_sgn s; ps " -> ";
				print_sgn (if Sc.is_val s then Sc.sgn2val s'' else s'')
			);
			Hashtbl.add cp_sgn_tbl s' s'';
			if Sc.is_val s then Sc.sgn2val s'' else s''

let cp_sgn_add s s' =
	let s = Sc.sgnorval2sgn s in
	let s' = Sc.sgnorval2sgn s' in
	try ignore (Hashtbl.find cp_sgn_tbl s)
	with Not_found ->
			Hashtbl.add cp_sgn_tbl s s';
			if debug_level DbgRctCopy then (
				pn(); ps "copy_sgn_add: "; print_sgn s; ps " -> "; print_sgn s'
			)

let cp_sig_mem fxs axs =
	match sig2mem fxs, sig2mem axs with
	| None, _ -> ()
	| Some m, None ->
			let m' = cp_sgn m in
			if debug_level DbgRctCopy then (
				pn(); ps "cp_sig_mem: actual "; print_sig axs;
				ps " <- "; print_sgn m';
			);
			mem_add axs m'
	| Some m, Some m' ->
			if debug_level DbgRctCopy then (
				pn(); ps "cp_sig_mem: "; print_sgn m;
				ps " -> "; print_sgn m'
			);
			cp_sgn_add m m'

let cp_dim_map_add (id: tmfid) (d: tdim) =
	try
		if List.assoc id !cp_dim_map_l = d then () else
			Err.intern "cp_dim_map_add:1"
	with Not_found ->
			if debug_level DbgRctCopy then (
				pn(); ps "cp_dim_map: id "; ps (mf2str id); ps " -> ";
				match d with
				| DimLen n -> pi n
				| DimVar id -> ps (mf2str id)
				| _ -> Err.intern "cp_dim_map_add:2"
			);
			cp_dim_map_l := (id, d)::!cp_dim_map_l

let cp_dim_map id =
	try List.assoc id !cp_dim_map_l
	with Not_found -> Err.intern "cp_dim_map_find"

let cp_dim = function
	| DimVar id, n -> cp_dim_map id, n
	| d -> d

let cp_sig_buf_dim fxs axs =
	match sig2buf fxs, sig2buf axs with
	| [], _ -> ()
	| d, _ -> add_buf_dim axs (List.map cp_dim d)

let rec cp_typ t =
	match t with
	| Array(t, DimLen 1, DimVar id) ->
			Array(t, DimLen 1, cp_dim_map id)
	| Array(t, DimVar id1, DimVar id2) ->
			Array(t, cp_dim_map id1, cp_dim_map id2)
	| Typ(id, tl) ->
			Typ(id, List.map cp_typ tl)
	| _ -> t

let cp_typ_add t t' =
	match t, t' with
	| Array(_, DimLen 1, DimVar id), Array(_, DimLen 1, DimLen n) ->
			cp_dim_map_add id (DimLen n)
	| Array(_, DimLen 1, DimVar id), Array(_, DimLen 1, DimVar id') ->
			cp_dim_map_add id (DimVar id')
	| Array(_, DimVar id1, DimVar id2), Array(_, DimLen n1, DimLen n2) ->
			cp_dim_map_add id1 (DimLen n1);
			cp_dim_map_add id2 (DimLen n2)
	| Array(_, DimVar id1, DimVar id2), Array(_, DimVar id1', DimVar id2') ->
			cp_dim_map_add id1 (DimVar id1');
			cp_dim_map_add id2 (DimVar id2')
	| _ -> ()

let cp_entity ent =
	{ ent with p_type = cp_typ ent.p_type }

let cp_expr e =
	if !cp_dim_map_l = []
	then Expr.sgn_map cp_sgn None e
	else Expr.sgn_map cp_sgn (Some cp_dim_map) e

let rec cp_frq f =
	{ ff = cp_sgn f.ff;
		cd = cp_expr f.cd;
		bf =
			( match f.bf with
				| None -> None
				| Some bf -> Some (cp_frq bf)
			);
	}

let cp_sgndcl d =
	{ sgn = cp_sgn d.sgn;
		buf = d.buf;
		frq = cp_frq d.frq;
		mem =
			( match d.mem with
				| None -> None
				| Some m -> Some (cp_sgn m)
			);
		aa = cp_sgn d.aa;
	}

let cp_sig xs =
	try Hashtbl.find cp_sig_tbl (sig2id xs, sig2sgn xs)
	with Not_found ->
			let dcl = some xs.rct_sgndcl "cp_sig" in
			let xs' =
				{ xs with rct_sgndcl = Some (cp_sgndcl dcl);
					trs_styp = cp_typ (sig2typ xs)
				} in
			Hashtbl.add cp_sig_tbl (sig2id xs, sig2sgn xs) xs';
			xs'

let cp_sig_add fxs axs =
	try ignore (Hashtbl.find cp_sig_tbl (sig2id fxs, sig2sgn fxs))
	with Not_found ->
			Hashtbl.add cp_sig_tbl (sig2id fxs, sig2sgn fxs) axs

(* substitute a parameter by an expression syntactically. for clock check *)
let cp_fml2expr_find f =
	try List.assoc f !cp_fml2expr_l
	with Not_found ->
			Err.intern "cp_fml2expr_find"

let cp_fml2expr_add f e =
	let f = match f with
		| SigPrm xs -> sig2id xs
		| VarPrm { prm_ent = v }-> v.p_name
		| _ -> Err.intern "cp_fml2expr_add"
	in
	try ignore (List.assoc f !cp_fml2expr_l)
	with Not_found ->
			if debug_level DbgRctCopy then (
				pn(); ps "cp_fml2expr_add: ";
				ps (mf2str f); ps " -> "; ps (expr2str e)
			);
			cp_fml2expr_l := (f, e.expr)::!cp_fml2expr_l

let cp_actuals e =
	Expr.sig_map cp_fml2expr_find e

(* for copying reactive equations we know that, for principal signal,
equations may be specified in several components. hence we have
to take the disjunction of these equations, hence the "add_or" *)

let rec cp_rctdef ro lb d =
	if Sc.is_extval d.sc_sgn || Sc.is_mem d.sc_sgn || Sc.is_pre d.sc_sgn
	then
		def_or d.sc_knd (cp_sgn d.sc_sgn) (cp_rctexp d.sc_def)
	else
		match d.sc_act with
		| None -> def_add d.sc_knd (cp_sgn d.sc_sgn) (cp_rctexp d.sc_def)
		| Some a -> def_act d.sc_knd (cp_sgn d.sc_sgn) (cp_rctexp d.sc_def) (cp_cll ro lb a)

and cp_rctexp = function
	| S x -> S (cp_sgn x)
	| CC x -> CC (cp_sgn x)
	| FF -> FF
	| TT -> TT
	| AAnd a -> AAnd (List.rev_map cp_rctexp a)
	| OOr a -> OOr (List.rev_map cp_rctexp a)
	| NNot x -> NNot (cp_rctexp x)

and cp_cll ro lb =
	(* ~add = true : valid label, must be extended and added to the
	valid label sequences
	~add = false : invalid label, do not add to the label sequences
	*)
	let mk_lbl ~add lbl =
		match lb with
		| None -> lbl
		| Some lb ->
				if add then add_label_seq (lb:: lbl) else ();
				lb:: lbl
	and cp_blck cc = List.map (fun(e, c) -> cp_rctexp e, cp_cll ro lb c) cc in
	function
	| ActInit(lbl, s, t, _) ->
			ActInit(mk_lbl ~add: false lbl, cp_sgn s, cp_typ t, ro)
	| ActStmt(lbl, e, _) ->
			ActStmt(mk_lbl ~add: true lbl, cp_expr e, ro)
	| ActProd(lbl, ac, e1, e2, cc, _) ->
			ActProd(mk_lbl ~add: true lbl, cp_sgn ac, cp_expr e1, cp_expr e2, cp_blck cc, ro)
	| ActPrm(lbl, s, e, x, _) ->
			ActPrm(mk_lbl ~add: true lbl, cp_sgn s, cp_expr e, x, ro)
	| ActBlck(cc) ->
			ActBlck(cp_blck cc)
	| ActSigInit(lbl, sv, xs, _) ->
			ActSigInit(mk_lbl ~add: false lbl, cp_sgn sv, cp_sig xs, ro)
	| ActEmit(lbl, f, sv, i, Rct e, cc, xs, _) ->
			ActEmit(mk_lbl ~add: true lbl, f, cp_sgn sv, i, Rct(cp_rctexp e), cp_blck cc, cp_sig xs, ro)
	(* pure signals cannot be labelled *)
	| ActEmit(lbl, f, sv, i, Val e, cc, xs, _) when Expr.is_null e ->
			ActEmit(mk_lbl ~add: false lbl, f, cp_sgn sv, i, Val(cp_expr e), cp_blck cc, cp_sig xs, ro)
	| ActEmit(lbl, f, sv, i, Val e, cc, xs, _) ->
			ActEmit(mk_lbl ~add: true lbl, f, cp_sgn sv, i, Val(cp_expr e), cp_blck cc, cp_sig xs, ro)
	| ActSigPres(lbl, s, xs, _) ->
			ActSigPres(mk_lbl ~add: false lbl, cp_sgn s, cp_sig xs, ro)
	| ActSigVal(lbl, s, xs, _) ->
			ActSigVal(mk_lbl ~add: false lbl, cp_sgn s, cp_sig xs, ro)
	| ActSigIn(lbl, xs, _) ->
			ActSigIn(mk_lbl ~add: false lbl, cp_sig xs, ro)
	| ActSigOut(lbl, xs, _) ->
			ActSigOut(mk_lbl ~add: false lbl, cp_sig xs, ro)
	| ActAssgn(lbl, s, e, cc, _) ->
			ActAssgn(mk_lbl ~add: false lbl, cp_sgn s, cp_expr e, cp_blck cc, ro)
	| ActAssgnInBlck(lbl, s, e, _) ->
			ActAssgnInBlck(mk_lbl ~add: false lbl, cp_sgn s, cp_expr e, ro)
	| ActBool(lbl, b, e, _) ->
			ActBool(mk_lbl ~add: true lbl, cp_sgn b, cp_expr e, ro)
	| ActTimeInit(lbl, s, _) ->
			ActTimeInit(mk_lbl ~add: true lbl, cp_sgn s, ro)
	| ActTimeAdd(lbl, s, _) ->
			ActTimeAdd(mk_lbl ~add: true lbl, cp_sgn s, ro)
	| ActTimeCond(lbl, b, s, e, _) ->
			ActTimeCond(mk_lbl ~add: true lbl, cp_sgn b, cp_sgn s, cp_expr e, ro)
	| ActDbg(lbl, _) ->
			ActDbg(mk_lbl ~add: false lbl, ro)

let cp_alts (x, y) = !r.alts <- (List.map cp_sgn x, List.map cp_sgn y)::!r.alts
let cp_dpds (x, y) = !r.dpds <- (cp_sgn x, List.map cp_sgn y)::!r.dpds

let cp_axiom ro sp = !r.axms <- (sp, ro)::!r.axms
let cp_prop ro sp = !r.prps <- (sp, ro)::!r.prps

let cp_int_dcl ro = function
	| LclFld (s, x) -> add_field_dcl (cp_sgn s) (cp_entity x)
	| IntSig (s, t, n) -> add_internal_sig_dcl (cp_sgn s) (cp_typ t) n
	| LclSig (xs) -> add_local_sig_dcl (cp_sig xs)

(* parameter passing for dimension parameters *)
let cp_sig_dim_param fxs ap =
	match ap.sye_val with
	| Val xe ->
			let ftyp = some (sigtyp2valtyp (sig2typ fxs)) "cp_sig_dim_p" in
			cp_typ_add ftyp (Expr.expr2typ xe)
	| Rct _ -> ()

let cp_val_dim_param fp ap =
	let fp_vtyp = fp.prm_ent.p_type
	and ap_vtyp = Expr.expr2typ ap.sye_xe in
	cp_typ_add fp_vtyp ap_vtyp

let cp_dim_param_l fpl apl =
	List.iter2
		( fun fp ap ->
					match fp with
					| SigPrm xs -> cp_sig_dim_param xs ap
					| VarPrm x -> cp_val_dim_param x ap
					| _ -> Err.intern "cp_dim_param_l"
		) fpl apl

(* parameter passing for value and signal parameters *)
let cp_sig_param is_node lbl fxs ap bc ro alpha kappa =
	let xc = sig2clck fxs and xc' = ap.sye_freq.cd in
	if debug_level DbgParamList then (
		pn(); ps "Clock Check for Method Parameters: ";
		pnT 3; ps " formal at "; print_expr xc;
		pnT 3; ps " actual at "; print_expr xc'
	);
	let cid = ro2cid ro in
	if is_node then (
		(* check that all signals on true have the same clock *)
		if (sig2frq fxs).cd = v_true then (
			try
				let ff = Hashtbl.find cp_sgn_tbl (sig2frq fxs).ff in
				if ff <> ap.sye_freq.ff then
					Err.msg(Err.InconsistentClocksInNodeCall(cid, ap.sye_xe.elbl))
			with Not_found -> ()
		);
	) else (
		if not(are_clocks_equal_for_node_call bc (cp_actuals xc) xc')
		then
			Err.msg(Err.ClocksInconsistentInMethodCall(cid, ap.sye_xe.elbl))
		;
	);
	cp_sgn_add (sig2frq fxs).ff ap.sye_freq.ff;
	match ap.sye_val with
	| Rct (S s) ->
			cp_sgn_add (sig2sgn fxs) s;
			cp_sig_mem fxs (sgn2sig s (ro2sigs ro));
			[],[], kappa
	| Rct v ->
			let s = Sc.next_var() in
			def_add Eqn s (OOr[AAnd[v; S alpha]; kappa]);
			cp_sgn_add (sig2sgn fxs) s;
			[s],[], kappa
	| Val { expr = SigVal(_, axs, _) } ->
			cp_sgn_add (sig2sgn fxs) (sig2sgn axs);
			cp_sig_add fxs axs;
			cp_sig_mem fxs axs;
			cp_sig_buf_dim fxs axs;
			[],[], kappa
	| Val v ->
			let s = Sc.next_aux() in
			let sgndcl = { sgn = s; buf =[]; frq = ap.sye_freq; mem = None; aa = alpha }
			and typ = Typ(id_signal,[Expr.expr2typ ap.sye_xe]) in
			let axs = { fxs with rct_sgndcl = Some sgndcl; trs_styp = typ } in
			add_local_sig_dcl axs;
			cp_sgn_add (sig2sgn fxs) (sig2sgn axs);
			cp_sig_add fxs axs;
			cp_sig_mem fxs axs;
			cp_sig_buf_dim fxs axs;
			let lbll = match lbl with
				| None -> [v.elbl]
				| Some lbl -> [lbl; v.elbl]
			in
			let a = Sc.next_act() in
			def_act Eqn a (OOr[S alpha; kappa]) (ActAssgn(lbll, s, v,[], ro));
			[s; a],[a], CC a

let cp_val_param lbl fp ap ro alpha kappa =
	let v =
		match ap.sye_val with
		| Val v -> v
		| _ -> Err.intern "cp_val_param:1" in
	let x = Sc.next_var() in
	cp_sgn_add (some fp.prm_sgn "cp_val_param:2") x;
	let lbll =
		match lbl with
		| None -> [v.elbl]
		| Some lbl -> [lbl; v.elbl] in
	let a = Sc.next_act() in
	def_act Eqn a (OOr[S alpha; kappa]) (ActPrm(lbll, x, v, fp.prm_ent, ro));
	a:: ap.sye_alfa,[a], CC a

let cp_param_l is_node lbl fpl apl ro alpha kappa =
	let bc = if apl =[] || not (cntxt_is_node())
		then None
		else Some(List.hd apl).sye_freq.cd
	in
	let rec cp fpl apl kappa =
		match fpl, apl with
		| [],[] ->
				[],[], kappa
		| fp:: fpl, ap:: apl ->
				let alfa1, acts1, kappa1 =
					match fp with
					| SigPrm xs -> cp_sig_param is_node lbl xs ap bc ro alpha kappa
					| VarPrm x -> cp_val_param lbl x ap ro alpha kappa
					| _ -> Err.intern "cp_param_l:1"
				in
				let alfa2, acts2, kappa2 = cp fpl apl kappa1 in
				alfa1@alfa2, acts1@acts2, kappa2
		| _ -> Err.intern "cp_param_l:2"
	in
	cp fpl apl kappa

let cp_principals fmls ro =
	List.iter
		( fun fxs ->
					try let axs = sig_find (ro2sigs ro) (sig2id fxs) in
						cp_sgn_add (sig2sgn fxs) (sig2sgn axs);
						cp_sgn_add (sig2frq fxs).ff (sig2frq axs).ff;
						cp_sig_mem fxs axs;
						cp_sig_buf_dim fxs axs
					with Not_found ->
							Err.intern ("cp_principals:\n obj: "^(obj2str ro)^
									"\n sgn: "^(sig2str fxs))
		) fmls

(* -------------------------------------------------------------------------
METHOD CALL

- translate the actual parameters, and link them to the formal ones
- link the respective method body
- first the copy table is built (matching environment & parameters)
- then the body is copied
------------------------------------------------------------------------- *)
let sc_link_method ~is_node lbl sc el ap rho alpha beta tau chi mu =
	(* reset the copy tables *)
	cp_reset();
	(* compute syntactic substitutions of parameters by actuals *)
	List.iter2 cp_fml2expr_add sc.sc_fmls el;
	(* translate the parameters them *)
	let aps, alfa1, acts1, cc = sye_exp_l ap rho alpha mu tau in
	(* bind the dimension parameters *)
	cp_dim_param_l sc.sc_fmls aps;
	(* copy the parameters them *)
	let alfa2, acts2, kappa2 =
		cp_param_l is_node (Some lbl) sc.sc_fmls aps rho.ro alpha FF in
	let alpha3, alfa3 =
		match kappa2 with
		| FF -> alpha,[]
		| e ->
				let y = Sc.next_gamma() in
				def_add Eqn y (OOr[S alpha; e]);
				y,[y] in
	(* copy the environment *)
	cp_sgn_add Sc.alpha Sc.alpha;
	cp_sgn_add Sc.beta Sc.beta;
	cp_sgn_add Sc.tau Sc.tau;
	cp_sgn_add Sc.alpha' alpha3;
	cp_sgn_add Sc.beta' beta;
	cp_sgn_add Sc.tau' tau;
	cp_sgn_add Sc.mu' mu;
	(* copy the fields *)
	cp_principals (ro2sigs rho.ro) rho.ro;
	(* copy the internal declarations *)
	List.iter (cp_int_dcl sc.sc_ro) sc.sc_dcls;
	(* copy the body *)
	List.iter (cp_rctdef sc.sc_ro (Some lbl)) sc.sc_defs;
	List.iter cp_alts sc.sc_alts;
	List.iter cp_dpds sc.sc_dpds;
	{ sya_alfa = alfa1@alfa2@alfa3@acts2@(List.rev_map cp_sgn sc.sc_alfa);
		sya_acts = acts1@acts2@(List.rev_map cp_sgn sc.sc_acts);
		sya_regs = List.rev_map cp_sgn sc.sc_regs;
		sya_omga = cp_rctexp sc.sc_omga;
		sya_taus = [];
		sya_ctrl = List.rev_map cp_rctexp sc.sc_ctrl;
		sya_inst = sc.sc_inst;
		sya_splt = sc.sc_splt;
	}

(* -------------------------------------------------------------------------
OBJECT CREATION

- generate the declaration
- link the respective class body
- first the copy table is built (matching environment & parameters)
- then the body is copied
------------------------------------------------------------------------- *)
let sc_object sc ro dcll rho alpha beta tau chi mu =
	if debug_level DbgParamList then (
		pn(); ps "link object: "; ps (obj2str ro)
	);
	(* reset the copy tables *)
	cp_reset();
	(* translate the actuals and relate properly to the signal bus *)
	let dcll = dcll (new_env (ro2cid ro) ro) alpha mu tau in
	let alpha', alfa =
		match dcll.syd_caus with
		| FF -> alpha,[]
		| e ->
				let y = Sc.next_gamma() in
				def_add Eqn y (OOr[S alpha; e]);
				y,[y] in
	(* copy the environment *)
	Hashtbl.add cp_sgn_tbl Sc.tick Sc.tick;
	Hashtbl.add cp_sgn_tbl Sc.alpha alpha';
	Hashtbl.add cp_sgn_tbl Sc.beta beta;
	Hashtbl.add cp_sgn_tbl Sc.tau tau;
	(* copy the the fields and constructor parameters to principals *)
	let fmls =
		List.map
			(function SigPrm xs -> xs | _ -> Err.intern "sc_object:fml")
			(sc.sc_flds@sc.sc_fmls)
	in
	cp_principals fmls ro;
	(* copy the internal declarations *)
	List.iter (cp_int_dcl ro) sc.sc_dcls;
	(* copy the body *)
	List.iter (cp_rctdef ro None) sc.sc_defs;
	List.iter cp_alts sc.sc_alts;
	List.iter (cp_axiom ro) sc.sc_axms;
	List.iter (cp_prop ro) sc.sc_prps;
	{ sya_alfa = alfa@dcll.syd_alfa@(List.rev_map cp_sgn sc.sc_alfa);
		sya_acts = dcll.syd_acts@(List.rev_map cp_sgn sc.sc_acts);
		sya_regs = List.rev_map cp_sgn sc.sc_regs;
		sya_omga = cp_rctexp sc.sc_omga;
		sya_taus = [];
		sya_ctrl = List.rev_map cp_rctexp sc.sc_ctrl;
		sya_inst = sc.sc_inst;
		sya_splt = sc.sc_splt;
	}

(* -------------------------------------------------------------------------
Reduction of Wires
------------------------------------------------------------------------- *)
let reduce_wires eqns mems dbgs =
	let wire_counter = ref 0
	and free_wires = ref []
	and wire_xrf = Hashtbl.create (List.length eqns) in
	let rec next_wire () = wire_counter:=!wire_counter + 1;!wire_counter
	and rewire d =
		match d.sc_act with
		| None ->
				( try
					let w = Hashtbl.find wire_xrf d.sc_sgn in
					free_wires := w::!free_wires;
					Hashtbl.remove wire_xrf d.sc_sgn;
					Some { d with sc_sgn = w; sc_def = rewire_rctexp d.sc_def }
				with Not_found ->
						None
				);
		| Some a ->
				Some { d with sc_def = rewire_rctexp d.sc_def; sc_act = Some (rewire_call a) }
	and rewire_sgn s =
		if is_inp_sgn s || Sc.is_extval s || Sc.is_act s
		|| Sc.is_reg_or_mem_or_pre s || Sc.is_lcl s || Sc.is_aux s
		then s
		else
			try Hashtbl.find wire_xrf s
			with Not_found ->
					if !free_wires <> [] then (
						let w = List.hd !free_wires in
						Hashtbl.add wire_xrf s w;
						pn(); ps "rewire "; print_sgn s; ps "  wire "; pi w;
						free_wires := List.tl !free_wires;
						w
					) else (
						let w = next_wire() in
						pn(); ps "rewire "; print_sgn s; ps "  wire "; pi w;
						Hashtbl.add wire_xrf s w;
						w
					)
	and rewire_rctexp = function
		| TT -> TT
		| FF -> FF
		| S s -> S (rewire_sgn s)
		| CC _ -> Err.intern "rewire_rctexp"
		| NNot e -> NNot (rewire_rctexp e)
		| AAnd el -> AAnd (rewire_rctexp_l el)
		| OOr el -> OOr (rewire_rctexp_l el)
	and rewire_rctexp_l el = List.rev_map rewire_rctexp el
	and rewire_blck cc = List.map (fun(e, c) -> rewire_rctexp e, rewire_call c) cc
	and rewire_call =
		function
		| ActSigInit (lbl, sv, t, ro) -> ActSigInit(lbl, sv, t, ro)
		| ActInit (lbl, s, t, ro) -> ActInit(lbl, s, t, ro)
		| ActStmt (lbl, e, ro) -> ActStmt(lbl, e, ro)
		| ActProd (lbl, a, e1, e2, cc, ro) -> ActProd (lbl, a, e1, e2, cc, ro)
		| ActPrm (lbl, id, e, id', ro) -> ActPrm(lbl, id, e, id', ro)
		| ActBlck (cc) -> ActBlck(rewire_blck cc)
		| ActEmit (lbl, f, sv, i, e, cc, xs, ro) -> ActEmit(lbl, f, sv, i, e, rewire_blck cc, xs, ro)
		| ActSigPres (lbl, s, xs, ro) -> ActSigPres(lbl, s, xs, ro)
		| ActSigVal (lbl, s, xs, ro) -> ActSigVal(lbl, s, xs, ro)
		| ActSigIn (lbl, xs, ro) -> ActSigIn(lbl, xs, ro)
		| ActSigOut (lbl, xs, ro) -> ActSigOut(lbl, xs, ro)
		| ActAssgn (lbl, s, e, cc, ro) -> ActAssgn(lbl, s, e, rewire_blck cc, ro)
		| ActAssgnInBlck (lbl, s, e, ro) -> ActAssgnInBlck(lbl, s, e, ro)
		| ActBool(lbl, b, e, ro) -> ActBool(lbl, rewire_sgn b, e, ro)
		| ActTimeInit (lbl, s, ro) -> ActTimeInit(lbl, s, ro)
		| ActTimeAdd (lbl, s, ro) -> ActTimeAdd(lbl, s, ro)
		| ActTimeCond (lbl, b, s, e, ro) -> ActTimeCond(lbl, rewire_sgn b, s, e, ro)
		| ActDbg (lbl, ro) -> ActDbg (lbl, ro)
	in
	let _ = List.fold_left
			( fun l d ->
						match rewire d with
						| None -> l
						| Some d -> d:: l
			) [] eqns
	in
	!wire_counter

(* -------------------------------------------------------------------------
APPLICATION
------------------------------------------------------------------------- *)
let split_eqns_for_matlab dl sigs =
	let is_pre_post x y = (Tsort.is_pre_post !tsort x y) in
	let outs = List.filter is_output_sig sigs in
	let outs = List.map sig2sgn outs in
	let sgns = ref outs and vals = ref (List.rev_map Sc.sgn2val outs) in
	List.iter
		( fun d ->
					if List.exists (is_pre_post d.sc_sgn) !sgns then (
						sgns := d.sc_sgn:: (rctexp2sgns d.sc_def)@(!sgns);
						match d.sc_act with
						| None -> ()
						| Some c ->
								vals := match c with
								| ActEmit (_, _, sv, _, _, _, _, _) ->
										if List.mem sv !vals
										then (Expr.cll2vals c)@(!vals)
										else !vals
								| ActBool(_, b, _, _) ->
										if List.mem b !sgns
										then (Expr.cll2vals c)@(!vals)
										else !vals
								| ActProd(_, acc, _, _, _, _) ->
										if List.mem acc !vals
										then (Expr.cll2vals c)@(!vals)
										else !vals
								| _ ->
										(Expr.cll2vals c)@(!vals)
					)
		) (List.rev dl);
	let observables = List.rev_map Sc.sgnorval2sgn (!sgns@ !vals) in
	if debug_level DbgRctCopy then (
		pn(); ps "observables";
		List.iter (fun s -> pnT 2; print_sgn s) observables;
	);
	observables

let rec final_split = function
	| S x ->
			if x = Sc.alpha then (S x, FF) else
			if x = Sc.beta then (FF, S x) else
			if is_beta_sgn x then (FF, S x) else
				S x, S x
	| NNot e -> let (a, b) = final_split e in (NNot a, NNot b)
	| AAnd el -> let (a, b) = final_split_l el in (AAnd a, AAnd b)
	| OOr el -> let (a, b) = final_split_l el in (OOr a, OOr b)
	| _ -> Err.intern "final_split"
and final_split_l el = List.split (List.map final_split el)

let rec final_defs dl la lb =
	let count = ref 0 in
	match dl with
	| [] -> (la, lb)
	| d:: dl ->
			( match final_split d.sc_def with
				| FF, FF -> (la, lb)
				| FF, b -> d.sc_def <- b; (la, d:: lb)
				| a, FF -> d.sc_def <- a; (d:: la, lb)
				| a, b ->
						let d' = { sc_knd = d.sc_knd;
							sc_sgn = d.sc_sgn;
							sc_def = b;
							sc_act = d.sc_act;
							sc_sig = d.sc_sig;
							sc_occ = d.sc_occ;
						}
						in
						count := !count + 1;
						pn(); ps "count  "; pi !count;
						d.sc_def <- a;
						d:: la, d':: lb
			)

let sc_appl rtl pl =
	cntxt.mode <- Appl rtl;
	sc_reset ();
	(* translate declarations and body *)
	def_add Eqn Sc.tau FF;
	List.iter2
		( fun ro p ->
					p (new_env (ro2cid ro) ro) Sc.alpha Sc.beta Sc.tau [] Sc.alpha; ()
		) rtl pl;
	let (eqns, mems, dbgs) = check_causality_and_reduce () in
	(* let wires = reduce_wires eqns mems dbgs in
	pn(); ps "Number of rewired Wires: "; pi wires; *)
	let filter_sgns l d =
		match d.sc_act with
		| None -> d.sc_sgn:: l
		| Some _ -> l in
	let filter_sigs l ro =
		l@( List.filter is_principal_signal (ro2sigs ro)) in
	let sigs = List.fold_left filter_sigs [] rtl
	and wires = List.fold_left filter_sgns [] eqns
	and regs = List.fold_left filter_sgns [] mems in
	let observables =
		if se.target_sys = Simulink || se.target_sys = Scicos
		then split_eqns_for_matlab eqns sigs
		else [] in
	(*final_defs (List.rev eqns) [][];*)
	{ sa_sigs = sigs;
		sa_obsv = observables;
		sa_alpha = Sc.alpha;
		sa_beta = Sc.beta;
		sa_dcls = !r.dcls;
		sa_wires = wires;
		sa_regs = regs;
		sa_eqns = eqns;
		sa_mems = mems;
		sa_axms = !r.axms;
		sa_prps = !r.prps;
		sa_dbgs = dbgs;
	}
