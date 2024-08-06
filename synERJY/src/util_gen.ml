open Ast
open Util
open P
open Util_print
open Gen_application

(* -------------------------------------------------------------------------
TYPES FOR DATA STRUCTURES CODE GENERATION

''tcc_context'' is the type of a data structure for code generation.
There exist ONE OBJECT ''cc'' of this type. It is IMPLICIT PARAMETER of
all functions of this file.
Further the object ''ac'' from the application generator is needed. See
for details in ''gen_application.ml''. Most data of ''cc'' and ''ac'' is
copied data, cached for efficiency. --> BE CAREFUL <--
''texpr_ctxt'' is the type for parameters of functions generating expr - code.
------------------------------------------------------------------------- *)
type texpr_ctxt =
	{ at_name : string;
		at_type : ttype;
		at_exp : bool;
	}

let mk_ctxt name typ =
	{ at_name = name;
		at_type = typ;
		at_exp = false;
	}

type tcc_context =
	{ (* cache to ''optimize'' dynamic binding: *)
		mutable cc_dynbind : ((ttype * tmfid * int), (ttype * (ttype list)) list)
		Hashtbl.t;
		(* constant data for a class to be generated *)
		mutable cc_typ : ttype;
		mutable cc_this_ctxt : texpr_ctxt;
		mutable cc_v2t : (Ast.tclass * Ast.ttype) list;
		(* mutable data for a class to be generated, refers to methods *)
		mutable cc_decl : tdecl_entry;
		(* has post conditions, thus needs return var and return label *)
		mutable cc_has_rtn_lbl : bool;
		(* true if references to time are made in case that the timing is 0 *)
		mutable cc_has_timing : bool;
	}

let cc =
	{ cc_dynbind = Hashtbl.create 23;
		cc_typ = Null;
		cc_this_ctxt = { at_name = "";
			at_type = Null;
			at_exp = false;
		};
		cc_v2t =[];
		cc_decl = { name = id_dont_use; origin = c_dont_use; entry = NoEntry;
			final = true; scope = Instance; volatile = false;
			parameter = false; access = Private;
			signature = { rt = Null; pt = None }
		};
		cc_has_rtn_lbl = false;
		cc_has_timing = false;
	}

(* ------------------------------------------------------------------------- *)
let this_name () = if cc.cc_this_ctxt.at_exp
	then "&("^cc.cc_this_ctxt.at_name^")"
	else cc.cc_this_ctxt.at_name

let set_cc_context typ this v2t exp =
	cc.cc_typ <- typ;
	cc.cc_this_ctxt <- {
		at_name = this;
		at_type = typ;
		at_exp = exp;
	};
	cc.cc_has_rtn_lbl <- false;
	cc.cc_v2t <- v2t

let get_supertype typ =
	let cid = type2id typ
	and v2t = prepare_subst typ in
	let ext = (i_ast cid).extends in
	subst_map ext v2t

(* -------------------------------------------------------------------------
handling of temporary variables (array new, null test, dynamic binding)
------------------------------------------------------------------------- *)
let num_tmpvar = 10 (* >=2 *)
let top_tmpvar = ref 0

let reset_tmpvar () =
	top_tmpvar := 0

let alloc_tmpvar () =
	let tmpvar = !top_tmpvar + 1 in
	if tmpvar > num_tmpvar then (
		Err.intern "alloc_tmpvar: temporary variables exhausted. Simplify \
		expressions or contact reinhard.budde@ais.fraunhofer.de"
	) else (
		top_tmpvar := tmpvar;
		tmpvar
	)

let tmpvar2int tmpvar =
	"t__"^(string_of_int tmpvar)^".i"

let tmpvar2ptr tmpvar =
	match tmpvar with
	| None -> Err.intern "tmpvar2ptr"
	| Some t -> "t__"^(string_of_int t)^".p"

let free_tmpvar tmpvar =
	if tmpvar = !top_tmpvar
	then top_tmpvar := tmpvar - 1
	else Err.intern "free_tmpvar"

let gen_tmpvar () =
	ps "union t_tmpvar t__1";
	for i = 2 to num_tmpvar do
		ps ",t__"; pi i
	done;
	ps " ;\n"

let print_arity dcl =
	let pl = some dcl.signature.pt "print_arity" in
	let ll = List.length pl in
	pi ll

let primop2str m =
	try
		Some (List.assoc m [
				(id_op_lt, " < ");
				(id_op_le, " <= ");
				(id_op_gt, " > ");
				(id_op_ge, " >= ");
				(id_op_add, "+");
				(id_op_sub, "-");
				(id_op_mult, " * ");
				(id_op_div, "/");
				(id_op_mod, "%");
				(id_op_bit_and, " & ");
				(id_op_bit_or, " | ");
				(id_op_xor, " ^ ");
				(id_op_log_and, " && ");
				(id_op_log_or, " || ");
				(id_op_plus, "+");   (* unary *)
				(id_op_minus, "-");   (* unary *)
				(id_op_complement, "~");   (* unary *)
				(id_op_leftshift, "<<");
				(id_op_rightshift, ">>");
				(id_op_rightshift0,">>");
				]
		)
	with _ -> None

let mk_index cid upperbound index =
	match index.expr with
	| Slice r ->
			let l =
				match r.lower.expr with
				| Literal(_, l, _) -> s2i l
				| _ -> Err.intern "mk_index"
			and u = match r.upper.expr with
				| Literal(_, l, _) -> s2i l
				| _ -> Err.intern "mk_index"
			in
			if 0 <= l
			then () (* ok *)
			else Err.msg(Err.Index(cid, r.lower.elbl));
			if u <= upperbound
			then () (* ok *)
			else Err.msg(Err.Index(cid, r.upper.elbl));
			(l, u)
	| Literal(_, l, _) ->
			let l = s2i l in (l, l)
	| _ -> Err.intern "mk_index"

(* -------------------------------------------------------------------------
is_basic_type_equal ... true if both simple and equal
------------------------------------------------------------------------- *)
let is_basic_type_equal t1 t2 =
	match (t1, t2) with
	| Simple c1, Simple c2 ->
			c1 = c2 || is_literal_type t1 || is_literal_type t2
	| _ ->
			false

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

(* -------------------------------------------------------------------------
auxiliary functions
------------------------------------------------------------------------- *)
let ll2str ll = lst_sep2str Ly.lbl2str ":" ll
let apl2str apl = "_"^(i2s (List.length apl))

let is_static dcl =
	dcl.scope = Class || dcl.scope = Single

(* -------------------------------------------------------------------------
code generation for matrix expressions
------------------------------------------------------------------------- *)
let cc_mk_e t e = { etyp = Some t; expr = e; elbl = Ly.nolbl }

let cc_mk_var i = cc_mk_e t_int (Var i)

let cc_mk_offset index exp =
	match exp.expr with
	| Slice r ->
			( match r.lower.expr with
				| Literal (_,"0", _) ->
						cc_mk_var index
				| _ ->
						cc_mk_e t_int (Offset(cc_mk_var index, r.lower))
			)
	| _ -> exp

let cc_mk_get exp ind =
	match exp.etyp, ind with
	| Some (Array(t, Arbitrary, _)), _
	| Some (Array(t, _, Arbitrary)), _ ->
			exp
	| Some (Array(t, _, _)), _ ->
			let apl = List.map cc_mk_var ind in
			cc_mk_e t (Dot(exp, cc_mk_e t (Call(id_op_get, Some apl))))
	| _ -> exp

let rec cc_call_add_get ind subst exp =
	match exp.expr with
	
	| Dot(_,{ expr = Call(op, Some []) })
	when op = id_op_diagonal ->
			let exp = cc_mk_get exp ind in
			cc_call_subst subst exp
	
	| Dot(e1,{ expr = Call(op, Some []) }) when op = id_op_transp ->
			let ind, subst' =
				match ind with
				| [] -> [],[]
				| [_] -> ind, subst
				| [i; j] ->
						[0; 1],[0, cc_mk_var j; 1, cc_mk_var i]
				| _ -> Err.intern "cc_call_add_get:transp"
			in
			let e1 = cc_call_add_get ind subst' e1 in
			cc_call_subst subst e1
	
	| Dot(e1,{ expr = Call(op, Some apl) }) when op = id_op_get ->
			let ind, subst' =
				match ind, apl with
				| [],[] ->
						[],[]
				| [i],[ap] ->
						[0],[0, cc_mk_offset i ap]
				| [i],[ap1; ap2] ->
						[0; 1],[0, cc_mk_offset i ap1; 1, cc_mk_offset i ap2]
				| [i; j],[ap1; ap2] ->
						[0; 1],[0, cc_mk_offset i ap1; 1, cc_mk_offset j ap2]
				| _ -> Err.intern "cc_call_add_get:slice"
			in
			let e1 = cc_call_add_get ind subst' e1 in
			cc_call_subst subst e1
	
	| Dot(e1, e2) ->
			{ exp with expr =
					Dot(cc_call_add_get ind subst e1, cc_call_add_get ind subst e2) }
	
	| Call(mf, None) ->
			let exp = cc_mk_get exp ind in
			cc_call_subst subst exp
	
	| Call(op, Some apl) ->
			{ exp with expr =
					Call(op, Some (List.map (cc_call_add_get ind subst) apl)) }
	
	| ClassCall(cid, mf, None) ->
			exp
	
	| ClassCall(cid, mf, Some apl) ->
			{ exp with expr =
					ClassCall(cid, mf, Some (List.map (cc_call_add_get ind subst) apl)) }
	
	| New(typ, apl) ->
			{ exp with expr = New(typ, List.map (cc_call_add_get ind subst) apl) }
	
	| Super(fm, apl) ->
			{ exp with expr = Super(fm, List.map (cc_call_add_get ind subst) apl) }
	
	| Cast(to_typ, e) ->
			{ exp with expr = Cast(to_typ, cc_call_add_get ind subst e) }
	
	| This -> exp
	
	| ThisConstr(apl) ->
			{ exp with expr = ThisConstr(List.map (cc_call_add_get ind subst) apl) }
	
	| SuperConstr(apl) ->
			{ exp with expr = SuperConstr(List.map (cc_call_add_get ind subst) apl) }
	
	| If(tl) ->
			let rec theni =
				function
				| [] -> []
				| [Else(lbl1,[ExprStmt(lbl2, s)])] ->
						[Else(lbl1,[ExprStmt(lbl2, cc_call_add_get ind subst s)])]
				| (Then(lbl1, e,[ExprStmt(lbl2, s)])):: tl ->
						Then(lbl1, cc_call_add_get ind subst e,
							[ExprStmt(lbl2, cc_call_add_get ind subst s)]):: (theni tl)
				| _ -> Err.intern "cc_call_add_get:then"
			in
			{ exp with expr = If(theni tl) }
	
	| NullObj
	| IncrDecr _
	| Assign _
	| Literal _ ->
			exp
	
	| ArrayLiteral (Dim1 a) ->
			( match ind with
				| [_] ->
						let exp = cc_mk_get exp ind in
						cc_call_subst subst exp
				| _ -> Err.intern "cc_call_add_get:arrayliteral1"
			)
	
	| ArrayLiteral (Dim2 a) ->
			( match ind with
				| [_; _] ->
						let exp = cc_mk_get exp ind in
						cc_call_subst subst exp
				| _ -> Err.intern "cc_call_add_get:arrayliteral2"
			)
	
	| DeltaT
	| Instant
	| Present _
	| Timestamp _ -> exp
	
	| SigVal _
	| Value _
	| DotDot _
	| Offset _ ->
			let exp = cc_mk_get exp ind in
			cc_call_subst subst exp
	| Slice _
	| Var _ -> Err.intern "cc_call_add_get:SliceOrVar"

and cc_call_subst subst exp =
	match exp.expr with
	| Dot(e1, e2) ->
			{ exp with expr = Dot(cc_call_subst subst e1, cc_call_subst subst e2) }
	
	| Call(mf, None) ->
			exp
	
	| Call(op, Some apl) ->
			{ exp with expr = Call(op, Some (List.map (cc_call_subst subst) apl)) }
	
	| ClassCall(cid, mf, None) ->
			exp
	
	| ClassCall(cid, mf, Some apl) ->
			{ exp with expr =
					ClassCall(cid, mf, Some (List.map (cc_call_subst subst) apl)) }
	
	| New(typ, apl) ->
			{ exp with expr = New(typ, List.map (cc_call_subst subst) apl) }
	
	| Super(fm, apl) ->
			{ exp with expr = Super(fm, List.map (cc_call_subst subst) apl) }
	
	| Cast(to_typ, e) ->
			{ exp with expr = Cast(to_typ, cc_call_subst subst e) }
	
	| This ->
			exp
	
	| ThisConstr(apl) ->
			{ exp with expr = ThisConstr(List.map (cc_call_subst subst) apl) }
	
	| SuperConstr(apl) ->
			{ exp with expr = SuperConstr(List.map (cc_call_subst subst) apl) }
	
	| If(tl) ->
			let rec theni =
				function
				| [] -> []
				| [Else(lbl1,[ExprStmt(lbl2, s)])] ->
						[Else(lbl1,[ExprStmt(lbl2, cc_call_subst subst s)])]
				| (Then(lbl1, e,[ExprStmt(lbl2, s)])):: tl ->
						Then(lbl1, cc_call_subst subst e,
							[ExprStmt(lbl2, cc_call_subst subst s)]):: (theni tl)
				| _ -> Err.intern "cc_call_subst:then"
			in
			{ exp with expr = If(theni tl) }
	
	| NullObj
	| IncrDecr _
	| Assign _
	| Literal _
	| ArrayLiteral _
	| DeltaT
	| Instant
	| Present _
	| Timestamp _
	| SigVal _
	| Value _
	| DotDot _ -> exp
	| Slice _ -> Err.intern "cc_call_subst:Slice"
	| Offset(e1, e2) ->
			{ exp with expr = Offset(cc_call_subst subst e1, cc_call_subst subst e2) }
	| Var i -> (List.assoc i subst)


