open Ly
open Util
open Ast
open P
open Util_print

(* -------------------------------------------------------------------------
"ttc_context" is the type of a data structure for typechecking.
There exist ONE OBJECT "tc" of this type. It is IMPLICIT PARAMETER of
all functions of file type_check.ml. The object contains only valid data
during type - checking of ONE CLASS. Most data of "tc" is copied data,
cached for efficiency. --> BE CAREFUL <--

''tc_symtab'' is the hashtable for class - wide known entities.
- initially empty
- then fields are type - checked. This includes typechecking the init - expr.
- each field is entered into the table after typecheck.
- it is thus available for init of those fields, which follow later.
- accessing methods or superclass entities is explicitly forbidden,
because it violates clarity of code. Use the constructor for that ...
- when methods and constructors are typechecked, of course no such
init sequence is enforced.

''tc_fl'' is a hash - table containing the declarations of formal and local
parameters. The table is loaded when a method starts to be typechecked,
and modified by a LetStmt.
------------------------------------------------------------------------- *)
type ttc_context =
	{
		(* constant data, set for a class to typecheck *)
		mutable tc_cid_to_tc : tclass;
		mutable tc_outer_cid : tclass; (* context class, if ANONYMOUS *)
		mutable tc_typ_to_tc : ttype;
		(* accumulated data for a class to typecheck *)
		mutable tc_symtab : tdecl_table;
		mutable tc_arraylits : tal list;
		mutable tc_arraylit_ct : int;
		mutable tc_sig_maps : tbsexpr list ;
		mutable tc_instmthd : (tmfid * ttype) list ;
		mutable tc_classmthd : (tmfid * ttype) list ;
		mutable tc_new : ttype list ;
		mutable tc_callgraph : (tmfid * tmfid) list ;
		(* data changed for each method or field to be typechecked *)
		mutable tc_newflag : bool ;
		(* if true: new used to define a field in an assignment *)
		mutable tc_dyn_new : ttype list ;
		(* list of types which have dynamically generated objects *)
		mutable tc_decl : tdecl_entry ;
		mutable tc_reactive : bool ;
		mutable tc_signalbus : bool ;
		mutable tc_flowcontext : bool ;
		mutable tc_lbl : tlbl ;
	}

let empty_default_decl name =
	{
		name = name; origin = c_dont_use; entry = NoEntry; final = true;
		scope = Instance; volatile = false; parameter = false;
		signature ={ rt = Null; pt = None }; access = Private
	}

let tc =
	{
		tc_cid_to_tc = c_dont_use; tc_outer_cid = c_dont_use; tc_typ_to_tc = Null;
		tc_symtab = Hashtbl.create 23;
		tc_arraylits =[]; tc_arraylit_ct = 1; tc_sig_maps =[];
		tc_instmthd =[]; tc_classmthd =[];
		tc_newflag = false; tc_dyn_new =[]; tc_new =[]; tc_callgraph =[];
		tc_decl = empty_default_decl id_dont_use;
		tc_reactive = false; tc_signalbus = false;
		tc_flowcontext = false; tc_lbl = Ly.nolbl;
	}

let tc_fl = (Hashtbl.create 23 : (tmfid, tdecl_entry) Hashtbl.t)

(* -------------------------------------------------------------------------
error reporting for type - checking
------------------------------------------------------------------------- *)
let class_ra () =
	"class "^
	(c2str tc.tc_cid_to_tc)^
	" in method/field "^
	(mf2str tc.tc_decl.name)^
	Err.cid_lbl2str tc.tc_cid_to_tc tc.tc_lbl

let type_err p =
	let cf = class_ra ()
	and p' = if p = "" then "" else (". \n"^p) in
	Err.msg (Err.Typing(cf^p'))

let err_literal_type_not_unique () =
	type_err "The type inferencer cannot compute a unique type \
for a manifest literal \
(e.g. the literal may be legally both short and int. \
Cast the literal to a type like, e.g., \"(int)0\").\n"

let unknown_id typ mfid len =
	if is_literal_type typ then (
		err_literal_type_not_unique ()
	) else (
		Err.msg (Err.UnknownId(type2id typ, mfid, len, class_ra()))
	)

let mustbe_nulltype typ =
	if typ != Null then
		type_err ("This statement should return type Null, but returns "^Err.type2str typ)

let set_lbl lbl = tc.tc_lbl <- lbl

(* -------------------------------------------------------------------------
incr_array_lit[12] generate a unique name for an array literal
------------------------------------------------------------------------- *)
let incr_array_lit1 at l =
	let n = tc.tc_arraylit_ct in
	l.an1 <- n;
	tc.tc_arraylit_ct <- n + 1;
	tc.tc_arraylits <- (Dim1 l):: tc.tc_arraylits

let incr_array_lit2 at l =
	let n = tc.tc_arraylit_ct in
	l.an2 <- n;
	tc.tc_arraylit_ct <- n + 1;
	tc.tc_arraylits <- (Dim2 l):: tc.tc_arraylits

(* =========================================================================
FUNCTIONS FOR TYPECHECKING
========================================================================= *)
let mk_e e lbl = { etyp = None; expr = e; elbl = lbl }
let mk_edot e1 e2 lbl = mk_e (Dot(e1, e2)) lbl
let mk_ecall n p lbl = mk_e (Call (n, p)) lbl
let mk_e_inf n e1 e2 lbl = mk_edot e1 (mk_ecall n (Some [e2]) lbl) lbl

let mk_ecall n p lbl = { etyp = None; expr = (Call (n, p)); elbl = lbl }

(* -------------------------------------------------------------------------
utility functions for typevars typechecking
------------------------------------------------------------------------- *)
let rec leconstr tvar acc = function
	| [] -> acc
	| (TypLE c):: t ->
			( match acc with
				| None ->
						Some c
				| Some x ->
						if le_type ~sub: c ~super: x then
							leconstr tvar (Some c) t
						else if le_type ~sub: x ~super: c then
							leconstr tvar (Some x) t
						else
							type_err ("Invalid constraints for \
									type parameter "^(c2str tvar))
			)
	| _:: t -> leconstr tvar acc t

and eqconstr tvar acc = function
	| [] -> acc
	| (TypEQ c):: t ->
			( match acc with
				| None -> Some c
				| Some x -> if c = x
						then eqconstr tvar acc t
						else type_err ("Invalid actual type for \
									type parameter "^(c2str tvar))
			)
	| _:: t -> eqconstr tvar acc t

and typevar2constraint tvar constraintsl =
	try
		let constraints = get_map tvar constraintsl in
		let eq = eqconstr tvar None constraints in
		let le = leconstr tvar None constraints in
		match eq, le with
		| None, None ->
				TypeVar tvar
		| Some eq, Some le ->
				if le_type ~sub: eq ~super: le
				then eq
				else type_err ("Conflicting constraint and actual type \
							detected for type parameter "^(c2str tvar))
		| None, Some le ->
				le
		| Some eq, None ->
				eq
	with Not_found ->
			TypeVar tvar

and typevarl2constraint ast =
	let cstrl = a_typ_constrl ast in
	List.map (fun (tvar, _) -> typevar2constraint tvar cstrl) ast.typparams

(* -------------------------------------------------------------------------
tc_field_accessed_at ... returns a description of the context in which a
field access happens. The last rule (the catch - all) handles the
special case of accessing fields in invariants which are considered as
code accesses
------------------------------------------------------------------------- *)
let tc_field_accessed_at isnew e =
	match tc.tc_decl.entry with
	| Field _ ->
			if isnew
			then (NewInit e)
			else (AtInit e)
	| SigDecl _ ->
			if isnew
			then (NewInit e)
			else type_err "Invalid initialization for a signal \
	declaration"
	| Method _ ->
			(AtCode e)
	| Constructor _ ->
			if isnew
			then (NewCons e)
			else (AtCons e)
	| _ -> (AtCode e)

let tc_add_assign dcl expr =
	let isnew = match expr.expr with
		| New(_, _) -> true
		| ArrayLiteral(_) -> true
		| _ -> false in
	( match dcl.entry with
		| Field f ->
				( match tc.tc_decl.entry with
					| Method m ->
							m.mwrite <- add_sosgl dcl m.mwrite
					| Constructor c ->
							c.cwrite <- add_sosgl dcl c.cwrite
					| _ -> ()
				);
				let a = tc_field_accessed_at isnew expr in
				f.assigns <- a:: f.assigns
		| SigDecl s ->
				let err () = type_err "Signals must be initialized \
				in its declaration or in a constructor \
				tail, either with a new object or \
				a formal constructor parameter"
				and a = tc_field_accessed_at isnew expr in
				s.sig_assigns <- a:: s.sig_assigns;
				( match expr.expr with
					| New(_, _) ->
							()
					| Call(f, None) ->
							( try
								let dcl = Hashtbl.find tc_fl f in
								if dcl.entry = ConstLocal
								then ()
								else err ()
							with Not_found -> err ()
							)
					| _ -> err ()
				)
		| MutableLocal -> ()
		| _ -> Err.msg (Err.Lhs3Err(class_ra (), dcl.name))
	)

(* -------------------------------------------------------------------------
used to set the status of a type declaration to determine whether
a class is a singleton class. Singleton classes are generated as expanded
classes. See also gen_application.ml

The idea is to check whether more than one object is generated for a class.
Generation is due to
- a New statement, then cause
- a downward cast
- an upward cast

------------------------------------------------------------------------- *)
let set_status cid fm =
	let ast = i_ast cid in
	match ast.status with
	| Multiple -> ()
	| NotTouched -> ast.status <- Singleton (cid, fm)
	| Singleton _ -> ast.status <- Multiple

(* -------------------------------------------------------------------------
get the declaration of a method, constructor or field
------------------------------------------------------------------------- *)
type tlhs =
	| Lhs
	| Rhs

module Decl

(* begin signature: : sig

val get : tlhs -> Ly.tmfid -> int -> Ast.tdecl_entry
val get_signaltyp : Ly.tmfid -> Ast.ttype
val class_scope : Ast.ttype -> Ly.tmfid -> int -> bool
val signature : Ast.ttype -> Ly.tmfid -> int -> Ast.tsignature
val subst : Ast.ttype -> Ly.tmfid -> Ast.tsignature -> Ast.tsignature
val subst_typ : Ast.ttype -> Ly.tmfid -> Ast.ttype -> Ast.ttype
end
end signature *)

=
struct
	
	let rec tc_add_constraint (ctl : ttyp_constrl) = function
		| [],[] ->
				ctl
		| (v, _):: vl, t:: tl ->
				let ctl =
					if t = Any || is_typevar t
					then ctl
					else add_maplist (v,[TypEQ t]) ctl
				in
				tc_add_constraint ctl (vl, tl)
		| _ -> raise (Invalid_argument "")
	
	let rec tc_subst_typ_var typ dim constrl =
		let typ =
			match typ with
			| TypeVar(n) -> typevar2constraint n constrl
			| t -> t in
		match typ with
		| Typ(c, tl) ->
				Typ(c, List.map (fun t -> tc_subst_typ_var t None constrl) tl)
		| Array(t, d1, d2) ->
				( match d1, d2, dim with
					| DimLen 1, DimVar y, Some(DimLen 1, d2) when y = id_col ->
							Array(tc_subst_typ_var t None constrl, DimLen 1, d2)
					| DimVar x, DimLen 1, Some(DimLen 1, d2) when x = id_col ->
							Array(tc_subst_typ_var t None constrl, d2, DimLen 1)
					| DimVar x, DimVar y, Some(d1, d2) when x = id_row && y = id_col ->
							Array(tc_subst_typ_var t None constrl, d1, d2)
					| DimVar x, DimVar y, Some(d1, d2) when x = id_col && y = id_row ->
							Array(tc_subst_typ_var t None constrl, d2, d1)
					| _ -> typ
				)
		| _ -> typ
	
	(* bit of a hack: Assumption is that in builtin.ml the Matrix Types
	have dimension parameters "id_col" and "id_row". Do not change this
	without changing the corresponding clauses in builtin.ml.
	*)
	
	let tv_err mf = type_err ("Type parameter list error detected when "^
				(mf2str mf)^" was checked.\n")
	
	let subst lookup_typ errmsg_mfid sign =      (* formal -> actual typ params  *)
		let lookup_cid = type2id lookup_typ in (* get infos about lookup class *)
		let lookup_ast = i_ast lookup_cid in
		let lookup_ftl = lookup_ast.typparams in
		let lookup_atl = type2tpl lookup_typ in
		if lookup_ftl = [] then (
			if lookup_atl = []
			then { rt = sign.rt; pt = sign.pt }
			else tv_err errmsg_mfid
		) else (
			let my_ctl = i_typ_constrl tc.tc_cid_to_tc in
			let lookup_atl = List.map
					(fun t -> tc_subst_typ_var t None my_ctl)
					lookup_atl in
			let lookup_ctl = a_typ_constrl lookup_ast in
			try
				let lookup_ctl = tc_add_constraint
						lookup_ctl
						(lookup_ftl, lookup_atl) in
				let lookup_subst t =
					match lookup_typ with
					| Array(_, d1, d2) ->
							tc_subst_typ_var t (Some(d1, d2)) lookup_ctl
					| _ ->
							tc_subst_typ_var t None lookup_ctl
				in
				{ rt = lookup_subst sign.rt;
					pt = match sign.pt with
						| None -> None
						| Some apl -> Some (List.map lookup_subst apl)
				}
			with Invalid_argument _ ->
					tv_err errmsg_mfid
		)
	
	let subst_typ lookup_typ errmsg_mfid typ =
		let sgn = subst lookup_typ errmsg_mfid { rt = typ; pt = None } in
		sgn.rt
	
	let rec tc_entry cid mfid len =
		if cid = tc.tc_cid_to_tc then (
			try
				match Hashtbl.find tc.tc_symtab (mfid, len) with
				| entry:: _ -> entry
				| _ -> Err.intern "tc_entry:st"
			with Not_found ->
					if tc.tc_cid_to_tc = tc.tc_outer_cid then ( (* no anonymous class *)
						raise Not_found
					) else ( (* anonymous class *)
						if (mfid = id_p1) && (len = - 1) then (
							i_entry cid mfid len (* anonymous class *)
						) else (
							raise Not_found
						)
					)
		) else (
			i_entry cid mfid len
		)
	
	let bs_get_decl typ mfid len =
		let cid = type2id typ in
		let dcl = tc_entry cid mfid len in
		let access = dcl.access in
		(* OK: class scope may only use class scope *)
		if tc.tc_decl.scope = Class then (
			if dcl.scope = Class then (
			(* static -> static OK *)
			) else if is_constructor dcl then (
			(* static -> new OK *)
			) else if tc.tc_cid_to_tc != dcl.origin then (
			(* using some other class OK *)
			) else (
				(* XXX too restrictive, if locals are used in static methods *)
				Err.msg (Err.Lhs0Err(class_ra (), mfid))
			)
		) else (
		(* instance -> everything OK *)
		);
		(* OK: if 1. inside a class OR 2. access is granted *)
		if tc.tc_outer_cid = dcl.origin ||
		tc.tc_cid_to_tc = dcl.origin ||
		is_signal tc.tc_decl
		then dcl
		else if is_access_allowed ~forcid: tc.tc_cid_to_tc ~tocid: cid access
		then dcl
		else Err.msg (Err.Access(class_ra(), mfid))
	
	let class_scope typ mfid len =
		try
			let dcl = bs_get_decl typ mfid len in
			dcl.scope = Class
		with Not_found ->
				unknown_id typ mfid len
	
	let rec bs_get_without_locals typ mfid len =
		let dcl = bs_get_decl typ mfid len in
		if (type2tpl typ) = []
		then dcl
		else { dcl with signature = subst typ mfid dcl.signature }
	
	let get a_lhs mfid len =
		let typ = tc.tc_typ_to_tc in
		let dcl =
			try (* try class and superclasses *)
				let dcl = bs_get_decl typ mfid len in
				if (a_lhs = Lhs) && not( is_field dcl || is_signal dcl )
				then Err.msg (Err.Lhs1Err(class_ra (), mfid))
				else if a_lhs = Lhs &&
				dcl.origin != tc.tc_outer_cid &&
				dcl.origin != tc.tc_cid_to_tc
				then Err.msg (Err.Lhs2Err(class_ra (), mfid))
				else if (a_lhs = Lhs) && dcl.final &&
				not (is_field tc.tc_decl) &&
				not( is_constructor tc.tc_decl )
				then Err.msg (Err.Lhs3Err(class_ra (), mfid))
				else dcl
			with Not_found -> (* now try formal and local parameter *)
					try
						let dcl = Hashtbl.find tc_fl mfid in
						if a_lhs = Lhs && dcl.entry = ConstLocal
						then Err.msg (Err.Lhs1Err(class_ra (), mfid))
						else if is_constructor tc.tc_decl && tc.tc_reactive
						&& not(is_sensor_or_signal_typ dcl.signature.rt)
						then type_err "In the active part of a constructor the use \
						of formal parameters is restricted to \
						sensor or signal \
						types.\n Workaround: copy formal parameters \
						of any other type to an instance field. \
						If you are severely hit by this, please mail \
						reinhard.budde@ais.fraunhofer.de or \
						axel.poigne@ais.fraunhofer.de"
						else dcl
					with Not_found ->
							unknown_id typ mfid len
		in
		if (type2tpl typ) = []
		then dcl
		else { dcl with signature = subst typ mfid dcl.signature }
	
	let bs_chk_dcl dcl =
		if (not tc.tc_reactive) && (is_reactive dcl)
		then type_err "Calling reactive methods from data methods is illegal"
		else ();
		if is_field dcl && not( is_field tc.tc_decl || is_signal tc.tc_decl) then (
			( match tc.tc_decl.entry with
				| Method m -> m.mread <- add_sosgl dcl m.mread
				| Constructor c -> c.cread <- add_sosgl dcl c.cread
				| _ -> ()
			);
			match dcl.entry with
			| Field f -> let v = { etyp = Some Null; expr = NullObj; elbl = Ly.nolbl } in
					let r = tc_field_accessed_at false v in
					f.reads <- r:: f.reads
			| _ -> ()
		) else if (is_method_or_constructor dcl) then (
			match tc.tc_decl.entry with
			| Method m -> m.mcall <- add_sosgl dcl m.mcall
			| Constructor c -> c.ccall <- add_sosgl dcl c.ccall
			| _ -> ()
		) else (
		)
	
	let decl_entry typ fm len =
		if typ = Null
		then type_err "The null type has no accessible methods or fields"
		else ();
		let cid = type2id typ in
		if (tc.tc_cid_to_tc = cid) && (len = - 1) then (
			let dcl = get Rhs fm len in
			bs_chk_dcl dcl;
			dcl
		) else (
			try
				let dcl = bs_get_without_locals typ fm len in
				bs_chk_dcl dcl;
				dcl
			with Not_found ->
					unknown_id typ fm len
		)
	
	let signature typ fm len =
		(decl_entry typ fm len).signature
	
end

(* -------------------------------------------------------------------------
manage the local declaration in the formal / local symbol - table.
cleared, when typechecking of a method starts.
modified when either a ''private''- declaration (LetStmt) is found
or a ''switch''- stmt is found
------------------------------------------------------------------------- *)
let rm_local_param name =
	Hashtbl.remove tc_fl name

(* -------------------------------------------------------------------------
type - check ...

type checking depend on the context:
- in a flow context (tc_flowcontext = true)
all data are considered as flows, otherwise
- hence we can replace flow, sensor, and signal types by their respective
value types
this is done in the rule for field access and method access
(case Call(fm, None)), Call(fm, Some apl) )
- there is one exception for node calls:
though within a flow context, the actual parameter should be
of sensor or signal type if the formal parameters are
in this particular case the tc_flowcontext flag is toggled
(for method access Call(fm, Some apl)
------------------------------------------------------------------------- *)
let tc_vector_tmpl t l u =
	if l = u
	then t
	else Array(t, DimLen 1, DimLen (u - l + 1))

let tc_matrix_tmpl t l1 u1 l2 u2 =
	if l1 = u1 && l2 = u2
	then t
	else
	if l1 = u1
	then
		Array(t, DimLen 1, DimLen (u2 - l2 + 1))
	else
	if l2 = u2
	then Array(t, DimLen(u1 - l1 + 1), DimLen 1)
	else Array(t, DimLen(u1 - l1 + 1), DimLen(u2 - l2 + 1))

let rec tc_expr type_to_derive expr_to_check =
	let type_inferred = tc_infer tc.tc_typ_to_tc expr_to_check in
	if le_type ~sub: type_inferred ~super: type_to_derive then (
	(* ok *)
	) else (
		Err.msg (Err.Subtyping(class_ra(), type_inferred, type_to_derive))
	)

(* -------------------------------------------------------------------------
infer a type for an expression.
furthermore: save the inferred type in the expr - record, and,
return the inferred type
------------------------------------------------------------------------- *)
and bs_infer lookup_typ full_expr expr =
	set_lbl full_expr.elbl;
	match expr with
	| Dot({ expr = Call(s, None) },{ expr = Call(fm, Some []) })
	when fm = id_op_present || fm = id_op_timestamp || fm = id_op_value
	|| fm = id_op_dotdot ->
	(* present, timestamp or value or buffer access for a sensor or signal *)
			if is_typevar lookup_typ then (
				type_err ("Unconstrained type variable "^Err.type2str lookup_typ^
						" used for a field access.\n")
			) else (
				let t = (Decl.signature lookup_typ s (- 1)).rt in
				tc.tc_instmthd <- add_sosgl (s, lookup_typ) tc.tc_instmthd;
				if is_sensor_or_signal_typ t then (
					if fm = id_op_present then (
						t_bool
					) else if fm = id_op_timestamp then (
						t_time
					) else if fm = id_op_value then (
						match sigtyp2valtyp t with
						| Some t -> if t = Null
								then type_err "Invalid to access the value of a \
								pure signal"
								else t
						| None ->
								Err.intern "bs_infer:value1"
					) else if fm = id_op_dotdot then (
						match sigtyp2valtyp t with
						| Some t ->
								if t = Null then (
									type_err "Invalid to access the value \
									of a pure signal"
								) else (
									if is_double_or_float_typ t then (
										Array(t, DimLen 1, DimRef (ref (ref Arbitrary)))
									) else (
										type_err "buffered values are neither of double \
										nor of float type.\n"
									)
								)
						| None ->
								Err.intern "bs_infer:value1"
					) else (
						type_err "Invalid method for a sensor or signal or sensor"
					)
				) else (
					type_err "The operators ?, @, $, and .. are only valid \
					for sensors and signals"
				)
			)
	
	| Dot({ expr = DeltaT },{ expr = Call(fm, Some []) }) when fm = id_op_value ->
			t_double
	
	| Dot(c1,{ expr = Call(fm, Some []) }) when fm = id_op_diagonal || fm = id_op_all ->
			let t = tc_infer lookup_typ c1 in
			if tc.tc_flowcontext
			then ()
			else type_err "Diagonal matrix used outside of flow context";
			if is_double_or_float_typ t
			then ()
			else type_err "Diagonal operator applied to a value that is \
			neither of type float nor of type double";
			Array(t, DimRef (ref (ref Arbitrary)), DimRef (ref (ref Arbitrary)))
	
	| Dot(c1, c2) ->
			let t = tc_infer lookup_typ c1 in
			let t = tc_infer t c2 in
			set_lbl full_expr.elbl;
			t
	| Call(fm, None) -> (* access to an field *)
			if is_typevar lookup_typ then (
				type_err ("Unconstrained type variable "^Err.type2str lookup_typ^
						" used for a field access.\n")
			) else (
				let t = (Decl.signature lookup_typ fm (- 1)).rt in
				tc.tc_instmthd <- add_sosgl (fm, lookup_typ) tc.tc_instmthd;
				if is_reference_typ t then (
					set_status (type2id lookup_typ) fm
				);
				if is_sensor_or_signal_typ t
				then if tc.tc_flowcontext
					then match sigtyp2valtyp t with
						| Some t -> t
						| None -> type_err "Illegal use of a pure \
					sensor or signal in a \
					flow context {| ... |} "
					else t
				else t
			)
	| Call(fm, Some []) when fm = id_op_pre ->
			if tc.tc_flowcontext
			then ()
			else Err.msg (Err.FlowOutside (class_ra()));
			lookup_typ
	| Call(fm, Some []) when fm = id_op_upspl ->
			if tc.tc_flowcontext
			then ()
			else Err.msg (Err.FlowOutside (class_ra()));
			lookup_typ
	| Call(fm, Some [p]) when fm = id_op_equal || fm = id_op_not_equal ->
			if is_primitive_typ lookup_typ then (
				tc_expr lookup_typ p
			) else if is_sensor_or_signal_typ lookup_typ
			&& not( tc.tc_flowcontext ) then (
				Err.msg (Err.FlowOutside (class_ra()))
			) else if is_typevar lookup_typ then (
				tc_expr lookup_typ p
			) else if p.expr = NullObj then (
			(* is OK *)
			) else (
				tc_expr t_object p
			);
			t_bool
	| Call(fm, Some [p]) when fm = id_op_fby ->
			if tc.tc_flowcontext
			then ()
			else Err.msg (Err.FlowOutside (class_ra()));
			let ti = tc_infer tc.tc_typ_to_tc p in
			( match most_general_type [lookup_typ; ti] with
				| Null -> Err.msg (Err.SuperType (class_ra(), "In a flow equation \
									with \"->\" no suitable type could be inferred",
									[lookup_typ; ti]))
				| t -> t
			)
	| Call(fm, Some [p]) when fm = id_op_downspl ->
			if tc.tc_flowcontext
			then ()
			else Err.msg (Err.FlowOutside (class_ra()));
			tc_expr t_bool p;
			lookup_typ
	
	(* The get operator has ranges as arguments *)
	| Call(op, Some [p]) when op = id_op_get ->
			let err0() = type_err "Upper bound of a slice is greater than \
			the lower bound\n"
			and err1() = type_err "Slice exceeds size of the vector or buffer"
			and err2() = type_err "It is not possible to restrict a vector or \
			buffer by a slice if its dimension is not \
			specified by an integral value.\n"
			and err3() = type_err "Index is out of the bounds of a vector.\n"
			in
			( match p.expr with
				| Slice r ->
						tc_expr Null p;
						let l = eval_const_expr_2_int tc.tc_cid_to_tc r.lower
						and u = eval_const_expr_2_int tc.tc_cid_to_tc r.upper in
						if l <= u then () (* ok *) else err0();
						( match lookup_typ with
							| Array(t, DimLen 1, DimLen n1) ->
									if 0 <= l && u < n1 then () (* ok *) else err1();
									tc_vector_tmpl t l u
							| Array(t, DimLen 1, DimRef r) ->
									( match !(!r) with
										| DimLen n1 ->
												if 0 <= l && u < n1 then () (* ok *) else err1();
												tc_vector_tmpl t l u
										| Arbitrary ->
												!r := DimLen (u + 1);
												tc_vector_tmpl t l u
										| _ -> err2()
									)
							| Array(t, _, _) ->
									type_err "The array is two-dimensional or a matrix.\n"
							| _ -> err2()
						)
				| _ ->
						tc_expr t_int p;
						( match lookup_typ with
							| Array(t, DimLen 1, Arbitrary) ->
									t
							| Array(t, DimLen 1, _) ->
									( match try_eval_const_expr_2_int tc.tc_cid_to_tc p with
										| None -> t
										| Some i ->
												( match lookup_typ with
													| Array(t, DimLen 1, DimLen n1) ->
															if 0 <= i && i < n1 then () else err3();
															t
													| Array(t, DimLen 1, DimRef r) ->
															( match !(!r) with
																| DimLen n1 ->
																		if 0 <= i && i < n1 then () else err3();
																		t
																| _ -> t
															)
													| _ -> t
												)
									)
							| Array(t, _, _) ->
									type_err "The array is two-dimensional or a matrix.\n"
							| _ ->
									type_err "Indexing of a type that is not an array.\n"
						)
			)
	
	| Call(op, Some [p1; p2]) when op = id_op_get ->
			let err0l() = type_err "Upper bound of the first slice is \
			greater than its lower bound.\n"
			and err0u() = type_err "Upper bound of the second slice is \
			greater than its lower bound.\n"
			and err1() = type_err "Slice exceeds size of the matrix.\n"
			and err3() = type_err "Mismatch of slices with vector or \
			matrix type.\n"
			and err4() = type_err "Indeces are out of bounds of a matrix.\n"
			in
			( match p1.expr, p2.expr with
				| Slice r1, Slice r2 ->
						tc_expr Null p1;
						tc_expr Null p2;
						( match lookup_typ with
							| Array(t, DimLen n1, DimLen n2) ->
									let l1 = eval_const_expr_2_int tc.tc_cid_to_tc r1.lower
									and u1 = eval_const_expr_2_int tc.tc_cid_to_tc r1.upper
									and l2 = eval_const_expr_2_int tc.tc_cid_to_tc r2.lower
									and u2 = eval_const_expr_2_int tc.tc_cid_to_tc r2.upper in
									if l1 <= u1 then () (* ok *) else err0l();
									if l2 <= u2 then () (* ok *) else err0u();
									if 0 <= l1 && u1 < n1 then () (* ok *) else err1();
									if 0 <= l2 && u2 < n2 then () (* ok *) else err1();
									tc_matrix_tmpl t l1 u1 l2 u2
							| _ -> err3()
						)
				| Slice r1, _ ->
						tc_expr Null p1;
						tc_expr t_int p2;
						( match lookup_typ with
							| Array(t, DimLen n1, DimLen n2) ->
									let l1 = eval_const_expr_2_int tc.tc_cid_to_tc r1.lower
									and u1 = eval_const_expr_2_int tc.tc_cid_to_tc r1.upper
									and l2 = eval_const_expr_2_int tc.tc_cid_to_tc p2 in
									if l1 <= u1 then () (* ok *) else err0l();
									if 0 <= l1 && u1 < n1 then () (* ok *) else err1();
									if 0 <= l2 && l2 < n2 then () (* ok *) else err1();
									tc_vector_tmpl t l1 u1
							| _ -> err3()
						)
				| _, Slice r2 ->
						tc_expr t_int p1;
						tc_expr Null p2;
						( match lookup_typ with
							| Array(t, DimLen n1, DimLen n2) ->
									let l1 = eval_const_expr_2_int tc.tc_cid_to_tc p1
									and l2 = eval_const_expr_2_int tc.tc_cid_to_tc r2.lower
									and u2 = eval_const_expr_2_int tc.tc_cid_to_tc r2.upper in
									if l2 <= u2 then () (* ok *) else err0u();
									if 0 <= l1 && l1 < n1 then () (* ok *) else err1();
									if 0 <= l2 && u2 < n2 then () (* ok *) else err1();
									tc_vector_tmpl t l2 u2
							| _ -> err3()
						)
				| _ ->
						tc_expr t_int p1;
						tc_expr t_int p2;
						( match lookup_typ with
							| Array(t, _, Arbitrary)
							| Array(t, Arbitrary, _) ->
									t
							| Array(t, _, _) ->
									( match try_eval_const_expr_2_int tc.tc_cid_to_tc p1,
										try_eval_const_expr_2_int tc.tc_cid_to_tc p2 with
										| None, _
										| _, None ->
												t
										| Some i1, Some i2 ->
												( match lookup_typ with
													| Array(t, DimLen n1, DimLen n2) ->
															if 0 <= i1 && i1 < n1
															&& 0 <= i2 && i2 < n2 then () else err4();
															t
													| Array(t, DimLen n1, DimRef r2) ->
															( match !(!r2) with
																| DimLen n2 ->
																		if 0 <= i1 && i1 < n1
																		&& 0 <= i2 && i2 < n2 then () else err4();
																		t
																| _ -> t
															)
													| Array(t, DimRef r1, DimLen n2) ->
															( match !(!r1) with
																| DimLen n1 ->
																		if 0 <= i1 && i1 < n1
																		&& 0 <= i2 && i2 < n2 then () else err4();
																		t
																| _ -> t
															)
													| Array(t, DimRef r1, DimRef r2) ->
															( match !(!r1),!(!r2) with
																| DimLen n1, DimLen n2 ->
																		if 0 <= i1 && i1 < n1
																		&& 0 <= i2 && i2 < n2 then () else err4();
																		t
																| _ -> t
															)
													| _ -> t
												)
									)
							
							| _ -> type_err "Indexing of a type that is not an array.\n"
						)
			)
	
	(* The following is a hack to mimic overloading for the special case of
	vector and matrix operations. To be deleted when overloading is done
	properly
	The idea is that variables, vectors, and matrices share some operators
	For these we check for the most general value type, and then we look
	for the most general type with regard to primitive, vector, or matrix
	
	for multiplication one has to be particularly careful. We distinguish
	matrix product that is:
	- matrix multiplication on matrices
	- dot multiplication if one argument is a vector
	and array multiplication that is
	- point - wise multiplication on vectors and matrices
	and multiplication on primitive types
	
	The distinction is made in
	1. react2sc - where we distinguish bewtween matrix product and the others
	2. in util_gen - where we re - identify the latter.
	*)
	| Call(op, Some [p]) when is_matrix_op op ->
			let ti = tc_infer tc.tc_typ_to_tc p in
			let to_valtyp = function
				| Array(t, _, _) ->
						if tc.tc_flowcontext
						then ()
						else Err.msg (Err.FlowOutside (class_ra()));
						t
				| t -> t
			in
			let t1 = to_valtyp lookup_typ
			and t2 = to_valtyp ti in
			let err tl =
				let err = if is_array_typ lookup_typ && is_array_typ ti
					then "In a vector || matrix expression no suitable type \
					can be inferred. It may be the case that the \
					value types of vectors / matrices are not equal."
					else "No suitable type could be inferred."
				in
				Err.msg (Err.SuperType (class_ra(), err, tl))
			in
			let lt = match most_general_type [t1; t2] with
				| Null -> err [t1; t2]
				| t -> t
			in
			(* Now we have got the most general value type, and compute the
			most general result type. Since all vector operations are
			reduced to operations of primitive type within a flow context
			we have to map pointwise multiplication to ordinary
			multiplication *)
			let op' = if op = id_op_pointmult then id_op_mult else op in
			let sg = Decl.signature lt op' 1 in
			let rt = match sg.pt with
				| Some [t] ->
						if le_type ~sub: t1 ~super: t
						then sg.rt
						else err [lookup_typ; ti; lt; t]
				| _ -> Err.intern "bs_infer:bin-ops" in
			
			(*   -------------------- scalar product  --------------------------  *)
			if op = id_op_mult then (
				let err = "The inner dimensions of a vector / matrix multiplication \
				do not agree.\n"
				in
				match lookup_typ, ti with
				| Array(_, d11, d12), Array(_, d21, d22) when dim2value d11 = 1 && dim2value d21 = 1 ->
						if le_dim ~sub: d12 ~super: d22
						then rt      (* scalar product of two vectors *)
						else type_err err
				| Array(_, d11, d12), Array(_, d21, d22) ->
						if le_dim ~sub: d12 ~super: d21
						then
							if dim2value d11 = 1 && dim2value d22 = 1
							then rt
							else Array(rt, d11, d22)
						else type_err err       (* scalar product for matrix *)
				| Array(_, d1, d2), Simple _
				| Simple _, Array(_, d1, d2) ->
						Array(rt, d1, d2)
				| Simple _, Simple _ ->
						rt
				| _ ->
						Err.intern "bs_infer:id_op_mult"
				
			) else (
				(*   ------------- pointwise vector/matrix operations -------------   *)
				let err = "The dimensions do not agree.\n" in
				match ti, lookup_typ with
				| Array(_, d11, d12), Array(_, d21, d22) ->
						if le_dim ~sub: d11 ~super: d21 && le_dim ~sub: d12 ~super: d22
						then Array(rt, d11, d12)
						else type_err err
				| Array(_, d11, d12), Simple _
				| Simple _, Array(_, d11, d12) ->
						Array(rt, d11, d12)
				| Simple _, Simple _ ->
						rt
				| _ ->
						Err.intern "bs_infer:other vector ops"
			)
	(* end of the hack *)
	
	| Call(op, Some [p]) when is_primitive_typ lookup_typ ->
			let ti = tc_infer tc.tc_typ_to_tc p in
			let err tl =
				let err = "In an expression no suitable type could be \
				inferred. Often either an operand of a binary \
				method (this are operators like \"+\") is \
				illegal or an expression of primitive type \
				(e.g. of type short) has to be casted.\n" in
				Err.msg (Err.SuperType (class_ra(), err, tl))
			in
			let lt = match most_general_type [lookup_typ; ti] with
				| Null -> err [lookup_typ; ti]
				| t -> t
			in
			let sg = Decl.signature lt op 1 in
			( match sg.pt with
				| Some [t] ->
						if le_type ~sub: ti ~super: t
						then sg.rt
						else err [lookup_typ; ti; lt; t]
				| _ -> Err.intern "bs_infer:bin-ops"
			)
	
	| Call(fm, Some apl) ->
			if is_typevar lookup_typ then (
				type_err ("Unconstrained type variable "^Err.type2str lookup_typ^
						" used for a method call.\n")
			) else (
				let decl_entry = Decl.decl_entry lookup_typ fm (List.length apl) in
				let subst_sig = decl_entry.signature in
				let is_node = match decl_entry.entry with
					| Method m ->
							( match m.method_kind with
								| RctNode _ -> true
								| _ -> false
							)
					| _ -> false in
				if is_node && not tc.tc_flowcontext
				then (
					type_err ("The node "^mf2str fm^
							" is called outside of a flow context.\n")
				) else (
					try
					(* a hack for nodes: parameters must be flows
					when typechecking a node, but must be of valtype
					if typechecking a node call: would be better
					to check whether fm is a reactive node but I do not
					know how to do it *)
						let inferred_types =
							List.map
								( function
									| Typ(_,[vt]) as t when tc.tc_flowcontext &&
									is_sensor_or_signal_typ t ->
											if is_node then t else vt
									| t -> t
								) (some subst_sig.pt "bs_infer:signal")
						in
						let node_infer typ e =
							if is_sensor_or_signal_typ typ && is_node
							then (
								tc_expr (some (sigtyp2valtyp typ)"bs_infer:node") e;
							) else (
								tc_expr typ e
							)
						in
						List.iter2 (node_infer) inferred_types apl;
						tc.tc_instmthd <- add_sosgl (fm, lookup_typ) tc.tc_instmthd;
						subst_sig.rt
					with Invalid_argument(_) ->
							Err.msg (Err.LengthParamL(class_ra (), fm))
				)
			)
	
	| ClassCall(cid, fm, None) ->
			let ast = i_ast cid in
			let typ = Typ(cid,[]) in (* class calls may not have type parameter *)
			( try
				if (Decl.class_scope typ fm (- 1))
				then ()
				else type_err ((mf2str fm)^" invalid as static field.\n")
			with Not_found ->
					Err.msg (Err.Undeclared("class field", mf2str fm))
			);
			if ast.typparams = []
			then ()
			else Err.msg (Err.ClassMthdAndTypPar cid);
			(* now do the type-checking *)
			let subst_sig = Decl.signature typ fm (- 1) in
			tc.tc_classmthd <- add_sosgl (fm, typ) tc.tc_classmthd;
			subst_sig.rt
	
	| ClassCall(cid, fm, Some apl) ->
			let ast = i_ast cid in
			let typ = if cid = id_time
				then t_time
				else Typ(cid,[]) in
			( try
				if (Decl.class_scope typ fm (List.length apl))
				then ()
				else type_err ((mf2str fm)^" invalid as static method.\n")
			with Not_found ->
					Err.msg (Err.Undeclared("class method", mf2str fm))
			);
			if ast.typparams = []
			then ()
			else Err.msg (Err.ClassMthdAndTypPar cid);
			(* now do the type-checking *)
			let subst_sig = Decl.signature typ fm (List.length apl)
			in
			( try
				List.iter2 (tc_expr) (some subst_sig.pt "bs_infer;cc") apl;
				tc.tc_classmthd <- add_sosgl (fm, typ) tc.tc_classmthd;
				subst_sig.rt
			with Invalid_argument(_) ->
					Err.msg (Err.LengthParamL(class_ra (), fm))
			)
	
	| New(typ, apl) ->
	(* first check, whether a constructor is called *)
			let cid = type2id typ in
			let ast = i_ast cid in
			let cid' = Ly.c2mf cid in
			let len = List.length apl in
			if (ast.classkind = AbstractClass)
			then Err.msg (Err.AbstrCreated(class_ra ()))
			else ();
			(* now do the type-checking *)
			let subst_sig = Decl.signature typ cid' len in
			( try
			(* This is the second part of a hack:
			* The context tc.tc_newflag implies if true that
			* the object is not "dynamically allocated". Hence
			* has not to be be added to the list tc.tc_dyn_new
			* The flag is set to false to ensure that apl
			* is typechecked in an reset context *)
				if tc.tc_newflag
				|| is_sensor_or_signal_typ typ
				|| is_sim_input_or_output_typ typ then (
				(* the object is not considered as dynamically generated *)
				) else (
					tc.tc_dyn_new <- add_sosgl typ tc.tc_dyn_new
				);
				if debug_level DbgStatus then (
					print_status ps ast
				);
				tc.tc_newflag <- false;
				List.iter2 (tc_expr) (some subst_sig.pt "bs_infer:New") apl;
				tc.tc_new <- add_sosgl typ tc.tc_new;
				typ
			with Invalid_argument(_) ->
					Err.msg (Err.LengthParamL(class_ra (), cid'))
			)
	
	| This -> if (tc.tc_decl.scope = Class)
			then (type_err "\"This\" used in class methods.\n")
			else lookup_typ
	
	| Super(fm, apl) ->
			( try (* try the parents only *)
				let len = List.length apl in
				let dcll = Hashtbl.find (i_ast tc.tc_cid_to_tc).symtab (fm, len) in
				if List.exists (fun d -> not( is_abstract d )) dcll
				then ()
				else raise Not_found;
				bs_infer lookup_typ full_expr (Call(fm, Some apl))
			with
				Not_found -> Err.msg (Err.IllegalSuper(class_ra(), fm)) )
	
	| Cast(tgt, e) ->
			let src = tc_infer tc.tc_typ_to_tc e in
			let eid = match tgt with
				| TypeVar t -> Ly.c2mf t
				| _ -> id_dont_use in
			let tgt = Decl.subst_typ tc.tc_typ_to_tc eid tgt in
			if is_primitive_typ src then (
				Err.intern "bs_infer:Cast"
			) else if src = Null then (
				type_err "Expressions which return type Null ('void') \
				cannot be cast"
			) else (
				if le_type ~sub: src ~super: tgt
				|| le_type ~sub: tgt ~super: src then (
					tgt
				) else (
					type_err (Err.type2str src^" cannot be cast to "
							^Err.type2str tgt^". The types are incompatible")
				)
			)
	
	| ThisConstr(apl) ->
			type_err "Invalid (place of) call of this(...)"
	
	| SuperConstr(apl) ->
			type_err "Invalid (place of) call of super(...)"
	
	| NullObj -> Null
	
	| IncrDecr(_, _) -> tc_assign full_expr
	
	| Assign(_, _, _, _) -> tc_assign full_expr
	
	| ArrayLiteral (Dim1 l) ->
			let at = tc_arraylit1 l.av1 in
			incr_array_lit1 at l; at
	
	| ArrayLiteral (Dim2 l) ->
			let at = tc_arraylit2 l.av2 in
			incr_array_lit2 at l; at
	
	| Literal (t, _, _) -> t
	
	| If(thenl) ->
			let rt = tc_thenl thenl in
			let fn =
				function (* check the boolean tests *)
				| Then(_, e, _) -> set_lbl e.elbl;
						tc_expr t_bool e
				| Else(_, _) -> ()
			in
			let lbl = tc.tc_lbl in
			List.iter fn thenl;
			set_lbl lbl;
			rt
	
	| DeltaT ->
			if tc.tc_flowcontext
			then t_double
			else Typ(id_sensor,[t_double])
	
	| Instant ->
			let ck = (i_ast tc.tc_cid_to_tc).classkind in
			if not(ck = ConfClass)
			then Err.msg (Err.OnlyConf)
			else ();
			t_int
	
	| Slice r ->
			let _ = tc_expr t_int r.lower
			and _ = tc_expr t_int r.upper in
			Null
	
	| DotDot(_)
	| Present(_)
	| SigVal(_)
	| Value(_)
	| Var(_)
	| Offset(_)
	| Timestamp(_) -> Err.intern "bs_infer:encsig"

and tc_infer lookup_typ expr =
	let inferred_type = bs_infer lookup_typ expr expr.expr in
	expr.etyp <- Some inferred_type;
	inferred_type

and chk_formals_and_locals kind formal =
	tc_clockdecl formal.p_lbl formal.p_clock;
	let name = formal.p_name in
	try
		( try (* defined within the class? *)
			let _ = i_entry tc.tc_cid_to_tc name (- 1) in
			raise (Failure "")
		with Not_found -> ()
		);
		( try (* defined as other formal? *)
			let _ = Hashtbl.find tc_fl name in raise (Failure "")
		with Not_found -> ()
		);
		let sgn = Decl.subst tc.tc_typ_to_tc name { rt = formal.p_type; pt = None } in
		Hashtbl.add tc_fl name
			{ name = name; origin = tc.tc_cid_to_tc; entry = kind;
				scope = tc.tc_decl.scope; volatile = false;
				final = true; parameter = false;
				signature = sgn; access = Private
			}
	with Failure _ ->
			Err.msg (Err.IdentifierTwice(name, class_ra()))

and tc_clockdecl lbl clock =
	set_lbl lbl;
	let flowdef = tc.tc_flowcontext in
	try
		tc.tc_flowcontext <- true;
		tc_expr t_bool clock;
		tc.tc_flowcontext <- flowdef
	with Invalid_argument(_) ->
			tc.tc_flowcontext <- flowdef;
			Err.msg (Err.SigClock(class_ra ()))

and check_choice it c =
	match c with
	| CaseLiteral (t, _, _) ->
			if le_type ~sub: t ~super: it
			then ()
			else type_err "Cases in switch invalid"
	| CaseId i ->
			let dcl = Decl.get Rhs i (- 1) in
			if not(is_staticfinal dcl)
			then type_err ("\""^(mf2str i)^"\" must be static final.\n");
			if dcl.signature.rt != it
			then type_err ("\""^(mf2str i)^"\" is of invalid type.\n");

and check_case it { sfrom = c1; sto = c2; sstmtl = stmtl } =
	check_choice it c1;
	( match c2 with
		| None -> ()
		| Some c -> check_choice it c
	);
	mustbe_nulltype (tc_stmtl stmtl)

and check_assert = function
	| Assertion(expr, b, exc) ->
			tc_expr t_bool expr;
			tc_expr t_int exc;
			ignore (eval_const_expr tc.tc_cid_to_tc exc)
	| _ -> Err.intern "check_assert"

and tc_index dimension index =
	match index.expr with
	| Slice r ->
			let l = eval_const_expr_2_int tc.tc_cid_to_tc r.lower
			and u = eval_const_expr_2_int tc.tc_cid_to_tc r.upper in
			if l <= u
			then () (* ok *)
			else type_err "The upper bound of a slice of a vector or matrix \
			is greater than the lower bound";
			r.lower <- { etyp = Some t_int;
				expr = Literal (t_int, i2s l, Some(Int64.of_int l));
				elbl = r.lower.elbl
			};
			r.upper <- { etyp = Some t_int;
				expr = Literal (t_int, i2s u, Some(Int64.of_int l));
				elbl = r.lower.elbl
			};
			let range = DimLen(u - l + 1) in
			if le_dim ~sub: range ~super: dimension
			then () (* ok *)
			else type_err "The slice exceeds the size of a vector or matrix";
			Some range
	| _ ->
			tc_expr t_int index;
			None

and tc_expr_in_emit t expr il sid =
	let te = tc_infer tc.tc_typ_to_tc expr in
	let err t te =
		type_err ("The left hand side has type '"^Err.type2str t^
				"' but the right hand has type '"^Err.type2str te^".
				Note that the value types of arrays must be equal.
				We do not support subtyping for arrays. This is on purpose. \
				If values are of primitive type, the programmer should \
				be aware of the fact that the format of all data are \
				changed (at computational and memory costs). If the \
				values are of reference type upcast causes a covariance \
				problem invariably leading to a runtime error.\n") in
	( match t with
		| Array(vt, DimLen 1, d2) when tc.tc_flowcontext ->
				( match il with
					| [] ->
							if le_type ~sub: te ~super: t
							|| le_type ~sub: te ~super: vt (* polymorphic *)
							|| dim2value d2 = 1 && le_type ~sub: vt ~super: te
							then () (* ok *)
							else err t te
					| [i1] ->
							( match tc_index d2 i1 with
								| None ->
										if le_type ~sub: te ~super: vt
										then () (* ok *)
										else err t te
								| Some r ->
										if le_type ~sub: te ~super: (Array(vt, DimLen 1, r))
										then () (* ok *)
										else err (Array(vt, DimLen 1, r)) te
							)
					| _ -> type_err
								("Identifier '"^(mf2str sid)^ "' is of vector type.\n")
				);
		| Array(vt, d1, d2) when tc.tc_flowcontext ->
				( match il with
					| [] ->
							if le_type ~sub: te ~super: t || le_type ~sub: te ~super: vt
							(* polymorphic *)
							then () (* ok *)
							else err t te
					| [i1; i2] ->
							( match tc_index d1 i1, tc_index d2 i2 with
								| None, None ->
										if le_type ~sub: te ~super: vt
										then () (* ok *)
										else err t te
								| None, Some r2 ->
										if le_type ~sub: te
											~super: (Array(vt, DimLen 1, r2))
										then () (* ok *)
										else err (Array(vt, DimLen 1, r2)) te
								| Some r1, None ->
										if le_type ~sub: te
											~super: (Array(vt, r1, DimLen 1))
										then () (* ok *)
										else err (Array(vt, r1, DimLen 1)) te
								| Some r1, Some r2 ->
										if le_type ~sub: te ~super: (Array(vt, r1, r2))
										then () (* ok *)
										else err (Array(vt, r1, r2)) te
							)
					| _ ->
							type_err ("Identifier '"^(mf2str sid)^"' is of matrix type.\n")
				)
		| _ when te = t ->
				if il = []
				then () (* ok *)
				else type_err ("The signal at the left of the operator := is \
							not a vector or matrix type but is indexed.\n")
		| _ when is_primitive_typ t && le_type ~sub: te ~super: t ->
				if il = []
				then () (* ok *)
				else type_err ("The signal at the left of the operator := is \
							not a vector or matrix type but is indexed.\n")
		| _ ->
				if tc.tc_flowcontext then (
					type_err ("The signal at the left of the operator := is \
							of type \'"^Err.type2str t^"\'. But the \
							expression at the right is of type \
							\'"^Err.type2str te^"\'.")
				) else (
					type_err ("The type \'"^Err.type2str te^"\' of the value \
							does not match the type \'"^Err.type2str t^"\' \
							of the emitted signal.\n")
				)
	)

and tc_emit emit_typ sid il expr =
	match (Decl.get Rhs sid (- 1)).signature with
	| { pt = None; rt = t } ->
			if is_signal_typ t then (
			(* ok *)
			) else (
				if tc.tc_flowcontext then (
					type_err "The operator := may only be used with signals"
				) else (
					type_err "Emit may only be used with signals"
				)
			);
			let t = match sigtyp2valtyp t with
				| Some t ->
						t
				| None ->
						if tc.tc_flowcontext then (
							type_err "Identifier at the left hand side \
							of operator := is not a signal"
						) else (
							type_err "Identifier in an emit statement \
							is not a signal"
						)
			in
			let t =
				match emit_typ with
				| StateEq e ->
						if t = t_float || t = t_double then (
							( match e with
								| None -> ()
								| Some expr -> tc_expr_in_emit t expr il sid
							);
							t
						) else (
							type_err "State equations must have type \
							'float' or type 'double'"
						)
				| _ -> t
			in
			tc_expr_in_emit t expr il sid
	| _ -> Err.intern "tc_emit_and_constrain"

and tc_assign expr =
	match expr.expr with
	| Assign(id, NoArr, op, e) ->
			let dcl = Decl.get Lhs id (- 1) in
			let rt = dcl.signature.rt in
			(* The following is a bad hack, however:
			* newly generated objects are handled specifically if they occur
			* in a field declaration or an assignment defining a field. These
			* can be expanded. In other cases, the objects are generated
			* "dynamically" implying that garbage collection needs to be started
			* In order to distinguish these situations when a "New ... " expression
			* is typchecked the flag ac_newflag is set. If not the new object
			* is "allocated dynamically" hence its type is added to the
			* ac_newdy list *)
			( match expr.expr with
				| New(_, _) -> tc.tc_newflag <- true
				| _ -> ()
			);
			let it = tc_infer tc.tc_typ_to_tc e in
			tc.tc_newflag <- false;
			tc_add_assign dcl e;
			if le_type ~sub: it ~super: rt then (
			(* OK *)
			) else (
				Err.msg (Err.Subtyping(class_ra(), it, rt))
			);
			(* Here we remember whether how many instances are generated
			* for a class. At typechecking stage only three informations
			* are relevant:
			* NotTouched - initialisation
			* Singleton - only one instance
			* Multiple - possibly several instances.
			*)
			if is_reference_typ rt then (
				set_status dcl.origin dcl.name;
			);
			(* START: if the rt is a signal typ, check correct signal bus +++ *)
			if is_sensor_or_signal_typ rt then (
				if tc.tc_signalbus then (
					match e.expr with
					| New(_, _) ->
							tc.tc_newflag <- true
					| Call(fm, None) ->
							let dcl = Decl.get Rhs fm (- 1) in
							if dcl.entry = ConstLocal
							then ()
							else type_err "Only formal constructor \
					parameters || new signal types \
					can be assigned to"
					| _ ->
							Err.intern "tc_assign"
				) else (
					type_err "Assigning to variables of signal types is illegal \
					here. The signal bus must be built at the \
					unconditional tail of the statement list \
					of a constructor"
				)
			) else ();
			(* END: check for correct signal bus ---------------------------- *)
			if dcl.signature.pt = None then (
				if e.expr = NullObj then
					if op = id_op_assign
					then ()
					else type_err "Invalid type for this assignment <op>="
				else
				if op = id_op_assign
				then ()  (* =, and not += etc *)
				else ( let i = mk_ecall id None expr.elbl in
					tc_expr rt (mk_e_inf op i e expr.elbl) )
				;
				rt
			) else (
				type_err "Invalid left-hand-side in ="
			)
	| Assign(id, idx, op, e) when op = id_op_assign ->
			let err () =
				type_err ("Identifier "^(mf2str id)^" has a type which \
						is invalid for this kind of assignment to \
						an array item.\n")
			and sub_err it at = Err.msg (Err.Subtyping(class_ra(), it, at))
			and dcl = Decl.get Rhs id (- 1) in
			let rt = dcl.signature.rt in
			let it = tc_infer tc.tc_typ_to_tc e in
			( match rt with
				| Array(at, DimLen 1, d2) ->
						( match idx with
							| ADim1 x ->
									( match tc_index d2 x with
										| None ->
												if le_type ~sub: it ~super: at
												then () (* ok *)
												else sub_err at it
										| Some r ->
												if le_type ~sub: it ~super: (Array(at, DimLen 1, r))
												then () (* ok *)
												else sub_err (Array(at, DimLen 1, r)) it
									)
							| ADim2 _ ->
									type_err ("Identifier "^(mf2str id)^
											" has one dimensional array type.\n")
							| _ ->
									Err.intern "tc_assign:array1"
						)
				| Array(at, d1, d2) ->
						( match idx with
							| ADim1 _ ->
									type_err ("Identifier "^(mf2str id)^
											" has two dimensional array type.\n")
							| ADim2(x, y) ->
									( match tc_index d1 x, tc_index d2 y with
										| None, None ->
												if le_type ~sub: it ~super: at
												then () (* ok *)
												else sub_err at it
										| None, Some r2 ->
												if le_type ~sub: it
													~super: (Array(at, DimLen 1, r2))
												then () (* ok *)
												else sub_err (Array(at, DimLen 1, r2)) it
										| Some r1, None ->
												if le_type ~sub: it
													~super: (Array(at, r1, DimLen 1))
												then () (* ok *)
												else sub_err (Array(at, r1, DimLen 1)) it
										| Some r1, Some r2 ->
												if le_type ~sub: it ~super: (Array(at, r1, r2))
												then () (* ok *)
												else sub_err (Array(at, r1, r2)) it
									);
							| _ -> err ()
						)
				| _ -> err ()
			);
			if dcl.signature.pt = None then (
				rt
			) else (
				type_err "Invalid left-hand-side in ="
			)
	| Assign(i, idx, op, e) ->
			let err () =
				type_err ("Identifier "^(mf2str i)^" has a type which \
						is invalid for this kind of assignment to \
						an array item.\n") in
			let dcl = Decl.get Rhs i (- 1) in
			let rt = dcl.signature.rt in
			let it = tc_infer tc.tc_typ_to_tc e in
			let at = match rt with
				| Array(at, DimLen 1, _) ->
						( match idx with
							| ADim1 i ->
									tc_expr t_int i;
									at
							| ADim2 _ ->
									type_err ("Identifier "^(mf2str i)^
											" has one dimensional \
											array type.\n")
							| _ ->
									Err.intern "tc_assign:array1"
						)
				| Array(at, _, _) ->
						( match idx with
							| ADim1 _ ->
									type_err ("Identifier "^(mf2str i)^
											" has two dimensional \
											array type.\n")
							| ADim2(x, y) ->
									tc_expr t_int x;
									tc_expr t_int y;
									at
							| _ ->
									Err.intern "tc_assign:array2"
						)
				| _ -> err ()
			in
			if le_type ~sub: it ~super: at
			then ()
			else Err.msg (Err.Subtyping(class_ra(), it, at));
			if dcl.signature.pt = None then (
				if e.expr = NullObj
				then if op = id_op_assign
					then ()
					else type_err "Invalid type for this assignment <op>="
				else if op = id_op_assign
				then ()  (* =, and not += etc *)
				else ( let i = mk_ecall i None expr.elbl in
					tc_expr at (mk_e_inf op i e expr.elbl) );
				rt
			) else (
				type_err "Invalid left-hand-side in ="
			)
	| IncrDecr(i, _) ->
			let dcl = Decl.get Lhs i (- 1) in
			let rt = dcl.signature.rt in
			( match rt with
				| Simple(c) when ( c = id_byte || c = id_short ||
					c = id_int || c = id_long ||
					c = id_char || c = id_uint16 ||
					c = id_uint32 || c = id_uint64 ) ->
						()
				| _ ->
						type_err "Invalid type for increment/decrement"
			);
			expr.etyp <- Some rt;    (* needed by tc_add_assign, done by
			tc_infer a second time (redundantly) *)
			tc_add_assign dcl expr;  (* expr indicates not new & result type *)
			rt
	| _ -> Err.intern "tc_assign"

and tc_arraylit1 el =
	let tl = List.map (tc_infer tc.tc_typ_to_tc) el in
	match most_general_type tl with
	| Null ->
			Err.msg (Err.SuperType (class_ra(), "In an array literal no \
						suitable type could be inferred", tl))
	| t ->
			if is_literal_type t
			then err_literal_type_not_unique ()
			else if is_array_typ t
			then Err.intern "tc_arraylit1"
			else Array(t, DimLen 1, DimLen (List.length tl))

and tc_arraylit2 ell =
	let tll = List.map (fun el -> List.map (tc_infer tc.tc_typ_to_tc) el) ell in
	let tl = List.flatten tll in
	match most_general_type tl with
	| Null ->
			Err.msg (Err.SuperType (class_ra(), "In an array literal no \
						suitable type could be inferred", tl))
	| t ->
			if is_literal_type t
			then err_literal_type_not_unique ()
			else if is_array_typ t
			then Err.intern "tc_arraylit2"
			else try Array(t,
						DimLen (List.length tll),
						DimLen (List.length (List.hd tll)))
				with _ -> Err.intern "tc_arraylit2"

(* -------------------------------------------------------------------------
typecheck STATEMENTS
------------------------------------------------------------------------- *)
and tc_stmtl =
	function
	| [] ->
			Null
	| [stmt] ->
			tc_stmt stmt
	| stmt:: stmtl ->
			mustbe_nulltype (tc_stmt stmt);
			tc_stmtl stmtl

and tc_chk_then =
	function
	| [] ->
			Null
	| [s] ->
			tc_stmt s
	| sl ->
			let inferred = tc_stmtl sl in
			if inferred = Null
			then Null
			else type_err "Statement list illegal in then"

and tc_thenl =
	function
	| [] ->
			Null
	| [Then(_, _, stmtl)] ->
			tc_chk_then stmtl
	| [Else(_, stmtl)] ->
			tc_chk_then stmtl
	| t:: tl ->
			let tt = tc_thenl [t] in
			let tlt = tc_thenl tl in
			if (le_type ~sub: tt ~super: tlt)
			then tlt
			else if (le_type ~sub: tlt ~super: tt)
			then tt
			else type_err "Then/Else are incompatible"

and tc_bool_time_thenl =
	function
	| Then(_, e, stmtl) ->
			let ti = tc_infer tc.tc_typ_to_tc e in
			if ti = t_bool || ti = t_time
			then ()
			else type_err "condition of await must \
			evaluate either to a \
			bool or a time value";
			mustbe_nulltype (tc_stmtl stmtl)
	| Else(_, stmtl) ->
			type_err "illegal else in await or cancel statement"

and tc_stmt =
	function
	| Activate(lbl, _, stmtl, e) ->
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl stmtl);
			set_lbl lbl; tc_expr t_bool e;
			Null
	| AssertStmt(lbl, al) ->
			set_lbl lbl;
			List.iter check_assert al;
			Null
	| GraphicStM (lbl, g) ->
			Null
	| TextStM (lbl, a) ->
			set_lbl lbl;
			tc_automaton a;
			Null
	| Await(lbl, _, thenl) ->
			set_lbl lbl;
			List.iter tc_bool_time_thenl thenl;
			Null
	| Break(lbl, None) ->
			set_lbl lbl; Null
	| Break(lbl, Some mfid) ->
			set_lbl lbl;
			Err.msg (Err.NYI "break with label \
					not yet implemented.\n")
	| Continue(lbl, None) ->
			set_lbl lbl; Null
	| Continue(lbl, Some mfid) ->
			set_lbl lbl;
			Err.msg (Err.NYI "continue with label \
					not yet implemented.\n")
	| DoStmt(lbl, cond, loop) ->
			set_lbl lbl;
			tc_expr t_bool cond;
			mustbe_nulltype (tc_stmtl loop);
			Null
	| Emit(lbl, et, sid, il, expr) ->
			set_lbl lbl;
			tc_emit et sid il expr;
			Null
	| ExprStmt(lbl, expr) ->
			set_lbl lbl;
			let rt = tc_infer tc.tc_typ_to_tc expr in
			if (is_assign expr)
			then Null
			else rt
	| Halt(lbl) ->
			set_lbl lbl;
			Null
	| Next(lbl) ->
			set_lbl lbl;
			Null
	| Nothing(lbl) ->
			set_lbl lbl;
			Null
	| NextState(_, _) ->
			type_err "next state statement illegal"
	| Par(lbl, stmtl_l) -> set_lbl lbl;
			let fp stmtl = mustbe_nulltype (tc_stmtl stmtl) in
			List.iter fp stmtl_l;
			Null
	| FlowContext(lbl, fc) ->
			set_lbl lbl;
			tc_flowcontext fc;
			Null
	| Cancel(lbl, _, _, stl, thl) ->
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl stl);
			set_lbl lbl;
			List.iter tc_bool_time_thenl thl;
			Null
	| RctLoop(lbl, stmtl) ->
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl stmtl);
			Null
	| Return(lbl, None) ->
			set_lbl lbl;
			if tc.tc_decl.signature.rt = Null
			then ( Null )
			else ( type_err "A method with a return type \
				(a function) has to define a \
				value to be returned in each \
				return statement.\n" )
	| Return(lbl, Some expr) ->
			set_lbl lbl;
			if tc.tc_decl.signature.rt = Null
			then ( type_err "A return value in a return \
				statement is illegal for a \
				method without return type.\n" )
			else ( let rt = Decl.subst_typ tc.tc_typ_to_tc
						tc.tc_decl.name
						tc.tc_decl.signature.rt in
				tc_expr rt expr;
				Null )
	| Schedule(lbl, expr) ->
			set_lbl lbl;
			tc_expr Null expr;
			Null
	| Sustain(lbl, stmtl) ->
			set_lbl lbl;
			let err () = type_err "sustain is applied to a \
			statement that is not \
			instanteaneous.\n" in
			List.iter (chk_instanteaneous err) stmtl;
			Null
	| Throw(lbl, expr) ->
			set_lbl lbl;
			tc_expr t_int expr;
			( try
				ignore (eval_const_expr tc.tc_cid_to_tc expr)
			with _ ->
					type_err "The value of a throw statement \
			has to be an int expression \
			which evaluates to a constant at \
			compile time"
			);
			Null
	| While(lbl, cond, loop) ->
			set_lbl cond.elbl;
			tc_expr t_bool cond;
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl loop);
			Null
	
	(* longer type-checks are here ---------------------------------------------- *)
	| Switch (lbl, sw) -> (* fix type of switch, then tc *)
			set_lbl lbl;
			let it = tc_infer tc.tc_typ_to_tc sw.swexpr in
			if is_integral_typ it then (
			(* ok *)
			) else (
				type_err "~nvalid type of expression in switch statement"
			);
			set_lbl lbl;
			List.iter (check_case it) sw.swcase;
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl sw.swdflt);
			Null
	| ForStmt(lbl, f) ->
			( match f.forinit with
				| None ->
						()
				| Some e -> set_lbl e.elbl;
						if is_assign e then (
							ignore(tc_infer tc.tc_typ_to_tc e) (* any type OK *)
						) else (
							tc_expr Null e
						)
			);
			( match f.fortest with
				| None ->
						()
				| Some e ->
						set_lbl e.elbl;
						tc_expr t_bool e;
			);
			( match f.forupd with
				| None ->
						()
				| Some e ->
						set_lbl e.elbl;
						ignore(tc_infer tc.tc_typ_to_tc e) (* any type OK *)
			);
			set_lbl lbl;
			mustbe_nulltype (tc_stmtl f.forstmtl);
			Null
	| LetStmt(lbl, l) -> (* add the local decl to the symbol table, typecheck the
	stmtl, then remove the local decl
	Note: there is a similar function used for FlowContext *)
			set_lbl lbl;
			( match l.letexpr with
				| None ->
						( )
				| Some e ->
						tc_expr l.letent.p_type e
			);
			chk_formals_and_locals l.letdf l.letent;
			mustbe_nulltype (tc_stmtl l.letil);
			rm_local_param l.letent.p_name;
			Null

(* -------------------------------------------------------------------------
the following functions are used to typecheck FlowContext's
------------------------------------------------------------------------- *)
and tc_flowcontext fc =
	tc.tc_flowcontext <- true;
	List.iter (tc_flowdecls ~add: true) fc.f_dcls;
	mustbe_nulltype (tc_stmtl fc.f_equ);
	List.iter (tc_flowdecls ~add: false) fc.f_dcls;
	tc.tc_flowcontext <- false;

and tc_flowdecls ~add = function
	| LetStmt(lbl, l) ->
	(* add the local decl to the symbol table,
	or remove the local decl
	Note: there is a similar function used for LetStmt *)
			set_lbl lbl;
			( match l.letexpr with
				| None -> ()
				| Some e -> tc_expr l.letent.p_type e
			);
			( match l.letil with
				| [] -> ()
				| _ -> Err.intern "tc_flowdecls:letil"
			);
			if add
			then chk_formals_and_locals l.letdf l.letent
			else rm_local_param l.letent.p_name
	| _ -> Err.intern "tc_flowdecls:stmt"

(* -------------------------------------------------------------------------
the following functions are used to typecheck AUTOMATONS
------------------------------------------------------------------------- *)
and state_err s =
	Err.msg (Err.FsmState(class_ra (), s))

and tc_statename_dupl = function
	| [] -> None
	| s:: sn ->
			if List.mem s sn
			then Some s
			else tc_statename_dupl sn

and tc_automaton automaton =
	let state_names = List.map (fun s -> s.sname) automaton.a_states in
	( match tc_statename_dupl state_names with
		| Some dupl -> state_err ("state name \""^mf2str dupl^"\" not unique")
		| None -> ()
	);
	if (List.mem id_exit state_names) then
		state_err "\"exit\" illegal as state name"
	else
		let states = automaton.a_init :: automaton.a_states in
		fsm_valid_states state_names (id_init:: state_names) states

(* -------------------------------------------------------------------------
fsm_valid_stmtl unvisited_states state_l:
traverse the reactive statements of ''state_l'' and check them.
------------------------------------------------------------------------- *)
and fsm_valid_states or_states unvisited_states = function
	| [] ->
			if unvisited_states = []
			then ()
			else state_err "some states are unused"
	| s:: sl ->
			let (v, unvisited_states) =
				List.partition
					( fun n -> n = s.sname
					) unvisited_states
			in
			( match v with
				| [_] -> ()
				| _ -> Err.intern "fsm_valid_states"
			);
			fsm_valid_state or_states s;
			fsm_valid_states or_states unvisited_states sl

and fsm_valid_state or_states state =
	set_lbl state.slbl;
	mustbe_nulltype (tc_stmtl state.sdo);    (* any reactice stmt is valid *)
	List.iter fsm_valid_stmt state.sentry;   (* only instantaneous stmts   *)
	List.iter fsm_valid_stmt state.sexit;
	List.iter fsm_valid_stmt state.sduring;
	fsm_valid_thenl or_states false state.strans

(* -------------------------------------------------------------------------
check the consistency of transitions
A SIMILAR, BUT SIMPLER VERSION IS USED IN util_parse.ml
------------------------------------------------------------------------- *)
and fsm_then_expr expr =
	let ti = tc_infer tc.tc_typ_to_tc expr in
	if ti = t_bool || ti = t_time
	then ()
	else type_err "A condition of a transition must \
evaluate either to a bool or a time value"

and fsm_valid_thenl or_states total =
	function
	| [] ->
			if total
			then state_err "transition not total"
			else ()
	| [Then(_, e, stmtl)] ->
			fsm_then_expr e;
			if total
			then state_err "transition not total"
			else fsm_valid_then or_states total stmtl
	| [Else(_, stmtl)] ->
			fsm_valid_then or_states total stmtl
	| Then(_, e, stmtl):: thenl ->
			fsm_then_expr e;
			fsm_valid_then or_states total stmtl;
			fsm_valid_thenl or_states total thenl
	| Else(_, _):: _ ->
			Err.intern "fsm_valid_thenl"

and fsm_valid_then or_states total =
	function
	| [] ->
			state_err "next state missing"
	| [ExprStmt(_,{ expr = If(thenl) })] ->
			fsm_valid_thenl or_states true thenl
	| [NextState(_, s)] ->
			if (List.mem s or_states) || s = id_exit
			then ()
			else ( state_err ("state "^(mf2str s)^" missing") )
	| stmt:: stmtl ->
			fsm_valid_stmt stmt;
			fsm_valid_then or_states total stmtl

and fsm_valid_stmt stmt =
	let err () = state_err "invalid statement (not instanteaneous)" in
	chk_instanteaneous err stmt

and chk_instanteaneous err stmt =
	match stmt with
	| ExprStmt(_,{ expr = If(thenl) }) ->
			mustbe_nulltype (tc_stmt stmt);
			List.iter (chk_instanteaneous_if err) thenl
	| Nothing _
	| FlowContext _
	| ExprStmt _
	| Emit _
	| Schedule _
	| AssertStmt _ ->
			mustbe_nulltype (tc_stmt stmt)
	| Par(_, stmtll) ->
			List.iter (List.iter (chk_instanteaneous err)) stmtll
	| _ ->
			err ()

and chk_instanteaneous_if err = function
	| Then(_, _, stmtl) ->
			List.iter (chk_instanteaneous err) stmtl
	| Else(_, stmtl) ->
			List.iter (chk_instanteaneous err) stmtl

(* -------------------------------------------------------------------------
check whether the last statement is a return [needed for functions]
------------------------------------------------------------------------- *)
let rec last_stmt_is_save_return sl =
	try
		match list_last sl with
		| Return _
		| Throw _ ->
				true
		| LetStmt(_, l) ->
				last_stmt_is_save_return l.letil
		| ExprStmt(_, e) ->
				( match e.expr with
					| If tl -> List.for_all last_thenelse_is_save_return tl
					| _ -> false
				)
		| _ -> false
	with _ -> false

and last_thenelse_is_save_return =
	function
	| Then(_, _, sl) -> last_stmt_is_save_return sl
	| Else(_, sl) -> last_stmt_is_save_return sl

(* -------------------------------------------------------------------------
typechecking of a method or constructor
------------------------------------------------------------------------- *)
let tc_method mthd =
	set_lbl mthd.msrcp;
	tc.tc_flowcontext <- false;
	mthd.mread <- [];
	mthd.mwrite <- [];
	mthd.mcall <- [];
	Hashtbl.clear tc_fl;
	List.iter (fun f -> chk_formals_and_locals (ConstLocal) f) mthd.mformals;
	List.iter check_assert mthd.mpre;
	List.iter check_assert mthd.mpost;
	match mthd.mbody with
	| TextStmtL sl ->
			if tc.tc_decl.signature.rt = Null
			then ()
			else if last_stmt_is_save_return sl
			then ()
			else type_err "If a method returns a result, \
			the last statement of its body \
			is required to be a return \
			statement";
			mustbe_nulltype (tc_stmtl sl)
	| Nobody -> ()

type tregion =
	| AnyStmt
	| SignalInit
	| RctObjInit

let signalbus_err () =
	type_err "The constructor of a reactive class must have a so - called \
constructor tail of statements which specify the signalbus \
(the reactive objects and the signal connections). This \
constructor tail must first initialize all signals, then \
call the constructors for reactive objects and be terminated \
by an active compound statement"

let rec adj_sigro region = function
	| [] ->
			([],[],[], AnyStmt) (* 4rd don't care *)
	| s:: sl ->
			let e = match s with
				| ExprStmt(_, e) -> e
				| _ -> Err.intern "adj_sigro" in
			let t = some e.etyp "adj_sigro" in
			if is_sensor_or_signal_typ t then (
				if region = AnyStmt || region = SignalInit then (
					let (cbody, csig, cro, _) = adj_sigro SignalInit sl in
					(cbody, s:: csig, cro, AnyStmt) (* 4rd don't care *)
				) else (
					signalbus_err ()
				)
			) else if is_reactive_type t then (
				( match e.expr with
					| Assign(_, NoArr, op, e) when op = id_op_assign && is_newexpr e ->
							()
					| _ -> signalbus_err ()
				);
				let (cbody, csig, cro, _) = adj_sigro RctObjInit sl
				in
				(cbody, csig, s:: cro, AnyStmt) (* 4rd don't care *)
			) else (
				if region = AnyStmt then (
					let (cbody, csig, cro, _) = adj_sigro AnyStmt sl
					in
					(s:: cbody, csig, cro, AnyStmt) (* 4rd don't care *)
				) else (
					signalbus_err ()
				)
			)

let tc_constructor constr =
	set_lbl constr.csrcp;
	tc.tc_flowcontext <- false;
	constr.cread <- [];
	constr.cwrite <- [];
	constr.ccall <- [];
	Hashtbl.clear tc_fl;
	List.iter (fun f -> chk_formals_and_locals (ConstLocal) f) constr.cformals;
	( match constr.cbody with
		| s:: sl ->
				( match s with
					| ExprStmt(lbl, ({ expr = ThisConstr(apl) } as orig)) ->
							let bsex = Call(Ly.c2mf tc.tc_cid_to_tc, Some apl) in
							let expr = { etyp = None; expr = bsex; elbl = lbl } in
							let infer = bs_infer tc.tc_typ_to_tc expr bsex in
							mustbe_nulltype infer;
							orig.etyp <- Some infer;
							mustbe_nulltype (tc_stmtl sl);
					| ExprStmt(lbl, ({ expr = SuperConstr(apl) } as orig)) ->
							let super_typ = (i_ast tc.tc_cid_to_tc).extends in
							let super_cid = type2id super_typ in
							let bsex = Call(Ly.c2mf super_cid, Some apl) in
							let expr = { etyp = None; expr = bsex; elbl = lbl } in
							let infer = bs_infer super_typ expr bsex in
							mustbe_nulltype infer;
							orig.etyp <- Some infer;
							mustbe_nulltype (tc_stmtl sl);
					| _ -> mustbe_nulltype (tc_stmtl constr.cbody)
				)
		| [] -> ()
	);
	tc.tc_signalbus <- true;
	mustbe_nulltype (tc_stmtl constr.csig);
	mustbe_nulltype (tc_stmtl constr.cro);
	tc.tc_signalbus <- false;
	( let (cbody, csig, cro, _) = adj_sigro AnyStmt (constr.csig@constr.cro) in
		constr.cbody <- constr.cbody @ cbody;
		constr.csig <- csig;
		constr.cro <- cro
	);
	( match constr.cactive with
		| None -> ()
		| Some sl ->
				if tc.tc_reactive then (
					Err.intern "tc_constructor"
				) else (
					tc.tc_reactive <- true;
					mustbe_nulltype (tc_stmtl sl);
					tc.tc_reactive <- false
				)
	)

(* -------------------------------------------------------------------------
typechecking of a field
------------------------------------------------------------------------- *)
let rec tc_fielddecl typ =
	let fus = match tc.tc_decl.entry with
		| Field fus -> fus
		| _ -> Err.intern "tc_fielddecl:1"
	in
	match typ with
	| Simple _ ->
			( match fus.fldkind with
				| BttmField -> fus.fldkind <- DataField
				| DataField
				| NativeFromC _ -> ()
				| NativeToC _ -> ()
				| _ -> Err.intern "tc_fielddecl:2"
			)
	| TypeVar(_) ->
			( match fus.fldkind with
				| BttmField -> fus.fldkind <- DataField
				| DataField -> ()
				| _ -> Err.intern "tc_fielddecl:3"
			)
	| Array(typ, _, _) ->
			fus.fldkind <- DataField;
			tc_fielddecl typ  (* no reactive context anymore *)
	| Typ(cid, tpl) ->
			let ast = i_ast cid in
			let ck = ast.classkind in
			( match ck with
				| AbstractClass
				| EffectiveClass
				| Anonymous _
				| FinalClass
				| Interface ->
						( match fus.fldkind with
							| BttmField -> fus.fldkind <- DataField
							| DataField -> ()
							| _ -> Err.intern "tc_fielddecl:4"
						)
				| ConfClass
				| RctClass ->
						if tc.tc_cid_to_tc = tc.tc_outer_cid then (
							(* no anonymous class *)
							match fus.fldkind with
							| BttmField -> fus.fldkind <- ReactiveField
							| ReactiveField -> ()
							| _ -> Err.msg (Err.IllegalField (class_ra ()))
						) else (
						(* in an anonymous class the
						reference to enclosing class may be a
						reactive class, and thus be valid *)
						)
			);
			let tc_types act frm =
				if (is_typevar frm) || (le_type ~sub: act ~super: frm)
				then ()
				else Err.msg (Err.ActualTypePar(class_ra (), act, frm))
			in
			( try
				List.iter2 tc_types tpl (typevarl2constraint ast)
			with Invalid_argument(_) ->
					Err.msg (Err.LengthATL (class_ra ()))
			)
	| _ -> Err.intern "tc_fielddecl:6"

let guarantee_validsigtype asig valtyp =
	let builtin_field fmid _ dcl =
		match dcl.entry with
		| Field f ->
				let rt = Decl.subst_typ tc.tc_typ_to_tc dcl.name
						dcl.signature.rt in
				if is_primitive_typ rt || dcl.scope = Class
				then ()
				else Err.msg (Err.SigVal1 (mf2str asig, class_ra()))
		| _ -> ()
	in
	if (is_primitive_typ valtyp) || (valtyp = Null) then (
	(* nothing to do *)
	) else (
		let cid = type2id valtyp in
		let name = mf2str asig in
		let ast = i_ast cid in
		if ast.classkind = FinalClass then (
			(* user defined value type *)
			symtabiter builtin_field ast.symtab;
			try
				match Hashtbl.find ast.symtab (Ly.c2mf cid, 0) with
				| d:: _ when d.origin = cid -> ()
				| _ -> raise Not_found
			with Not_found ->
					Err.msg (Err.SigVal2 (name, class_ra()))
		) else if is_array_typ valtyp then (
			match valtyp with
			| Array(Simple _, DimLen 1, Arbitrary) ->
					Err.msg (Err.SigVal4 (name, class_ra()))
			| Array(Simple _, Arbitrary, Arbitrary) ->
					Err.msg (Err.SigVal4 (name, class_ra()))
			| Array(t, _, _) when not (is_double_or_float_typ t) ->
					Err.msg (Err.SigVal3 (name, class_ra()))
			| _ -> ()
		) else (
			Err.msg (Err.SigVal1 (name, class_ra()));
		)
	)

let tc_signaldecl () =
	let asig = tc.tc_decl.name in
	let sigt = tc_infer tc.tc_typ_to_tc (mk_ecall asig None tc.tc_lbl) in
	match sigtyp2valtyp sigt with
	| Some valt -> guarantee_validsigtype asig valt
	| _ -> Err.intern "tc_signaldecl"

(* -------------------------------------------------------------------------
debug output for usage of fields
------------------------------------------------------------------------- *)
let dbg_assn s e =
	let t = Err.type2str (some e.etyp "dbg_assn") in
	let e = expr2str e in
	ps s; ps e; ps " : "; ps t; ps "]"

let dbg_assn = function
	| NewInit e -> dbg_assn "\n    new   in decl[" e
	| AtInit e -> dbg_assn "\n    =     in decl[" e
	| NewCons e -> dbg_assn "\n    new   in cons[" e
	| AtCons e -> dbg_assn "\n    =     in cons[" e
	| AtCode e -> dbg_assn "\n    new/= in mthd[" e

let dbg_read = function
	| NewInit _ -> Err.intern "dbg_read"
	| AtInit m -> ps "decl "
	| NewCons _ -> Err.intern "dbg_read"
	| AtCons m -> ps "cons "
	| AtCode m -> ps "mthd "

let dbg_mc dcl =
	psmf dcl.name; ps "["; psc dcl.origin; ps "] "

let dbg_usage dcl =
	match dcl.entry with
	| Field f ->
			ps "\n"; ps (mf2str dcl.name);
			if (can_expand dcl)
			then ps "[EXPANDED]:"
			else ps ":";
			ps "\n  assigned: "; List.iter dbg_assn f.assigns;
			ps "\n  read:     "; List.iter dbg_read f.reads
	| Method m ->
			ps "\n"; ps (mf2str dcl.name); ps ":";
			ps "\n  writes: "; List.iter dbg_mc m.mwrite;
			ps "\n  reads:  "; List.iter dbg_mc m.mread;
			ps "\n  calls:  "; List.iter dbg_mc m.mcall
	| Constructor c ->
			ps "\n"; ps (mf2str dcl.name); ps ":";
			ps "\n  writes: "; List.iter dbg_mc c.cwrite;
			ps "\n  reads:  "; List.iter dbg_mc c.cread;
			ps "\n  calls:  "; List.iter dbg_mc c.ccall
	| _ -> ()

(* -------------------------------------------------------------------------
tc_sig_or_field_init: typechecks the optional init - expression
and puts the field into the symtab, i.e. it is
known for init - expressions following textually
------------------------------------------------------------------------- *)
let tc_sig_or_field_init () =
	let dcl = tc.tc_decl in
	try
		let rt = Decl.subst_typ tc.tc_typ_to_tc dcl.name dcl.signature.rt in
		Hashtbl.add tc.tc_symtab (dcl.name, decl2arity dcl) [dcl];
		let ie = get_initexpr dcl in
		( match ie.expr with
			| New(_, _) -> tc.tc_newflag <- true
			| _ -> ()
		);
		let it = tc_infer tc.tc_typ_to_tc ie in
		tc.tc_newflag <- false;
		tc_add_assign dcl ie;
		if le_type ~sub: it ~super: rt
		then ()
		else Err.msg (Err.Subtyping(class_ra(), it, rt));
		(* Here we remember whether how many instances are generated
		* for a class. At typechecking stage only three informations
		* are relevant:
		* NotTouched - initialisation
		* Singleton - only one instance
		* Multiple - possibly several instances.
		*)
		if is_reference_typ rt then (
			set_status dcl.origin dcl.name
		);
		
		(* A HACK to evaluate constant int expressions at compile time + *)
		if (dcl.scope = Class) && dcl.final then (
			let tt = dcl.signature.rt in
			if is_integral_typ tt then (
				match dcl.entry with
				| Field f ->
						let c = tc.tc_cid_to_tc in
						let v = eval_const_expr c ie in
						let l = Literal (tt, Int64.to_string v, Some v) in
						f.init <- Some { ie with expr = l }
				| _ ->
						Err.intern "tc_sig_or_field_init"
			) else if is_literal ie || is_newexpr ie then (
			(* literals & NEW are fine for static finals *)
			) else (
				type_err "This kind of initialization expression is invalid \
				for static finals - use primitive literals only"
			)
			(* HACK - *)
		) else (
		(* no hack for all other cases *)
		)
	with Not_found ->
			Hashtbl.add tc.tc_symtab (dcl.name, decl2arity dcl) [dcl]

(* -------------------------------------------------------------------------
the two main steps in typechecking
tc_step1: check fields
reset assigns - list, which stores assignments to a field
tc_step2: check methods
reset read / writes / calls, for abstract interpretation
------------------------------------------------------------------------- *)
let tc_step1 is_rct dcl =
	if not(dcl.origin = tc.tc_cid_to_tc) then Err.intern "tc_step1" else ();
	tc.tc_decl <- dcl;
	tc.tc_reactive <- is_reactive dcl;
	tc.tc_signalbus <- false;
	tc.tc_flowcontext <- false;
	( match dcl.entry with
		| Field f ->
				f.assigns <- []; f.reads <- [];
				set_lbl f.fldsrcp;
				if dcl.signature.pt = None
				then ()
				else Err.intern "tc_step1";
				if is_rct && not( dcl.access = Private )
				then if dcl.scope = Class && dcl.final
					then ()
					else type_err "All fields in reactive classes \
				have to be either static final \
				or to be private"
				else ();
				let rt = Decl.subst_typ tc.tc_typ_to_tc dcl.name
						dcl.signature.rt in
				tc_fielddecl rt;
				tc_sig_or_field_init ()
		| SigDecl s ->
				set_lbl s.sig_pos;
				s.sig_assigns <- [];
				if dcl.signature.pt = None
				then ()
				else Err.intern "tc_step1";
				if is_rct && not( dcl.access = Private )
				then type_err "All signals in reactive classes \
				have to be private"
				else ();
				tc_clockdecl s.sig_pos s.sig_clock;
				tc_sig_or_field_init (); (* decl must be in symtab ...    *)
				tc_signaldecl ()
		(* ... otherwise tc_signaldecl fails *)
		| _ -> ()
	)

let tc_step2 is_rct dcl =
	tc.tc_decl <- dcl;
	tc.tc_reactive <- is_reactive dcl;
	tc.tc_signalbus <- false;
	tc.tc_flowcontext <- false;
	match dcl.entry with
	| Method r ->
			set_lbl r.msrcp;
			if is_rct then
				if dcl.scope = Instance
				then if dcl.access = Private || is_constructor dcl
					then ()
					else type_err "Instance methods in a reactive \
				class must be private"
				else if dcl.name = id_main
				then ()
				else if dcl.access = Private
				then ()
				else type_err "Class methods in a reactive \
			class must be private"
			else ();
			( match r.method_kind with
				| RctMethod _ | RctNode _ ->
						if r.mpre != [] || r.mpost != [] then
							type_err "Reactive methods may not have pre or post conditions"
						else if dcl.scope = Class then
							type_err "Reactive methods may not be static"
						else if dcl.signature.rt != Null then
							type_err "Reactive methods may not declare a return type"
						else
							( match r.mbody with
								| TextStmtL _ -> ()
								| Nobody -> type_err "Invalid body for a reactive method"
							)
				| _ -> ()
			);
			tc_method r
	| Constructor c -> tc_constructor c
	| _ -> ()

(* -------------------------------------------------------------------------
tc_step4 ... decides whether
- reactive or final fields are initialized
- signal fields are initialized once
------------------------------------------------------------------------- *)
let tc_step4 cstrl dcl =
	tc.tc_decl <- dcl;
	tc.tc_reactive <- is_reactive dcl;
	tc.tc_signalbus <- false;
	tc.tc_flowcontext <- false;
	match dcl.entry with
	| Field f ->
			set_lbl f.fldsrcp;
			let rct =
				match f.fldkind with
				| BttmField -> Err.intern "tc_step4"
				| ReactiveField -> true
				| _ -> false
			and par =
				match f.fldkind with
				| DataField when dcl.parameter -> true
				| _ -> false
			and fin =
				match f.fldkind with
				| DataField when dcl.final -> true
				| _ -> false in
			if rct then (
				match f.assigns with
				| [NewCons _] | [NewInit _] -> ()
				| _ -> type_err "Reactive fields must be initialized \
				once at their declaration or in a \
				constructor with a new object"
			) else ();
			if par then (
				let ck = (i_ast tc.tc_cid_to_tc).classkind in
				if ck = ConfClass
				then ()
				else type_err "parameters declarations are only allowed \
				in the configuration class";
				match f.assigns with
				| [NewInit _] | [AtInit _]
				-> ()
				| _ -> type_err "Parameters must be initialized \
				when declared"
			) else if fin then (
				match f.assigns with
				| [NewCons _] | [AtCons _] | [AtInit _] | [NewInit _]
				-> ()
				| _ -> type_err "Final fields must be initialized \
				once either when declared or within a \
				constructor"
			) else ()
	| SigDecl s ->
			set_lbl s.sig_pos;
			(* If datas are of reference type the reference type cannot be
			* expanded
			*)
			( match dcl.signature.rt with
				| Typ(_,[t]) when t <> Null && t <> Any ->
						let cid = type2id t in
						let ast = i_ast cid in
						ast.status <- Multiple
				| _ -> ()
			);
			let e1() = type_err "Signals must be created or assigned to either \
			when declared or in a constructor (not in both)"
			and e2() = type_err "A callback object must either be a new object \
			or an expandable field.\nMail to \
			axel.poigne@iais.fraunhofer.de \
			if this restriction is too severe."
			in
			( match s.sig_assigns with
				| [NewCons e] | [AtCons e] | [NewInit e] ->
						( match e.expr with
							| New (_,[cb]) ->
									( match cb.expr with
										| New _ ->
												()
										| Call(cb, None) ->
												( try let dcl = Decl.get Rhs cb (- 1) in
													if can_expand dcl
													then ()
													else e2()
												with Not_found ->
														() (* must be a formal param *)
												)
										| _ -> e2()
									)
							| New (_,[]) -> ()
							| Call(_, None) -> ()
							| _ -> e1()
						)
				| _ -> e1()
			)
	
	| _ -> ()

(* -------------------------------------------------------------------------
tc_anon_hide symtab dcl ... guarantee that dcl doesn't hide an entry
in symtab
------------------------------------------------------------------------- *)
let tc_anon_hide symtab dcl =
	let key = dcl.name in
	let arity = decl2arity dcl in
	try
		let _ = Hashtbl.find symtab (key, arity) in
		type_err ("Hiding of identifier \""^(mf2str key)^"\" from the \
				context class in an anonymous class is \
				illegal")
	with Not_found ->
			()

(* -------------------------------------------------------------------------
tc_constraint ... typechecks the specification
------------------------------------------------------------------------- *)
let tc_val_constraint r =
	let ti = tc_infer tc.tc_typ_to_tc r.upper in
	if ti = t_time
	then
		match r.upper.expr with
		| Literal(_, s, _) ->
				let u = eval_const_expr_2_int tc.tc_cid_to_tc r.upper in
				r.upper <- { etyp = Some t_time;
					expr = Literal (t_time, s, Some(Int64.of_int u));
					elbl = r.upper.elbl
				}
		| _ ->
				type_err "The time constraint of a signal or field is \
	not a literal of type 'time'."
	else
		type_err "The time constraint of a signal or field is not \
of type 'time'."

let tc_spec = function
	| Assertion(expr, _, exc) ->
			tc.tc_decl <- empty_default_decl id_dont_use;
			tc.tc_reactive <- false;
			tc.tc_signalbus <- false;
			set_lbl expr.elbl;
			tc_expr t_bool expr;
			tc_expr t_int exc;
			ignore (eval_const_expr tc.tc_cid_to_tc exc)
	| ClassPrec _
	| ObjPrec _ ->
			()
	| Comment _ ->
			()
	| Invariant _ ->
			Err.msg (Err.NYI "data invariants not yet implemented")

let tc_axioms = function
	| ValConstraint (_, _, r) ->
			tc_val_constraint r
	| ACtl _
	| Ctl _
	| Ltl _
	| Ptl _ ->
			()

let tc_props = function
	| ValConstraint _ ->
			type_err "Time constraints are not allowed as proposition"
	| ACtl _
	| Ctl _
	| Ltl _
	| Ptl _ ->
			()

(* -------------------------------------------------------------------------
check_precl
check for CLASS precedences whether the precedence list is valid:
- all label exist & are used as labels (i.e. not as fields)
- all fields and methods / constructor exist & are used properly
- the before / after relation between label - lists, fields and
methods / constructor is acyclic
check for OBJECT precedences whether a precedence
- adresses one signal
- the precedences are either reactive objects or this
------------------------------------------------------------------------- *)
let chk_prec_atom declfn = function
	| RhsFieldPrec(f)
	| LhsFieldPrec(f) ->
			let dcl = declfn f (- 1) in
			if is_field dcl
			then ( (* field is OK, label not, see below ... *) )
			else raise (Failure ("Identifier "^(mf2str f)^"is used like \
							a field, but is no field (a label?)"))
	| MethodPrec(f, tl) ->
			let dcl = declfn f (List.length tl) in
			if is_method_or_constructor dcl && not( is_rct_method dcl)
			then ( (* method is OK, constructor unusual ... *) )
			else raise (Failure ("No suitable method of name "^(mf2str f)^
							" found"))
	| LabelPrec ls ->
			( let check_label lbl =
					try
						let _ = i_lbl2pos tc.tc_cid_to_tc lbl in ()
					with _ ->
							raise (Failure ("In a label-list the id "^(lbl2str lbl)^
										" is used, which is no label"))
				in
				List.iter check_label ls
			)

let check_class_precl is_rct =
	let st = (i_ast tc.tc_cid_to_tc).symtab in
	let declfn m a =
		try
			symtab_entry m a st
		with Not_found ->
				raise (Failure ("No suitable method of name "^(mf2str m)^" found"))
	in
	
	let counter = ref 0 in
	let mk_cnt () = incr counter; !counter in
	let tsort = Tsort.init 100
	and hastidt = Hashtbl.create 43 in
	let sign2id s =
		let s =
			match s with
			| RhsFieldPrec _
			| LhsFieldPrec _
			| MethodPrec _ ->
					s
			| LabelPrec ll ->
					(LabelPrec [list_last ll]) in
		try
			Hashtbl.find hastidt s
		with Not_found ->
				let id = mk_cnt() in
				Hashtbl.add hastidt s id;
				id
	in
	
	let rec prec_preL_postLL = function
		| [] -> ()
		| [_] -> ()
		| prel:: postll ->
				List.iter (fun pre -> prec_pre_postLL pre postll) prel;
				prec_preL_postLL postll
	
	and prec_pre_postLL pre postll =
		let pre = sign2id pre in
		let prec pre post = Tsort.set_pre_post tsort pre (sign2id post) in
		List.iter (List.iter (prec pre)) postll
	
	in
	
	let precl = i_class_precl tc.tc_cid_to_tc in
	if precl = [] then (
	(* OK: no precedences to check *)
	) else (
		(* precedences to check *)
		if is_rct then (
		(* OK *)
		) else (
			let txt = "Class precedences only valid in reactive classes" in
			Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
		);
		List.iter prec_preL_postLL precl;
		( match Tsort.sort tsort with
			| Tsort.Sorted _ ->
					()
			| Tsort.Cycle _ ->
					let txt = "Dependency between methods and / or \
					labels is cyclic" in
					Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
			| _ ->
					Err.intern "precedence_valid"
		);
		( try
			let chk_prec_comm l = List.iter (chk_prec_atom declfn) l in
			let chk_prec_entry l = List.iter chk_prec_comm l in
			List.iter chk_prec_entry precl
		with Failure txt ->
				Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
		)
	)

(* ------------------------------------------------------------------------- *)
let get_decl f st =
	try
		match Hashtbl.find st (f,- 1) with
		| dcl:: _ -> dcl
		| _ -> Err.intern "get_decl:objprec"
	with _ ->
			let txt = "Unknown identifier "^mf2str f^" used \
			in an object precedence declaration" in
			Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))

let rec chk_dcl ~expect_sig st = function
	| [] -> ()
	| ro:: rol ->
			if List.mem ro rol then (
				let txt = callthis2str ro^" used more than once in a \
				object precedence declaration" in
				Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
			) else (
			(* OK *)
			);
			( match ro with
				| This ->
						()
				| Call(f, None) ->
						let dcl = get_decl f st in
						if expect_sig then (
							(* f's type must be a signal *)
							if is_signal dcl then (
							(* OK *)
							) else (
								let txt = mf2str f^" is not of signal type" in
								Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
							)
						) else (
							(* f's type must be based on a reactive class *)
							if is_reactive_type dcl.signature.rt then (
							(* OK *)
							) else (
								let txt = mf2str f^" is no instance of a \
								reactive class" in
								Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
							)
						)
				| _ -> Err.intern "chk_dcl"
			);
			chk_dcl expect_sig st rol

let rec check_obj_prec st = function
	| [] -> ()
	| (sign, rol):: precl ->
			chk_dcl ~expect_sig: true st [(Call(sign, None))];
			chk_dcl ~expect_sig: false st rol;
			if List.exists (fun (s, _) -> s = sign) precl
			then let txt = "Signal "^mf2str sign^" has more than one \
				object precedence" in
				Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
			else ();
			check_obj_prec st precl

let check_obj_precl is_rct =
	let precl = i_obj_precl tc.tc_cid_to_tc in
	if precl = [] then (
	(* OK: no precedences to check *)
	) else (
		if is_rct then (
		(* OK *)
		) else (
			let txt = "Object precedences only valid in reactive classes" in
			Err.msg (Err.InvalidPrec(tc.tc_cid_to_tc, txt))
		);
		check_obj_prec (i_ast tc.tc_cid_to_tc).symtab precl
	)

(* -------------------------------------------------------------------------
make cid
TYPECHECK THE CLASS cid
------------------------------------------------------------------------- *)
let rec make cid =
	ignore(i_guarantee_status cid InheritChecked);
	if class_status cid (>=) TypeChecked then (
	(* already done *)
	) else (
		let ast = i_ast cid in
		let imp = List.map type2id ast.implements in
		let inh = if ast.extends = Any
			then imp
			else (type2id ast.extends):: imp in
		List.iter (fun cid -> if class_status cid (<) TypeChecked
						then make cid
						else ()
			) inh;
		( match ast.classkind with
			| Anonymous ctxt ->
					let ifc = match ast.implements with
						| [t] -> type2id t
						| _ -> Err.intern "Typecheck.make:ifc"
					in
					( match (i_ast ifc).classkind with
						| Interface -> ()
						| _ -> Err.msg (Err.AnonymousOnlyIfc(ctxt, ifc))
					);
					make ctxt
			| _ -> ()
		);
		tc.tc_cid_to_tc <- cid;  (* must be set: constant & accumulated data *)
		tc.tc_outer_cid <-
		( match ast.classkind with
			| Anonymous ctxt -> ctxt
			| _ -> cid
		);
		tc.tc_typ_to_tc <- Typ(cid, typevarl2constraint ast);
		tc.tc_arraylits <- [];
		tc.tc_arraylit_ct <- 1;
		tc.tc_sig_maps <- [];
		tc.tc_instmthd <- [];
		tc.tc_classmthd <- [];
		tc.tc_arraylit_ct <- 1;
		tc.tc_sig_maps <- [];
		tc.tc_instmthd <- [];
		tc.tc_classmthd <- [];
		tc.tc_newflag <- false;
		tc.tc_dyn_new <- [];
		tc.tc_new <- [];
		tc.tc_callgraph <- [];
		
		tc.tc_decl <- empty_default_decl id_dont_use;
		tc.tc_reactive <- false;
		tc.tc_signalbus <- false;
		tc.tc_flowcontext <- false;
		
		let is_rct = is_reactive_classkind ast.classkind in
		
		tc.tc_symtab <- Hashtbl.create 23;          (* comment line 14 *)
		List.iter (tc_step1 is_rct) ast.declseq;     (* check fields *)
		( match ast.classkind with
			| Anonymous ccid ->
					let cast = i_ast ccid in
					List.iter (tc_anon_hide cast.symtab)
						ast.declseq;
					tc.tc_symtab <- cast.symtab
			| _ ->
					tc.tc_symtab <- ast.symtab
		);
		List.iter (tc_step2 is_rct) ast.declseq;             (*   ... methods *)
		
		tc.tc_decl <- empty_default_decl id_dont_use;
		tc.tc_reactive <- false;
		tc.tc_signalbus <- false;
		tc.tc_flowcontext <- false;
		(* ... MTHDS/FLDS good def'd *)
		( let cstrl = match ast.classkind with
				| Anonymous _ |
				Interface ->
						[] (* no constructor *)
				| _ ->
						let condense v = match v with
							| { entry = Constructor c } -> Some c
							| _ -> None
						in
						list_condense condense ast.declseq
			in
			List.iter (tc_step4 cstrl) ast.declseq
		);
		set_lbl Ly.nolbl;
		check_class_precl is_rct;       (* class & object precedence list OK *)
		check_obj_precl is_rct;
		
		List.iter tc_spec ast.specs;
		List.iter tc_axioms ast.axioms;
		if debug_level DbgAssign then (
			ps "\nusage of fields of class "; psc cid; ps ":";
			List.iter dbg_usage ast.declseq; ps "\n"
		);
		set_class_sta cid TypeChecked;
		set_class_use
			cid
			tc.tc_instmthd
			tc.tc_new tc.tc_dyn_new
			tc.tc_classmthd
			tc.tc_arraylits
	)
