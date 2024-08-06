open Ly
open Ast
open Util
open P
open Util_print

let lower a = a >= 97 && a <= 122
let upper a = a >= 65 && a <= 90
let idsuf a = ( a >= 97 && a <= 122) || ( a = 95 ) || ( a >= 48 && a <= 57 ) ||
	( a >= 65 && a <= 90 )
let digit a = ( a >= 48 && a <= 57 )
let hex a = ( a >= 48 && a <= 57 ) || ( a >= 97 && a <= 102 )
let bit a = ( a = 48 ) || ( a = 49 )

let prs_err s = raise (Parse s)

let lit2int (t, _, v) =
	let v =
		match v with
		| Some v -> v
		| _ -> prs_err "literal invalid for conversion to int" in
	if (Int64.compare v (Int64.of_int max_int)) <= 0
	then Int64.to_int v
	else prs_err "literal too large for conversion to int"

let lit2int32 (t, _, v) =
	let v =
		match v with
		| Some v -> v
		| _ -> prs_err "literal invalid for conversion to int32" in
	if (Int64.compare v (Int64.of_int32 Int32.max_int)) <= 0
	then Int64.to_int32 v
	else prs_err "literal too large for conversion to int32"

let lit2string (t, s, _) =
	if (t = t_string)
	then s
	else prs_err "string literal is required here"

(* -------------------------------------------------------------------------
"tparser_persistent_data" is the type of a data structure for the parser.
There exist ONE OBJECT "s" of this type. It is IMPLICIT PARAMETER of all
functions of file yacc.mly and util_parse.ml. Data is only valid during
parsing.
------------------------------------------------------------------------- *)
type tparser_persistent_data =
	{ mutable p_kind : t_kind;
    mutable p_parsing_graphic : bool;
		mutable p_text_file : string;
		mutable p_graphic_file : string;
		mutable p_graphic_stms : tgraphicstm list;
		mutable p_goid : int;             (* number of GO for position info *)
		mutable p_blockparse_started : bool;
		mutable p_class_kind : tclass_kind option;
		mutable p_class_id : tclass;
		mutable p_extends : ttype;
		mutable p_implements : ttype list;
		mutable p_typparams : ttypparams list;
		mutable p_symbol_table : tdecl_table;
		mutable p_specs : tspec list;
		mutable p_axioms : trctspec list;
		mutable p_props : trctspec list;
		mutable p_anonseq : (tlbl2sp -> tast) list;
		mutable p_declseq : tdecl_entry list;
		mutable p_imports : (tclass, tclass) Hashtbl.t;
		mutable p_method_kind : tmethod_kind; (* Constructor ~~~ DataMethod *)
		mutable p_active : bool;
		mutable p_node_param : bool;
	}

let s =
	{ p_kind = ClassKind;
    p_parsing_graphic = false;
		p_text_file ="";
		p_graphic_stms =[];
		p_graphic_file ="";
		p_goid = 0;
		p_blockparse_started = false;
		p_class_kind = None;
		p_class_id = id_object;
		p_extends = t_object;
		p_implements =[];
		p_typparams =[];
		p_specs =[];
		p_axioms =[];
		p_props =[];
		p_anonseq =[];
		p_symbol_table = Hashtbl.create 23;
		p_declseq =[];
		p_imports = Hashtbl.create 7;
		p_method_kind = BttmMethod;
		p_active = false;
		p_node_param = false;
	}

(* -------------------------------------------------------------------------
evaluate constant int expressions
similar to bs_const_expr but before typechecking
------------------------------------------------------------------------- *)
let rec prs_const_expr expr =
	match expr.expr with
	| Dot(e1, e2) ->
			let v1 = prs_const_expr e1 in
			prs_const_op v1 e2
	| Call(fm, None) ->
			fm2const fm
	| _ ->
			prs_literal expr.expr

and prs_literal = function
	| Literal (t, s, Some i) -> i
	| _ -> raise Not_found

and prs_const_op vlhs erhs =
	match erhs.expr with
	| Call(o, Some [e]) ->
			let vrhs = prs_const_expr e in
			if o = id_op_add then Int64.add vlhs vrhs else
			if o = id_op_sub then Int64.sub vlhs vrhs else
			if o = id_op_mult then Int64.mul vlhs vrhs else
			if o = id_op_div then Int64.div vlhs vrhs else
				raise Not_found
	| Call(o, Some []) ->
			if o = id_op_minus
			then Int64.neg vlhs
			else raise Not_found
	| _ -> raise Not_found

and fm2const fm =
	let err() = prs_err "This expression should be a constant \
	integral expression, but contains illegal \
	(variable) components\n" in
	let dcls = Hashtbl.find s.p_symbol_table (fm,- 1) in
	let dcl =
		match dcls with
		| dcl:: _ -> dcl
		| _ -> err()
	in
	if dcl.signature.rt = t_int then (
		match dcl.entry with
		| Field f ->
				( match f.init with
					| None -> err()
					| Some e ->
							( try prs_const_expr e
							with Not_found ->
									prs_err "This variables is not initialized \
							by a constant expression.\n"
							)
				)
		| _ -> err()
	) else (
		prs_err "This variable is not of integral type.\n"
	)

let rec prs_expr2const e =
	match e.expr with
	| Literal(t, s, Some i) ->
			i
	| Dot(e1, e2) ->
			let v1 = prs_expr2const e1 in
			prs_const_op v1 e2
	| Call(fm, None) ->
			( try fm2const fm
			with Not_found ->
					prs_err "This expression should be a constant integral \
			expression, but contains illegal (variable) components.\n"
			)
	| _ -> prs_err "This expression contains some subexpression that is \
not allowed in a constant integral expression.\n"
and prs_const_op vlhs erhs =
	match erhs.expr with
	| Call(o, Some [e]) ->
			let vrhs = prs_expr2const e in
			if o = id_op_add then Int64.add vlhs vrhs else
			if o = id_op_sub then Int64.sub vlhs vrhs else
			if o = id_op_mult then Int64.mul vlhs vrhs else
			if o = id_op_div then Int64.div vlhs vrhs else
				prs_err "This expression contains some operator \
	that is not admissable in a constant integral \
	expression.\n"
	| Call(o, Some []) ->
			if o = id_op_minus
			then Int64.neg vlhs
			else prs_err "This expression contains some \
	operator which is not admissable in a constant \
	integral expression.\n"
	| _ -> prs_err "This expression contains some subexpression that is \
not allowed in a constant integral expression.\n"

let expr2dim e =
	if s.p_node_param then (
		match e.expr with
		| Call(fm, None) ->
				DimVar fm
		| _ ->
				prs_err "The dimension of a signal parameter of \
		vector or matrix type must be a variable"
	) else (
		let d = int64_2_int(prs_expr2const e) in
		if d > 0
		then DimLen d
		else prs_err "vector/matrix index is not greater or  equal to 0.\n"
	)

let expr2index e =
	try
		let v = prs_const_expr e in
		let v' = i2s (int64_2_int v) in
		{ e with expr = Literal(t_int, v', Some v) }
	with Not_found
	| Parse _ -> prs_err "The index of a vector or matrix is not well \
specified. It should either by an integral value \
or an integral constant.\n"

(* -------------------------------------------------------------------------
initialize parse - data for a FILE and a CLASS within a file and for graphic
------------------------------------------------------------------------- *)
let init_parse_of_file file =
	s.p_text_file <- file;
	s.p_graphic_file <- "";
	s.p_parsing_graphic <- false;
	Hashtbl.clear s.p_imports;
	Ly.clr_pos2lc ()

let init_parse_of_class () =
	s.p_class_kind <- None;
	s.p_class_id <- id_object;
	s.p_extends <- t_object;
	s.p_implements <- [];
	s.p_typparams <- [];
	s.p_specs <- [];
	s.p_axioms <- [];
	s.p_props <- [];
	s.p_anonseq <- [];
	s.p_symbol_table <- Hashtbl.create 23;
	s.p_declseq <- [];
	s.p_graphic_stms <- [];
	ignore (Ly.clr_and_return_lbls ())

(* ------------------------------------------------------------------------- *)
let init_parse_graphic file =
	s.p_parsing_graphic <- true;
	s.p_graphic_file <- file;
	s.p_goid <- 0;
	s.p_method_kind <- (RctMethod NotAvailable);
	s.p_active <- false;
	Ly.clr_pos2lc ()

let exit_parse_graphic () =
	s.p_parsing_graphic <- false;
	s.p_graphic_file <- "";
	s.p_method_kind <- BttmMethod;
	s.p_active <- false

(* -------------------------------------------------------------------------
parsing data related to methods store the kind of method resp. whether
an active part was found. This data must be saved and restored if
anonymous classes are parsed: method def's occur in method def's
------------------------------------------------------------------------- *)
let methd_stack = ref []

let push_meth_data () =
	methd_stack := (s.p_method_kind, s.p_active) :: !methd_stack;
	s.p_method_kind <- BttmMethod;
	s.p_active <- false

let pop_meth_data () =
	match !methd_stack with
	| [] -> Err.intern "methd_stack"
	| (mk, ac):: ms -> s.p_method_kind <- mk;
			s.p_active <- ac;
			methd_stack := ms

(* -------------------------------------------------------------------------
(optional) initialization of fields
------------------------------------------------------------------------- *)
type tmfinit =
	| NoInit
	| ExprInit of texpr

type tmfname =           (* initialization of fields *)
	{ mf_name : tmfid;
		mf_init : tmfinit
	}

(* -------------------------------------------------------------------------
guarantee, that the class kind is promoted to
- at least RctClass [ConfClass is ok]
- ConfClass
- at least EffectiveClass [FinalClass or AbstractClass is ok]
------------------------------------------------------------------------- *)
let enforce_rctclass t =
	match s.p_class_kind with
	| None -> ps " [reactive]"; s.p_class_kind <- Some RctClass
	| Some RctClass
	| Some ConfClass -> ()
	| _ -> prs_err t

let enforce_confclass t =
	match s.p_class_kind with
	| None
	| Some RctClass -> ps " [configuration]"; s.p_class_kind <- Some ConfClass
	| Some ConfClass -> ()
	| _ -> prs_err t

let enforce_dataclass t =
	match s.p_class_kind with
	| None ->
			ps " [data]";
			s.p_class_kind <- Some EffectiveClass
	| Some EffectiveClass
	| Some FinalClass
	| Some AbstractClass ->
			()
	| _ -> prs_err t

(* -------------------------------------------------------------------------
the kind of a method must be inferred, which is problematic.
As a first step, reactive stmts enforce RctMethod
data stmts enforce DataMethod
For a Abstract Internal or Native method DataMethod is enforced, too.
Methods which stay in an unknown kind contain bivalent stmts only,
like if(...) {...} else {...} or plain method calls like m()
A call graph has to be built for all methods to decide their status.
The call graph is used to decide about acyclicity of RctMethod calls, too.
This is done later.
------------------------------------------------------------------------- *)
let enforce_rctmthd_else_err cls mthd =
	enforce_rctclass cls;
	if s.p_active then (
	(* active implicitly guarantees reactive *)
	) else (
		match s.p_method_kind with
		| RctMethod _
		| GraphicCall _
		| RctNode _ -> ()
		| DataMethod -> prs_err mthd
		| Abstract
		| NativeMethod _
		| NativeIntr _ -> Err.intern "enforce_rctmthd_else_err"
		| BttmMethod -> s.p_method_kind <- (RctMethod NotAvailable)
		| Internal -> () (* used for parsing clock expressions *)
	)

let has_reactive_cntxt () =
	match s.p_method_kind with
	| BttmMethod | RctMethod _
	| GraphicCall _
	| RctNode _ -> true
	| _ -> false

let enforce_rctmthd () =
	enforce_rctmthd_else_err
	(* 1. *) "A reactive method is illegal in a non reactive class"
		(* 2. *) "A method has parts which are restricted to occur in data \
methods (e.g. a while loop) and others restricted to occur \
in reactive methods (e.g. an emit statement or a signal as \
formal parameter. But a method \
must either be a data or a reactive method.\n\
This conflict was detected at: "

let enforce_datamthd_else err =
	let err () = prs_err err in
	if s.p_active then (
		err ()
	) else (
		match s.p_method_kind with
		| Internal
		| RctMethod _
		| GraphicCall _
		| RctNode _ -> err ()
		| DataMethod
		| Abstract
		| NativeMethod _
		| NativeIntr _ -> ()
		| BttmMethod -> s.p_method_kind <- DataMethod
	)

let enforce_datamthd () =
	let err = "This is a data statement, e.g. a while loop, which is invalid
	in a reactive context."
	in
	enforce_datamthd_else err

(* -------------------------------------------------------------------------
makes a source - position identification for parsing, debugging, simulation
------------------------------------------------------------------------- *)
type srcp_t = PosEmit | PosHalt | PosText

let mk_srcp ?(kind = PosText) ?(n = 0) () =
	let f, t =
		match kind with
		| PosText -> Parsing.symbol_start (), Parsing.symbol_end ()
		| PosEmit
		| PosHalt -> Parsing.rhs_start n, Parsing.rhs_end n in
	let (fl, fc) = Ly.pos2lc f
	and (tl, tc) = Ly.pos2lc t in
	let tc = if tc <= 1 then 1 else
		if tc <= fc && fl = tl then tc else
			tc - 1 in
	let f, go = if s.p_parsing_graphic
		then s.p_graphic_file, s.p_goid
		else s.p_text_file, 0 in
	match kind with
	| PosText -> TextPos(f, go, fl, fc, tl, tc)
	| PosEmit -> EmitPos(f, go, fl, fc, tl, tc)
	| PosHalt -> HaltPos(f, go, fl, fc, tl, tc)

let add_xst_lbl ?(kind = PosText) ?(n = 0) lbl =
	let sp = mk_srcp ~kind: kind ~n: n () in
	Ly.ext_lbl lbl sp;
	lbl

let add_new_lbl ?(kind = PosText) ?(n = 0) () =
	let sp = mk_srcp ~kind: kind ~n: n () in
	Ly.int_lbl sp

let add_sp ?(kind = PosText) ?(n = 0) =
	function
	| None -> add_new_lbl ~kind: kind ~n: n ()
	| Some lbl -> add_xst_lbl ~kind: kind ~n: n lbl

(* let add_sp_item knd n =
let mk_srcp n =
let f = Parsing.rhs_start n
and t = Parsing.rhs_end n in
let (fl, fc) = Ly.pos2lc f
and (tl, tc) = Ly.pos2lc t in
let tc = if tc <= 1 then
1
else if tc <= fc && fl = tl then
tc
else
tc - 1 in
let go = if s.p_parsing_graphic then s.p_goid else 0 in
match knd with
| PosEmit -> EmitPos(s.p_text_file, go, fl, fc, tl, tc)
| PosHalt -> HaltPos(s.p_text_file, go, fl, fc, tl, tc)
in
let add_xst_lbl n lbl =
let sp = mk_srcp n in
Ly.ext_lbl lbl sp;
lbl
in
le tadd_new_lbl n =
let sp = mk_srcp n in
Ly.int_lbl sp
in
function
| None -> add_new_lbl n
| Some lbl -> add_xst_lbl n lbl
*)

let add_emit_sp n = add_sp ~kind: PosEmit ~n: n
let add_halt_sp n = add_sp ~kind: PosHalt ~n: n

(* -------------------------------------------------------------------------
process IMPORTS
------------------------------------------------------------------------- *)
let import2hashtbl cid full_cid =
	try
		let _ = Hashtbl.find s.p_imports cid in
		prs_err ("Import has class name conflict for: "^(c2str cid))
	with _ ->
			Hashtbl.add s.p_imports cid full_cid

let import_class idl cid =
	let full_cid =
		(String.concat "." (List.map mf2str idl))^
		(if idl =[] then "" else ".")^(c2str cid) in
	let full_cid = c2id full_cid in
	if debug_level DbgImport
	then ( ps "\nimport "; psc full_cid; ps " as "; psc cid );
	import2hashtbl cid full_cid

let rec import_from opn pre =
	let file = Unix.readdir opn in
	if Filename.check_suffix file se.file_suffix then (
		let cid = c2id (Filename.chop_suffix file se.file_suffix) in
		let full_cid = c2id (pre^(c2str cid)) in
		if debug_level DbgImport then (
			ps "\n  "; psc full_cid; ps " as "; psc cid
		) else (
		(* no debug *)
		);
		import2hashtbl cid full_cid
	) else (
		prs_err ("Invalid file-/classname imported: "^file)
	)

let import_all idl =
	let stl = List.map mf2str idl in
	let pre = (String.concat "." stl)^(if idl =[] then "" else ".") in
	let dir = List.fold_left Filename.concat Filename.current_dir_name stl in
	if debug_level DbgImport
	then ( ps "\nimport all from directory \""; ps dir; ps "\"" );
	try
		let opn = Unix.opendir dir in
		try
			while (true) do
				import_from opn pre
			done
		with End_of_file ->
				Unix.closedir opn;
				if debug_level DbgImport
				then ( ps "\nimport all finished" )
	with
	| Parse(t) ->
			raise (Parse t)
	| _ ->
			prs_err ("Invalid \"import "^pre^"*\"
					(directory not accessible?)")

(* -------------------------------------------------------------------------
process the state actions ENTRY DURING EXIT DO
------------------------------------------------------------------------- *)
type tstatemodif =
	{ mutable m_entry : tstmtl option;
		mutable m_during : tstmtl option;
		mutable m_exit : tstmtl option;
		mutable m_do : tstmtl option;
	}

let stact_init () = { m_entry = None; m_during = None; m_exit = None; m_do = None }

let mk_stact x = function
	| None -> Some x
	| _ -> prs_err "state actions are invalid"

let get_stact dflt = function
	| None -> dflt
	| Some b -> b

let mk_stact_entry m x = m.m_entry <- mk_stact x m.m_entry
let mk_stact_during m x = m.m_during <- mk_stact x m.m_during
let mk_stact_exit m x = m.m_exit <- mk_stact x m.m_exit
let mk_stact_do m x = m.m_do <- mk_stact x m.m_do

let stact_entry mr = get_stact [] mr.m_entry
let stact_during mr = get_stact [] mr.m_during
let stact_exit mr = get_stact [] mr.m_exit
let stact_do mr = get_stact [] mr.m_do

(* -------------------------------------------------------------------------
process the declaration and the signal modifier
------------------------------------------------------------------------- *)
type tnative =
	| SameId
	| NewId of string

type tmodif =
	{ mutable mStatic : tscope option;
		mutable mVolatile : bool option;
		mutable mFinal : bool option;
		mutable mParameter : bool option;
		mutable mReactive : bool option;
		mutable mAbstract : bool option;
		mutable mAccess : taccess option;
		mutable mInterrupt : string option;
		mutable mFromC : tnative option;
		mutable mToC : tnative option;
	}

let m_init () = { mStatic = None;
	mVolatile = None;
	mFinal = None;
	mParameter = None;
	mReactive = None;
	mAbstract = None;
	mAccess = None;
	mInterrupt = None;
	mFromC = None;
	mToC = None;
}

let mk_rcd x = function
	| None -> Some x
	| _ -> prs_err "modifier is invalid"

let used = function
	| None -> false
	| _ -> true

let mk_static m x = m.mStatic <- mk_rcd x m.mStatic
let mk_volatile m x = m.mVolatile <- mk_rcd x m.mVolatile
let mk_final m x = m.mFinal <- mk_rcd x m.mFinal
let mk_parameter m x = m.mParameter <- mk_rcd x m.mParameter
let mk_reactive m x = m.mReactive <- mk_rcd x m.mReactive
let mk_abstract m x = m.mAbstract <- mk_rcd x m.mAbstract
let mk_access m x = m.mAccess <- mk_rcd x m.mAccess
let mk_interrupt m x = m.mInterrupt <- mk_rcd x m.mInterrupt
let mk_from_C m x = m.mFromC <- mk_rcd x m.mFromC
let mk_to_C m x = m.mToC <- mk_rcd x m.mToC

let get_rcd dflt = function
	| None -> dflt
	| Some b -> b

let getStatic mr = get_rcd Instance mr.mStatic
let getVolatile mr = get_rcd false mr.mVolatile
let getFinal mr = get_rcd false mr.mFinal
let getParameter mr = get_rcd false mr.mParameter
let getReactive mr = get_rcd false mr.mReactive
let getAbstract mr = get_rcd false mr.mAbstract
let getAccess mr = get_rcd Private mr.mAccess
let getInterrupt mr = get_rcd "" mr.mInterrupt
let getFromC mr = get_rcd SameId mr.mFromC
let getToC mr = get_rcd SameId mr.mToC

(* -------------------------------------------------------------------------
SIMPLE_IN_PAREN returns a simple cid, for casts this has to be converted
------------------------------------------------------------------------- *)
let simple2cast cid =
	if cid = id_bool then id_op_to_bool else
	if cid = id_char then id_op_to_char else
	if cid = id_byte then id_op_to_byte else
	if cid = id_short then id_op_to_short else
	if cid = id_uint16 then id_op_to_uint16 else
	if cid = id_int then id_op_to_int else
	if cid = id_uint32 then id_op_to_uint32 else
	if cid = id_long then id_op_to_long else
	if cid = id_uint64 then id_op_to_uint64 else
	if cid = id_float then id_op_to_float else
	if cid = id_double then id_op_to_double else
	if cid = id_string then id_op_to_string else
		Err.intern "simple2cast"

(* -------------------------------------------------------------------------
cmd_... AUXILIARY FUNCTIONS FOR COMMAND FILE PROCESSING
------------------------------------------------------------------------- *)
let cmd_int = function
	| CmdInt i -> i
	| _ -> raise Not_found

let cmd_string = function
	| CmdString s -> s
	| _ -> raise Not_found

let cmd_prtsize = function
	| CmdString ("a4" as s) -> s
	| CmdString ("a5" as s) -> s
	| _ -> raise Not_found

let font_weight (f, s, w) nw =
	let nw = cmd_string nw in
	match nw with
	| "bold" | "normal" -> (f, s, nw)
	| _ -> raise Not_found

let font_size (f, s, w) ns =
	let ns = cmd_int ns in
	if ns >= 6 && ns <= 102
	then (f, i2s ns, w)
	else raise Not_found

let cmd_fprefix str =
	let s = cmd_string str in
	let l = String.length s in
	let last = String.get s (l - 1) in
	if l = 0
	then "se."
	else if not(last = '.')
	then (s^".")
	else if (l = 1)
	then "se."
	else s

let cmd_conf str =
	let str = cmd_string str in
	if str = ""
	then Null
	else (Typ(c2id str,[]))

let cmd_wdwplace = function
	| CmdString "user" -> true
	| CmdString "wm" -> false
	| _ -> raise Not_found

let cmd_bool = function
	| CmdString "true" -> true
	| CmdString "false" -> false
	| _ -> raise Not_found

let cmd_ttime = function
	| CmdString "left" -> false
	| CmdString "right" -> true
	| _ -> raise Not_found

(* -------------------------------------------------------------------------
adds an entry to the symboltable of a class
Only trivial error - checking is done, complete error - checking is done later,
when the class gets the status ''Parsed''.

>> overloading is restricted to different number of parameters,
>> fields and parameterless methods are distinguished
------------------------------------------------------------------------- *)
let pure_st_entry st dcl =
	let key = dcl.name in
	let arity = decl2arity dcl in
	try
		let _ = Hashtbl.find st (key, arity) in
		prs_err ("Overloading of identifier '"^(mf2str key)^"' \
				in class '"^(c2str s.p_class_id)^"' illegal: \
				the number of parameters must differ")
	with Not_found ->
			Hashtbl.add st (key, arity) [dcl];
			if is_constructor dcl then (
				let cl = try Hashtbl.find st (key,- 2)
					with Not_found -> [] in
				Hashtbl.remove st (key,- 2);
				Hashtbl.add st (key,- 2) (dcl:: cl)
			) else ()

let bs_st_entry st dcl =
	s.p_declseq <- dcl :: s.p_declseq;
	pure_st_entry st dcl

let mkp_st_entry dcl =
	bs_st_entry s.p_symbol_table dcl

(* -------------------------------------------------------------------------
process type parameter
------------------------------------------------------------------------- *)
let add_typeparm name constrain =
	if List.exists
		( fun (t, _) -> t = name
		) s.p_typparams then (
		prs_err ("type parameter name "^(c2str name)^" not unique")
	) else (
		s.p_typparams <- (name, constrain):: s.p_typparams
	)

let check_typeparm = function
	| (t, Some c) ->
			if List.exists
				( fun (t, _) -> t = c
				) s.p_typparams then (
				prs_err ("type parameter name invalid as constraint \
						for type parameter "^(c2str t))
			) else ()
	| (_, None) -> ()

let class2typ cid atl =
	if List.exists (fun (t, _) -> t = cid) s.p_typparams
	then if atl = []
		then TypeVar(cid)
		else prs_err "type parameter may not have type parameters"
	else Typ(cid, atl)

(* -------------------------------------------------------------------------
reverse a statement list AND re - arrange the statement - list w.r.t. to
LOCAL decls [cannot be done in yacc without dozens of conflicts):
the first LOCAL in a statement - list becomes the last statement in the list,
all statements following the LOCAL are moved into the scope of the LOCAL.
I.e. are moved to the letil - component of the LetStmt - record.
------------------------------------------------------------------------- *)
let rec bs_rlf l1 l2 =
	match l1 with
	| [] -> l2
	| a :: l ->
			match a with
			| LetStmt (_, s) when s.letil =[] ->
					s.letil <- l2; bs_rlf l [a]
			| _ ->
					bs_rlf l (a:: l2)

let reverse_and_letify l = bs_rlf l []

(* -------------------------------------------------------------------------
check the consistency of transitions
------------------------------------------------------------------------- *)
let trans_err t = prs_err ("invalid automaton/synERJYchart: "^t)

let rec bchk_stmtl total acc = function
	| [] ->
			if total
			then trans_err "else required"
			else acc
	| [Else(_, stmtl)] ->
			bchk_stmt acc stmtl
	| [Then(_, e, stmtl)] ->
			if total
			then trans_err "else required"
			else bchk_stmt acc stmtl
	| Then(_, e, stmtl):: thenl ->
			let acc = bchk_stmt acc stmtl in
			bchk_stmtl total acc thenl
	| Else(_, _):: _ ->
			Err.intern "bchk_stmtl"

and bchk_stmt acc = function
	| [] -> trans_err "last statement must be \
	next state or if"
	| [NextState(_, state_id)] ->
			add_sosgl state_id acc
	| [ExprStmt(_,{ expr = If(thenl) })] ->
			bchk_stmtl true acc thenl
	| _:: stmtl ->
			bchk_stmt acc stmtl

let chk_when total stmtl =
	bchk_stmtl total [] stmtl

let init2nextstates automaton =
	bchk_stmtl true [] automaton.a_init.strans

let is_state_cyclic state =
	let nexts = bchk_stmtl false [] state.strans in
	mem_sosgl state.sname nexts

(* -------------------------------------------------------------------------
make an expression
------------------------------------------------------------------------- *)
let mk_e e = { etyp = None; expr = e; elbl = add_new_lbl () }
let mk_edot e1 e2 = mk_e (Dot(e1, e2))
let mk_ecall n p = mk_e (Call (n, p))
let mk_e_inf n e1 e2 = mk_edot e1 (mk_ecall n (Some [e2]))
let mk_e_pre n e1 = mk_edot e1 (mk_ecall n (Some []))
let mk_e_fld n e1 = mk_edot e1 (mk_ecall n None)
let mk_bssig b m = mk_edot (mk_e b) (mk_ecall m (Some []))
let mk_esig s m = mk_edot (mk_ecall s None) (mk_ecall m (Some []))

let rec mk_e_assign e1 op e2 =
	match e1.expr with
	| Dot({ expr = Call(i, None) },{ expr = Call(v, Some [p1]) }) when v = id_op_get ->
			mk_e (Assign(i, ADim1 p1, op, e2))
	| Dot({ expr = Call(i, None) },{ expr = Call(v, Some [p1; p2]) }) when v = id_op_get ->
			mk_e (Assign(i, ADim2(p1, p2), op, e2))
	| Dot({ expr = This }, e1) ->
			mk_e_assign e1 op e2
	| Dot({ expr = ClassCall(cid, fid, None) },{ expr = Call(v, Some [p1]) }) when v = id_op_get ->
			if cid = s.p_class_id
			then mk_e (Assign(fid, ADim1 p1, op, e2))
			else prs_err "error in assignment to static"
	| Dot({ expr = ClassCall(cid, fid, None) },{ expr = Call(v, Some [p1; p2]) })	when v = id_op_get ->
			if cid = s.p_class_id
			then mk_e (Assign(fid, ADim2(p1, p2), op, e2))
			else prs_err "error in assignment to static"
	| Dot _ ->
			prs_err "error in assignment (field access to an \
	object other than \'this\' on the left hand side"
	| Call(i, None) ->
			mk_e (Assign(i, NoArr, op, e2))
	| ClassCall(cid, fid, None) ->
			if cid = s.p_class_id
			then mk_e (Assign(fid, NoArr, op, e2))
			else prs_err "error in assignment to static"
	| _ ->
			prs_err "error in assignment"

let mk_extended_lit cid lit =
	match lit with
	| Literal(t, s, v) ->
			let tid = type2id t in
			if is_simple_subtype ~sub: tid ~super: cid
			then mk_e (Literal (Simple cid, s, v))
			else prs_err "the literal to be casted exceeds the limits of \
	the target type. Store the literal into a \
	variable and cast the variable, if you really \
	want the truncation to happen"
	| _ -> prs_err "this kind of literal cast is not supported"

(* -------------------------------------------------------------------------
make an array literal. The state parameter encodes, whether an 1 - dim or an
2 - dim - type is possible: - 1 = dont know [initial state]
0 = only 1 - dim array possible
> 0 = only 2 - dim array possible, the int value is the
length of the sub - array [all must have the
same length]
------------------------------------------------------------------------- *)
let mk_array_err i = prs_err ("array literal is invalid ("^i2s i^")")

let chk_arraylit e =
	match e.expr with
	| ArrayLiteral (Dim1 l) -> (true, l.av1, List.length l.av1)
	| _ -> (false,[], 0)

let rec chk_arraylit1 = function
	| [] -> ()
	| e:: el ->
			let (is_arraylit, _, _) = chk_arraylit e in
			if is_arraylit
			then mk_array_err 1
			else chk_arraylit1 el

let rec mk_arraylit1 e =
	match e.expr with
	| Literal _ ->
			e
	| Call(fm, None) ->
			( try let v = fm2const fm in
				let v' = i2s (int64_2_int v) in
				{ e with expr = Literal(t_int, v', Some v) }
			with Not_found -> prs_err "this expression should be a constant \
			integral expression, but contains illegal \
			(variable) components.\n"
			)
	| e ->
			mk_array_err 0

let rec mk_arraylit2 len = function
	| [] -> []
	| e:: el ->
			let (is_arraylit, ll, l) = chk_arraylit e in
			if is_arraylit
			then if (l = len) &&
				List.for_all (* sub-literals must be simple lits *)
					(fun e -> let (i, _, _) = chk_arraylit e in (not i))
					ll
				then let ll = List.map mk_arraylit1 ll in
					ll:: (mk_arraylit2 len el)
				else mk_array_err 2
			else mk_array_err 3

let mk_arraylit el =
	(* first check whether we habe an one- or two-dim array literal *)
	let (is_arraylit, _, len) =
		chk_arraylit (
				match el with
				| [] -> mk_array_err 4
				| e:: _ -> e
			)
	in
	mk_e (if is_arraylit
			then ( ArrayLiteral (Dim2 { av2 = (mk_arraylit2 len el); an2 = 0 }) )
			else ( chk_arraylit1 el;
				let el = List.map mk_arraylit1 el in
				ArrayLiteral (Dim1 { av1 = el; an1 = 0 }) )
		)

let array_cast_err =
	"Casts are not supported for arrays. We believe that, if \
needed at all, such cast should be made replaced by casting \
the elements of the array individually. If values are of \
primitive type, the programmer should be aware of the fact that \
the format of all data are changed (at computational and memory \
expense. If the values are of reference type downcast leads to \
a cast exception error anyway and and upcast causes a covariance \
problem invariably leading to a runtime error."

(* -------------------------------------------------------------------------
generate a type list from a formal parameter list
------------------------------------------------------------------------- *)
let frm2typl fpl =
	List.map (function f -> f.p_type) fpl

let check_formal_l = function
	| None -> ()
	| Some fl ->
			let nl = List.map (fun p -> p.p_name) fl in
			let rec check_name_l =
				function
				| [] -> ()
				| h:: t ->
						if (List.mem h t)
						then prs_err ("Identifier \""^(mf2str h)^
									"\"used twice with \
									the same signature")
						else check_name_l t
			in
			check_name_l nl

(* -------------------------------------------------------------------------
make the signame declaration for either a signal field or a local
------------------------------------------------------------------------- *)
let mk_sigdecl lbl sigtyp clock newopt =
	let newopt =
		match newopt with
		| NoInit -> None
		| ExprInit e -> Some e
	in
	{ sig_tmstmp = false; sig_init = newopt; sig_assigns =[];
		sig_clock = clock; sig_pos = lbl;
	}

(* -------------------------------------------------------------------------
process local declarations. This may be:
- signal
- data
------------------------------------------------------------------------- *)
let mk_let_data name typ clock init =
	let def = if is_sensor_or_signal_typ typ then (
			enforce_rctclass "signal declarations are only valid in \
			reactive classes.Reactive classes are \
			recognized by an active - block at the end \
			of their constructor";
			enforce_rctmthd ();
			ConstLocal
		) else (
			MutableLocal
		)
	and l = { p_lbl = add_new_lbl(); p_name = name; p_type = typ; p_clock = clock } in
	{ letent = l; letexpr = init; letdf = def; letil =[] }

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR LABELS
------------------------------------------------------------------------- *)
let mk_label lbl =
	let dcl = { name = lbl; origin = s.p_class_id; entry = Label;
		final = true; access = Private; scope = Instance; volatile = false;
		parameter = false; signature ={ rt = Null; pt = None } } in
	mkp_st_entry dcl

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR SIGNAL DEFINITIONS
- make symbol table entry for the signal
- the callback object, used for in / output
------------------------------------------------------------------------- *)
let mk_sig_entry sigtyp clock { mf_name = name; mf_init = newopt } =
	let lbl = add_new_lbl () in
	let sigdecl = mk_sigdecl lbl sigtyp clock newopt in
	mkp_st_entry { name = name; origin = s.p_class_id; entry = SigDecl sigdecl;
			final = true; scope = Instance; volatile = false; access = Private;
			parameter = false; signature ={ rt = sigtyp; pt = None }
		}

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR CONSTRUCTORS
------------------------------------------------------------------------- *)
let is_simple_assign e =
	match e with
	| ExprStmt(_, e) ->
			( match e.expr with
				| Assign(_, NoArr, op, e) when op = id_op_assign ->
						( match e.expr with
							| Call(_, None) -> true
							| New(_, _) -> true
							| _ -> false
						)
				| _ -> false
			)
	| LetStmt _ -> prs_err "local declaration may create confusion when \
	computing the layout of reactive objects and \
	their signal connection. \
	At the moment they are not allowed"
	| _ -> false

let rec adjust_constr_body = function
	| [] -> ([],[], true)
	| s:: sl ->
			let (body, signalbus, is_signalbus) = adjust_constr_body sl in
			if is_signalbus && (is_simple_assign s)
			then (body, s:: signalbus, true)
			else (s:: body, signalbus, false)

let mk_constr_entry name fpl mr body active pre post =
	if pre = [] && post = []
	then ()
	else prs_err "Constructors may not have pre- or post-conditions";
	( match s.p_class_kind with
		| Some Interface -> prs_err "interfaces may not have constructors"
		| _ -> ()
	);
	if (used mr.mStatic) || (used mr.mFinal) || (used mr.mAbstract) ||
	(used mr.mInterrupt) || (used mr.mFromC) || (used mr.mToC) ||
	(used mr.mReactive) || (used mr.mVolatile) || (used mr.mParameter)
	then (
		prs_err "invalid modifier used for a constructor"
	) else if name != s.p_class_id then (
		prs_err "invalid name of a constructor"
	) else
		let body, signalbus, _ = if active = None
			then body,[], true
			else adjust_constr_body body in
		let mth = { csrcp = add_new_lbl (); cformals = fpl;
			cbody = body; csig = signalbus; cro =[]; cactive = active;
			cread =[]; cwrite =[]; ccall =[]} in
		mkp_st_entry { name = Ly.c2mf name; origin = s.p_class_id;
				entry = Constructor mth; final = true; parameter = false;
				scope = Instance; volatile = false; access = getAccess mr;
				signature ={ rt = Null; pt = Some (frm2typl fpl) }
			}

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR DATA DEFINITIONS

after parsing a definition, this function checks what kind
of definition it is, checks for consistency (i.e. fields may
not have formal parameters, reactive classes restrict visibility, etc.
Then enter the definition into the symboltable.

Most of the information is coded in the record ''mr''
(see the is - defs above)
------------------------------------------------------------------------- *)
let mk_st_field_entry mr typ efn =
	function
	| { mf_name = m; mf_init = NoInit } ->
			if (s.p_class_kind = Some Interface) then (
				prs_err "Only initialized fields are legal in interfaces"
			) else if used mr.mStatic && used mr.mFinal
			&& not(used mr.mFromC) then (
				prs_err "Static final fields must have an initializer"
			) else if used mr.mParameter && not(used mr.mFromC) then (
				prs_err "Parameters  must have an initializer"
			) else (
				let fkd =
					match mr.mFromC, mr.mToC with
					| Some SameId , None -> NativeFromC (mf2str m)
					| Some (NewId n), None -> NativeFromC n
					| None , Some SameId -> NativeToC (mf2str m)
					| None , Some (NewId n) -> NativeToC n
					| None, None ->
							if is_primitive_typ typ
							then DataField
							else BttmField
					| _ -> prs_err "incompatible field modifier"
				in
				mkp_st_entry (efn m None fkd)
			)
	| { mf_name = m; mf_init = ExprInit i } ->
			let fkd =
				match mr.mFromC, mr.mToC with
				| None, Some SameId -> NativeToC (mf2str m)
				| None, Some (NewId n) -> NativeToC n
				| None, None ->
						if is_primitive_typ typ
						then DataField
						else BttmField
				| _ -> prs_err "incompatible field modifier"
			in
			mkp_st_entry (efn m (Some i) fkd)

let mk_field_entry name_init_l typ clock mr =
	if is_sensor_or_signal_typ typ then (
		enforce_rctclass "signal declarations are only valid in \
		reactive classes.Reactive classes are \
		recognized by an active - block at the end \
		of their constructor";
		s.p_method_kind <- Internal;
		s.p_active <- false;
		if (used mr.mParameter) then (
			prs_err "Signals cannot be parameters"
		) else (
			if (used mr.mStatic) || (used mr.mFinal) || (used mr.mAbstract) ||
			(used mr.mInterrupt) || (used mr.mFromC) || (used mr.mToC) ||
			(used mr.mReactive) || not( getAccess mr = Private )
			then prs_err "invalid modifier for signal decl \
			(only private is legal)"
			else ();
		);
		List.iter (mk_sig_entry typ clock) name_init_l
	) else (
		if (used mr.mParameter) then (
			if (used mr.mStatic)or (used mr.mFinal) then (
				prs_err "parameters are implicitly public static final"
			) else if (used mr.mAbstract) || (used mr.mInterrupt)
			|| (used mr.mFromC) || (used mr.mToC)
			|| (used mr.mReactive) then (
				prs_err "invalid modifier for a field declared in an interface"
			) else (
				mk_volatile mr false;
				mk_final mr true;
				mk_access mr (Private)
			)
			
		) else if (used mr.mReactive) then (
			prs_err "the reactive modifier can only be applied to methods"
		) else if (s.p_class_kind = Some Interface) then (
			if (used mr.mStatic) || (used mr.mFinal)
			|| (used mr.mAccess) || (used mr.mParameter) then (
				prs_err "interface fields are implicitly public static final"
			) else if (used mr.mInterrupt) ||
			(used mr.mFromC) || (used mr.mToC) then (
				prs_err "invalid modifier for a field declared in an interface"
			) else (
				mk_static mr Class;
				mk_volatile mr false;
				mk_final mr true;
				mk_access mr (Public [id_object])
			)
		) else (
			if (used mr.mInterrupt) then (
				prs_err "invalid modifier for a field"
			) else if (used mr.mVolatile) && (getStatic mr = Instance) then (
				prs_err "the volatile modifier may only be used for a static field"
			) else if (used mr.mVolatile) && (used mr.mFinal) then (
				prs_err "the volatile modifier may not be used with final fields"
			) else if (getStatic mr = Instance) &&
			(used mr.mFromC || used mr.mToC) then (
				prs_err "modifier only legal for a static field"
			) else if used mr.mAbstract then (
				prs_err "fields are never abstract"
			) else if not( clock.expr = l_true ) then (
				prs_err "only signal decl may have a clock modifier"
			) else (
			(* ok *)
			)
		);
		let lbl = add_new_lbl () in
		let efn n i k =
			{ name = n; origin = s.p_class_id; final = getFinal mr; scope = getStatic mr;
				volatile = getVolatile mr; parameter = getParameter mr;
				entry = Field { fldsrcp = lbl; init = i; fldkind = k; assigns =[]; reads =[]};
				signature ={ rt = typ; pt = None }; access = getAccess mr
			}
		in
		List.iter (mk_st_field_entry mr typ efn) name_init_l
		
	)

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR METHODS (AND NODES [SPECIAL METHODS])
------------------------------------------------------------------------- *)
let mk_method name rtn_typ clock fpl mr pre post body ckind mkind origin =
	if name = id_main then (
		prs_err "'main'is a reserved keyword. A 'main' will be provided \
		externally (cf. manual)"
	) else if (used mr.mToC) then (
		prs_err "a method modifier is used that is applicable only to fields"
	) else if used mr.mVolatile then (
		prs_err "the volatile modifier may not be used witj a method"
	) else if used mr.mFromC && used mr.mAbstract then (
		prs_err "a method which is implemented in C cannot be abstract"
	) else if used mr.mAbstract && not(ckind = Some AbstractClass) then (
		prs_err "abstract methods are only valid in abstract classes"
	) else if used mr.mReactive && not(ckind = Some RctClass) then (
		prs_err "reactive methods are only valid in reactive classes"
	) else if used mr.mReactive && mkind = DataMethod then (
		prs_err "method declared reactive has statements that are not allowed \
		in reactive methods (e.g. for or while compound statement)"
	) else if used mr.mReactive && rtn_typ != Null then (
		prs_err "illegal for a reactive methods to declare a return type"
	) else if used mr.mInterrupt &&
	not( mkind = BttmMethod || mkind = DataMethod ) then (
		prs_err "invalid (statements in a) body of an interrupt method"
	) else if used mr.mReactive &&
	(used mr.mFinal || used mr.mStatic || (getAccess mr != Private) ||
		used mr.mAbstract || used mr.mInterrupt || used mr.mFromC ||
		used mr.mToC || used mr.mParameter) then (
		prs_err "if a method is declared reactive further modifier are illegal"
	) else if not( clock.expr = l_true ) then (   (* XXX  ???? *)
		prs_err "lustre nodes are not yet implemented"
	) else if (getAccess mr = Private || getAccess mr = Protected) &&
	(not( pre = [] ) || not( post = [] )) then (
		prs_err "private or protected methods may not have pre - or post -\
		conditions"
	) else ();
	if ckind = Some Interface then (
		if (used mr.mFinal || used mr.mStatic || used mr.mAccess) then (
			prs_err "interface methods are implicitly public, abstract, and, \
			of course, not final"
		) else if body != Nobody then (
			prs_err "method body illegal in an interface"
		) else if (used mr.mInterrupt) || (used mr.mFromC) then (
			prs_err "invalid modifier for a method declared in an interface"
		) else (
			(* ok *)
			mk_static mr Instance;
			mk_final mr false;
			mk_access mr (Public [id_object])
		)
	) else (
		if body = Nobody && not(used mr.mAbstract || used mr.mFromC) then (
			prs_err "method without body must be abstract"
		) else if body != Nobody && (used mr.mAbstract || used mr.mFromC) then (
			prs_err "a method body is illegal for this kind of method"
		) else if (used mr.mInterrupt || used mr.mFromC) &&
		(pre != [] || post != []) then (
			prs_err "this kind of method may not have pre or post conditions"
		) else if used mr.mInterrupt &&
		(getStatic mr = Instance || fpl != [] || rtn_typ != Null) then (
			prs_err "interrupt methods are severely restricted: They must \
			be static methods returning no value (void), \
			without any formal parameter"
		) else if used mr.mInterrupt &&
		(body = Nobody || used mr.mFromC || used mr.mAbstract) then (
			prs_err "interrupt methods are severely restricted: It is \
			illegal postconditions. It is illegal to be abstract \
			or to implemented in C (\"native\")"
		) else (
		(* ok *)
		);
	);
	let mkind = if used mr.mFromC then (
			match mkind with
			| BttmMethod
			| DataMethod
			| Abstract -> DataMethod
			| _ -> Err.intern "mk_method"
		) else if rtn_typ != Null then (
			match mkind with
			| BttmMethod -> DataMethod
			| _ -> mkind
		) else (
			if used mr.mReactive then (
				match mkind with
				| BttmMethod -> (RctMethod NotAvailable)
				| _ -> mkind
			) else (
				match mkind with
				| BttmMethod -> DataMethod
				| _ -> mkind
			)
		)
	in
	let mkind = match mr.mFromC, mr.mInterrupt with
		| Some SameId , None -> NativeMethod (mf2str name)
		| Some (NewId m), None -> NativeMethod m
		| None , Some s -> NativeIntr s
		| None , None -> mkind
		| _ -> prs_err "invalid combination of method modifier"
	in
	let mth = { msrcp = add_new_lbl (); mpre = pre; mpost = post;
		mformals = fpl; method_kind = mkind; mbody = body;
		mread =[]; mwrite =[]; mcall =[] } in
	{ name = name; origin = origin; entry = Method mth;
		final = getFinal mr; parameter = false;
		scope = getStatic mr; volatile = false; access = getAccess mr;
		signature ={ rt = rtn_typ; pt = Some (frm2typl fpl) }
	}

let mk_rct_node name fpl flwctxt =
	let lbl = add_new_lbl () in
	let mth = { msrcp = lbl; mpre =[]; mpost =[]; mformals = fpl;
		method_kind = RctNode NotAvailable; mbody = TextStmtL flwctxt;
		mread =[]; mwrite =[]; mcall =[] } in
	let dcl = { name = name; origin = s.p_class_id; entry = Method mth;
		final = true; scope = Instance; volatile = false; parameter = false;
		access = Private; signature ={ rt = Null; pt = Some (frm2typl fpl) }
	} in
	mkp_st_entry dcl

let mk_graphic_call name stmtl =
	let mth = { msrcp = nolbl; mpre =[]; mpost =[]; mformals =[];
		method_kind = GraphicCall NotAvailable; mbody = TextStmtL stmtl;
		mread =[]; mwrite =[]; mcall =[] } in
	let dcl = { name = name; origin = s.p_class_id; entry = Method mth;
		final = true; scope = Instance; volatile = false; parameter = false;
		access = Private; signature ={ rt = Null; pt = Some []}
	} in
	mkp_st_entry dcl

(* -------------------------------------------------------------------------
THIS IS THE MAIN PARSING FUNCTION FOR ANONYMOUS CLASSES
------------------------------------------------------------------------- *)
let rec type2coding =
	function
	| Typ(cid, _) when cid = id_string ->
			( try
				let (_, str, _, _, _) = builtin_info id_string in
				str
			with Not_found -> Err.intern "type2coding:1"
			)
	| Typ(cid, atl) ->
			(c2str cid)^(atl2coding atl)
	| Simple(cid) ->
			( try
				let (_, str, _, _, _) = builtin_info cid in
				str
			with Not_found -> Err.intern "type2coding:2"
			)
	| Array _ -> Err.intern "type2coding:3"
	| Any
	| Null
	| TypeVar _ -> Err.intern "type2coding:4"

and atl2coding = function
	| [] -> ""
	| [Null] -> ""
	| atl ->
			let rec l2str =
				function
				| [] -> Err.intern "atl2coding"
				| [t] -> type2coding t
				| t:: tl -> (type2coding t)^(l2str tl)
			in
			"__"^(l2str atl)

let mk_anon_class__rtn_new ifc mthdl_fun lbl =
	let enctyp = Typ(s.p_class_id,[]) (* approximation of enclosing type *) in
	let cid = c2id (type2coding ifc^"__"^i2s (List.length s.p_anonseq)) in
	let fpl = [{ p_lbl = lbl; p_name = id_p1; p_type = enctyp; p_clock = v_true }] in
	let cstr = { csrcp = lbl; cformals = fpl; cbody =[];
		csig =[]; cro =[]; cactive = None; cread =[]; cwrite =[]; ccall =[] } in
	let cstr = { name = Ly.c2mf cid; origin = cid;
		entry = Constructor cstr; final = true; parameter = false;
		scope = Instance; volatile = false; access = Public [id_object];
		signature ={ rt = Null; pt = Some [enctyp]} } in
	let ckind = Some (Anonymous s.p_class_id) in
	let mthdl = List.map (fun f -> f ckind DataMethod cid) mthdl_fun in
	let mthdl = cstr:: mthdl in
	let st = Hashtbl.create 23 in
	List.iter (pure_st_entry st) mthdl;
	let fn lbl2srcp = { classkind = Anonymous s.p_class_id;
		classid = cid; typparams = List.rev s.p_typparams;
		src_file = s.p_text_file; src_pos = lbl;
		extends = t_object; implements =[ifc];
		specs = [];
		axioms = [];
		props = [];
		symtab = st;
		declseq = mthdl;
		lbl2sp = lbl2srcp;
		class_sta = { class_status = Parsed;
			methods_called =[];
			classes_created =[];
			classes_dyn_created =[];
			class_methods_called =[];
			array_lits =[];
			class_behavior = None;
			typ_constrl =[];
		};
		status = NotTouched;
	} in
	s.p_anonseq <- fn :: s.p_anonseq;
	mk_e (New(Typ(cid,[]),[mk_e (This)]))

(* -------------------------------------------------------------------------
Each class must declare a constructor, constructors must behave well:
- either call another constructor immediately [acyclic !]
- or call super(...) if they extend a class other than Object
------------------------------------------------------------------------- *)
let rec acyclic_constr tsort cid extending = function
	| [] -> ()
	| { entry = Constructor c } as d:: dl ->
			( match c.cbody with
				| s:: sl ->
						( match s with
							| ExprStmt(_,{ expr = ThisConstr l }) ->
									let s = some d.signature.pt "acyclic_constr" in
									let n = List.length s
									and n' = List.length l in
									Tsort.set_pre_post tsort n n'
							| ExprStmt(_,{ expr = SuperConstr l }) ->
									if extending
									then ()
									else let m = "no call to a super " in
										Err.msg (Err.ConstructorError(cid, m))
							| _ ->
									if extending
									then let m = "a call to its super " in
										Err.msg (Err.ConstructorError(cid, m))
									else ()
						)
				| [] ->
						if extending
						then let m = "a call to its super " in
							Err.msg (Err.ConstructorError(cid, m))
						else ()
			);
			acyclic_constr tsort cid extending dl
	| _ -> 
		Err.intern "acyclic_constr"
