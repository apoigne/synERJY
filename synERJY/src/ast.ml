(* --------------------------------------------------------------------------
a symbol table for all abstract syntax trees (ast) of all classes
known in the programming environment.
-------------------------------------------------------------------------- *)
type tclass_table =
	(Ly.tclass, tast) Hashtbl.t

and tclass = Ly.tclass
and tmfid = Ly.tmfid
and tlbl = Ly.tlbl
and tsc_id = Ly.tsc_id

and t_kind =
  | ClassKind
  | ObjectKind

and ttypparams = tclass * tclass option
(* typvar    constraint       *)

and tast =
	{ classkind : tclass_kind;
		src_file : string;
		src_pos : tlbl;
		classid : tclass;
		typparams : ttypparams list;
		extends : ttype;
		implements : ttype list;
		specs : (tspec list);
		axioms : (trctspec list);
		props : (trctspec list);
		symtab : tdecl_table;
		declseq : tdecl_entry list;
		mutable lbl2sp : Ly.tlbl2sp;
		class_sta : tclass_status_type;
		mutable status : tclass_status;
	}

and tclass_kind =
	| AbstractClass
	| EffectiveClass
	| FinalClass
	| RctClass
	| ConfClass
	| Interface
	| Anonymous of tclass (* class in which the anonymous class is def'd *)

(* for analysis of singleton objects *)
and tclass_status =
	| NotTouched
	| Singleton of tclass * tmfid
	| Multiple

(* --------------------------------------------------------------------------
a symbol table for all declarations related to a class.
The symbol table is stored inside the class's ast.
-------------------------------------------------------------------------- *)
and tdecl_table =
	(tmfid * int, tdecl_entry list) Hashtbl.t

and tdecl_entry =
	{ name : tmfid;
		origin : tclass;
		entry : tdef_entry;
		signature : tsignature;
		final : bool;
		volatile : bool;
		parameter : bool;
		mutable scope : tscope;
		access : taccess;
	}

and tdef_entry =
	| NoEntry
	| Field of tfield
	| Method of tmethod
	| Constructor of tconstructor
	| SigDecl of tsigdecl
	| ConstLocal      (* covers also formal parameters *)
	| MutableLocal
	| Label

and tsignature =
	{ rt : ttype; pt: (ttype list) option }

and tscope =
	| Class
	| Instance
	| Single

and taccess =
	| Public of (tclass list)
	| Protected
	| Private

(* --------------------------------------------------------------------------
builtin & user - defined types

characteristics of dimensions:
- None : unconstrained (for tap buffers s..)
- Some dim:
-- length determines the dimension
-- Arbitrary : unrestricted arrays
-- DimLen n : length of the dimension exactly n
-- DimVar x : length is a parameter (for node calls)
-- DimRef x : double refs needed for keeping track
of side effects while computing the dimension
(see le_dim in Util)
This is used only for the .. operator. Once the
typecheck is done, !(!r) should refer to
a dimension DimLen n. We reduce to this dimension
in react2sc when executing expr2sye.
-------------------------------------------------------------------------- *)
and tdim =
	| Arbitrary
	| DimLen of int
	| DimVar of tmfid
	| DimRef of tdim ref ref

and ttype =
	| Any
	| TypeVar of tclass
	| Simple of tclass
	| Typ of tclass * ttype list
	| Array of ttype * tdim * tdim
	| Null

(* --------------------------------------------------------------------------
fields
-------------------------------------------------------------------------- *)
and tfield =
	{ fldsrcp : tlbl;
		mutable fldkind : tfld_kind;
		mutable assigns : tlocation list;
		mutable reads : tlocation list;
		mutable init : texpr option;
	}

and tfld_kind =
	| BttmField             (* bottom of tfld_kind lattice *)
	| ReactiveField         (* > BttmField *)
	| DataField             (* > BttmField *)
	| NativeFromC of string (* > BttmField *)
	| NativeToC of string (* > BttmField *)

and tlocation =
	| AtInit of texpr
	| NewInit of texpr
	| AtCons of texpr
	| NewCons of texpr
	| AtCode of texpr

(* --------------------------------------------------------------------------
methods and constructors
-------------------------------------------------------------------------- *)
and tmethod =
	{ msrcp : tlbl;
		mpre : (tspec list);
		mpost : (tspec list);
		mformals : tformals;
		mbody : tmethod_body;
		mutable method_kind : tmethod_kind;
		mutable mread : tdecl_entry list;
		mutable mwrite : tdecl_entry list;
		mutable mcall : tdecl_entry list;
	}

and tconstructor =
	{ csrcp : tlbl;
		cformals : tformals;
		cactive : tstmtl option;
		mutable csig : tstmtl; (* inits for signals  *)
		mutable cro : tstmtl; (* inits for reactive objects *)
		mutable cbody : tstmtl; (* typecheck must adjust border of above *)
		mutable cread : tdecl_entry list;
		mutable cwrite : tdecl_entry list;
		mutable ccall : tdecl_entry list;
	}

and tmethod_kind =
	| BttmMethod                           (* bottom of tmethod_kind lattice *)
	| RctMethod of tmethod_rctcomp   (* > BttmMethod *)
	| GraphicCall of tmethod_rctcomp   (* > BttmMethod *)
	| RctNode of tmethod_rctcomp   (* > BttmMethod *)
	| DataMethod                           (* > BttmMethod *)
	| Abstract                             (* > BttmMethod *)
	| Internal                             (* > BttmMethod *)
	| NativeMethod of string            (* > BttmMethod *)
	| NativeIntr of string            (* > BttmField *)

and tmethod_body =
	| TextStmtL of tstmtl
	| Nobody

and tgraphicstm =
	{ gstm_file : string;
		mutable gstm_mf : tmfid option }

and tmethod_rctcomp =
	| NotAvailable
	| Touched
	| Available of tsc_comp

(* --------------------------------------------------------------------------
signal declarations
-------------------------------------------------------------------------- *)
and tsigdecl =
	{ mutable sig_assigns : tlocation list;
		mutable sig_tmstmp : bool;
		sig_init : texpr option;
		sig_clock : texpr;
		sig_pos : tlbl;
	}

(* --------------------------------------------------------------------------
statement type, and related types
-------------------------------------------------------------------------- *)
and tstmtl =
	(tstmt list)

and emit_t =
	| FlowEq
	| StateEq of texpr option
	| StdEmit

and tstmt =
	| Activate of tlbl * bool * tstmtl * texpr
	(* next or not *)
	| AssertStmt of tlbl * (tspec list)
	| TextStM of tlbl * tautomaton
	| GraphicStM of tlbl * tgraphicstm
	| Await of tlbl * bool * (tthen_part list)
	(* next or not *)
	| Break of tlbl * tmfid option
	| Continue of tlbl * tmfid option
	| DoStmt of tlbl * texpr * tstmtl
	| Emit of tlbl * emit_t * tmfid * tactuals * texpr
	(* flow/state equation or not,id,index, expression *)
	| ExprStmt of tlbl * texpr
	| ForStmt of tlbl * tfor
	| Halt of tlbl
	| LetStmt of tlbl * tlet
	| Next of tlbl
	| NextState of tlbl * tmfid
	| Nothing of tlbl
	| FlowContext of tlbl * tflowcontext
	| Par of tlbl * (tstmtl list)
	| Cancel of tlbl * bool * bool * tstmtl * (tthen_part list)
	(* strongly or not
	next or not *)
	| RctLoop of tlbl * tstmtl
	| Return of tlbl * (texpr option)
	| Schedule of tlbl * texpr
	| Sustain of tlbl * tstmtl
	| Switch of tlbl * tswitch
	| Throw of tlbl * texpr
	| While of tlbl * texpr * tstmtl

and tflowcontext =
	{ f_dcls : tstmtl; f_equ : tstmtl }

and tautomaton =
	{ a_init : tfsmstate; a_states : tfsmstate list }

and tfsmstate =
	{ sname : tmfid;
		sdo : tstmtl; sentry : tstmtl; sexit : tstmtl; sduring : tstmtl;
		strans : tthen_part list;
		slbl : tlbl;
	}

and tfor =
	{ forinit : texpr option;
		fortest : texpr option;
		forupd : texpr option;
		forstmtl : tstmtl
	}

and tlet =
	{ letent : tentity; letexpr : texpr option;
		letdf : tdef_entry; mutable letil : tstmtl }
(* letdf : ConstLocal, MutableLocal only *)

and tthen_part =
	| Then of tlbl * texpr * tstmtl
	| Else of tlbl * tstmtl

and tswitch =
	{ swexpr : texpr; swcase : tcase list; swdflt : tstmtl }

and tcase =
	{ sfrom : tcasev; sto : tcasev option; sstmtl : tstmtl }

and tcasev =
	| CaseLiteral of tliteral
	| CaseId of tmfid

(* --------------------------------------------------------------------------
the EXPRESSION type, and related types
-------------------------------------------------------------------------- *)
and ttime_unit =
	| Hour
	| Minute
	| Second
	| MilliSec
	| MicroSec

and tslice = { mutable lower : texpr; mutable upper : texpr }

and texpr =
	{ mutable etyp : ttype option;
		expr : tbsexpr; (* changed for signal values, NOT ELSE *)
		elbl : tlbl;
	}

and tbsexpr =
	| Dot of texpr * texpr
	| Call of tmfid * (tactuals option)
	| ClassCall of tclass * tmfid * (tactuals option)
	| New of ttype * tactuals
	
	| Cast of ttype * texpr
	
	| This
	| Super of tmfid * tactuals
	| ThisConstr of tactuals
	| SuperConstr of tactuals
	| NullObj
	
	| IncrDecr of tmfid * tmfid
	(* 1.lhs: an identifier
	2.kind, as ++ or -- *)
	| Assign of tmfid * tarrayasgn * tmfid * texpr
	(* 1.lhs: an identifier
	2.index: if an array is affected
	3.kind, as = += *= <<= etc
	4.rhs *)
	
	| Literal of tliteral
	| ArrayLiteral of tal
	
	| If of (tthen_part list)
	
	| DeltaT
	
	(* during parsing, signal - related expressions are denoted
	by Call(signal, Some [Call(sel, Some [])]),
	sel = id_op_present, id_op_timestamp, id_op_value.
	during reactive translation, they are translated:
	*)
	| Present of tsc_id
	| Timestamp of tsc_id
	| Value of tsc_id
	| SigVal of tsc_id * trctsig * int
	(* reincarnated sgn, signal *)
	| DotDot of tsc_id * trctsig * int
	| Slice of tslice
	| Offset of texpr * texpr
	
	| Var of int
	| Instant        (* compiler generated method, may be used in main to
effect one reaction step == one instant
*)

and tarrayasgn =
	| NoArr
	| ADim1 of texpr
	| ADim2 of texpr * texpr

and tliteral =
	ttype * string * int64 option

and tal =
	| Dim1 of tal1
	| Dim2 of tal2

and tal1 =
	{ av1 : tactuals;
		mutable an1 : int; }

and tal2 =
	{ av2 : tactuals list;
		mutable an2 : int; }

and textid =
	(tbsexpr * ttype) list

and tactuals =
	(texpr list)

(* --------------------------------------------------------------------------
the SPECIFICATION type, and related types
-------------------------------------------------------------------------- *)
and tspec =
	| ClassPrec of (tmthd_label_prec list) list
	| ObjPrec of tmfid * tbsexpr list
	| Assertion of texpr * bool * texpr
	| Comment of string
	| Invariant of texpr * bool * texpr
(* then / else
exception *)
and trctspec =
	| ValConstraint of tlbl * tmfid * tslice
	| Ctl of tlbl * tctl * tctl
	| ACtl of tlbl * tctl * tctl
	| Ltl of tlbl * tltl
	| Ptl of tlbl * tptl

and tmthd_label_prec =
	| MethodPrec of tmfid * (ttype list)
	| RhsFieldPrec of tmfid
	| LhsFieldPrec of tmfid
	| LabelPrec of tlbl list

and tprec =
	| PrecEnt4Class of tmthd_label_prec
	| PrecEnt4Appl of trctobj * tmfid

and tctl =
	| CtlAx of tctl
	| CtlAf of tctl
	| CtlAg of tctl
	| CtlEx of tctl
	| CtlEf of tctl
	| CtlEg of tctl
	| CtlAu of tctl * tctl
	| CtlEu of tctl * tctl
	| CtlNot of tctl
	| CtlAnd of tctl * tctl
	| CtlOr of tctl * tctl
	| CtlXor of tctl * tctl
	| CtlImplies of tctl * tctl
	| CtlEquiv of tctl * tctl
	| CtlSig of tmfid
	| CtlSigVal of tmfid
	| CtlConst of texpr

and tltl =
	| LtlX of tltl
	| LtlG of tltl
	| LtlF of tltl
	| LtlU of tltl * tltl
	| LtlV of tltl * tltl
	| LtlY of tltl
	| LtlZ of tltl
	| LtlH of tltl
	| LtlO of tltl
	| LtlS of tltl * tltl
	| LtlT of tltl * tltl
	| LtlNot of tltl
	| LtlAnd of tltl * tltl
	| LtlOr of tltl * tltl
	| LtlXor of tltl * tltl
	| LtlImplies of tltl * tltl
	| LtlEquiv of tltl * tltl
	| LtlSig of tmfid
	| LtlSigVal of tmfid
	| LtlConst of texpr

and tptl =
	| PtlPrevious of tptl
	| PtlSince of tptl * tptl
	| PtlOnce of tptl
	| PtlHasBeen of tptl
	| PtlNot of tptl
	| PtlAnd of tptl * tptl
	| PtlOr of tptl * tptl
	| PtlImplies of tptl * tptl
	| PtlEquiv of tptl * tptl
	| PtlSig of tmfid
	| PtlConst of texpr

(* --------------------------------------------------------------------------
the types describing a synchronous component
-------------------------------------------------------------------------- *)
and tsc_exp =
	| CC of tsc_id    (* add causal information, only in OOr context *)
	| S of tsc_id
	| FF
	| TT
	| NNot of tsc_exp
	| AAnd of tsc_exp list
	| OOr of tsc_exp list

and tsc_vexp =
	| Rct of tsc_exp
	| Val of texpr

and tsc_frq =
	{ ff : tsc_id;           (* frequency *)
		cd : texpr;            (* syntactic condition*)
		bf : tsc_frq option;   (* base freq*)
	}

and tsc_kind =
	| Eqn
	| Mem
	| Nxt
	| Dbg

and tsc_ctrl =
	{ sc_knd : tsc_kind;
		sc_sgn : tsc_id;
		mutable sc_def : tsc_exp;
		mutable sc_act : tsc_call option;
		sc_sig : trctsig option;
		mutable sc_occ : int; (* number of occurrences on the right hand
		side of wire equations *)
	}

and tsc_call =
	| ActInit of tlbl list * tsc_id * ttype * trctobj
	(* id, type, "this"  *)
	| ActStmt of tlbl list * texpr * trctobj
	(* call,   "this"  *)
	| ActProd of tlbl list * tsc_id * texpr * texpr
	* (tsc_exp * tsc_call) list * trctobj
	(* accumulator, array, array,
	list of conditional actions, "this" *)
	| ActPrm of tlbl list * tsc_id * texpr * tentity * trctobj
	(* fcopy,  expr, formal, "this"  *)
	| ActSigInit of tlbl list * tsc_id * trctsig * trctobj
	(* reincarnated sig-id, signal, "this"  *)
	| ActEmit of tlbl list * bool * tsc_id * tactuals * tsc_vexp
	* (tsc_exp * tsc_call) list * trctsig * trctobj
	(* is flow, reincarnated sig - id, indexes,
	(reactive) expr, list of conditional actions,
	signal, "this" *)
	| ActBlck of (tsc_exp * tsc_call) list
	(* list of conditional actions *)
	| ActSigPres of tlbl list * tsc_id * trctsig * trctobj
	| ActSigVal of tlbl list * tsc_id * trctsig * trctobj
	(* sig-id, signal, "this" *)
	| ActSigIn of tlbl list * trctsig * trctobj
	(* sig-id, signal, "this" *)
	| ActSigOut of tlbl list * trctsig * trctobj
	(* sig-id, signal, "this" *)
	| ActAssgn of tlbl list * tsc_id * texpr
	* (tsc_exp * tsc_call) list * trctobj
	(* Assignment for internal Variables
	sig - id, expr, list of conditional actions, "this" *)
	| ActAssgnInBlck
	of tlbl list * tsc_id * texpr * trctobj
	(* Assignment for internal Variables within a Blck
	sig - id, expr, "this" *)
	| ActBool of tlbl list * tsc_id * texpr * trctobj
	(* result trigger,
	expr, "this" *)
	| ActTimeInit of tlbl list * tsc_id * trctobj
	(* timestamp, "this" *)
	| ActTimeAdd of tlbl list * tsc_id * trctobj
	(* timestamp, "this" *)
	| ActTimeCond of tlbl list * tsc_id * tsc_id * texpr * trctobj
	(* elapsed trigger,
	time stamp,
	time expr,
	"this" *)
	| ActDbg of tlbl list * trctobj
(* label incl. optional state name, "this" *)

and tsc_lcl_dcl =    (* representation of local data: reactive translation *)
	| LclFld of tsc_id * tentity                          (* local fields *)
	| IntSig of tsc_id * ttype * int       (* internal flows for pre etc. *)
	| LclSig of trctsig                                   (* local signal *)

and trctparameter =
	{ prm_ent : tentity;
		prm_sgn : tsc_id option;
	}

and tsc_formal =
	| SigPrm of trctsig                             (* signal as parameter*)
	| SigFld of trctsig                                 (* signal in class*)
	| SigPrc of trctsig                          (* signal in application *)
	| SigLcl of trctsig                                   (* local signal *)
	| VarPrm of trctparameter                         (* valued parameter *)
	| VarLcl of tlet                                    (* local variable *)

and tsc_comp =
	{ sc_ro : trctobj;                               (* reactive object *)
		sc_flds : tsc_formal list;                                (* fields *)
		sc_fmls : tsc_formal list;                            (* parameters *)
		sc_dcls : tsc_lcl_dcl list;                   (* local declarations *)
		sc_alfa : tsc_id list;          (* local signals, for reincarnation *)
		sc_acts : tsc_id list;                                   (* actions *)
		sc_regs : tsc_id list;                                 (* registers *)
		sc_alts : (tsc_id list * tsc_id list) list;  (* mutual excl of id's *)
		sc_dpds : (tsc_id * tsc_id list) list;       (* sgn2regs dependency *)
		sc_omga : tsc_exp;                         (* termination condition *)
		sc_ctrl : tsc_exp list;             (* control signals or registers *)
		sc_inst : int;                                  (* termination code *)
		sc_splt : bool;                                (* to split in loops *)
		sc_defs : tsc_ctrl list;                       (* control equations *)
		sc_axms : trctspec list;                         (* reactive axioms *)
		sc_prps : trctspec list                    (* reactive propositions *)
	}

and tsc_appl =
	{ sa_sigs : trctsig list;                    (* reactive signal list *)
		sa_obsv : tsc_id list;                             (* observables *)
		sa_alpha : tsc_id;                                         (* alpha *)
		sa_beta : tsc_id;                                          (* beta *)
		sa_dcls : tsc_lcl_dcl list;                  (* local declarations *)
		sa_wires : tsc_id list;                    (* wires for declaration *)
		sa_regs : tsc_id list;                (* registers for declaration *)
		sa_eqns : tsc_ctrl list;                       (* wire definitions *)
		sa_mems : tsc_ctrl list;                   (* register definitions *)
		sa_axms : (trctspec * trctobj) list;   (* reactive reactive axioms *)
		sa_prps : (trctspec * trctobj) list;    (* relative reactive props *)
		sa_dbgs : tsc_ctrl list;                  (* debugging definitions *)
	}

and tsc_decl =
	{ sgn : tsc_id;                            (* internal id of a signal *)
		mutable buf : (tdim * int) list;     (* length of the buffer,offset *)
		frq : tsc_frq;                            (* frequerncy of a signal *)
		mem : tsc_id option;                          (* memory of a signal *)
		aa : tsc_id;                                (* alpha of the signal *)
	}

(* --------------------------------------------------------------------------
miscelleaneous simple types
-------------------------------------------------------------------------- *)
and tformals =
	tentity list

and tentity =
	{ p_lbl : tlbl;
		p_name : tmfid;
		p_type : ttype;
		p_clock : texpr;
	}

(* --------------------------------------------------------------------------
the types for the static tree of reactive objects, their signals, and
the mapping to the principal signals, which are ''higher'' in the tree
used in gen_application, react2sya and gen_c_code ...
-------------------------------------------------------------------------- *)
and trctobj =
	{ trs_type : ttype;                   (* class-name of an reactive field *)
		trs_name : textid;            (* dotted name relative to configuration *)
		trs_sig : trctsig list;
	}

and trctobjref =
	trctobj option

and trctsig =
	{ trs_sid : tmfid;
		trs_styp : ttype;
		trs_sdecl : tsigdecl;
		mutable rct_sgndcl : tsc_decl option;
		mutable trs_sname : textid;
		trs_principal : trsprincipal;
		mutable trs_ro : trctobjref;    (* is made mutable only to break
		the trctobj <-> trctsig cycle *)
		mutable simulinkno : int;           (* numbering of input and output
		signals for SIMULINK *)
	}

and trsprincipal =
	| Visible
	| Invisible
	| Principal of trctsig

(* -------------------------------------------------------------------------
types to maintain the status of classes w.r.t. the pu
------------------------------------------------------------------------- *)
and tupd_status =
	| Unknown
	| Parsed           (* added, declared entities (e.g. not twice) ok *)
	| InheritChecked   (* extension & implementation checked for consistency *)
	| TypeChecked
	| Synchron         (* synchronous automaton has been generated *)
	| NoUpdate         (* as good as TypeChecked, used for builtin's *)

and tclass_status_type =
	{ mutable class_status : tupd_status;
		mutable methods_called : (tmfid * ttype) list;
		mutable classes_created : ttype list;
		mutable classes_dyn_created : ttype list;
		mutable class_methods_called : (tmfid * ttype) list;
		mutable array_lits : tal list;
		mutable class_behavior : tsc_comp option;
		mutable typ_constrl : ttyp_constrl;
		(* typvar   constraints     *)
	}

and ttypconstr =
	| TypLE of ttype    (* bind typ to this typ or any subtype only   *)
	| TypEQ of ttype    (* bind typ param only to this typ            *)

and ttyp_constrl = (tclass * ttypconstr list) list

(* -------------------------------------------------------------------------- *)
type tsystem_kind =
	| Platform of string
	| Simulation
	| Host
	| Makefile
	| Simulink
	| Scicos
	| VerilogSimulation
	| Verilog of string
	| Verification

type tproject_kind =
	| CPrj
	| VerilogPrj

(* --------------------------------------------------------------------------
ids for some names, that are used anyway
-------------------------------------------------------------------------- *)
let id_exc_longjmp = Ly.mf2id "throw_value_is_zero_exception"
let id_exc_nullptr = Ly.mf2id "null_pointer_exception"
let id_exc_bounds = Ly.mf2id "array_index_out_of_bounds_exception"
let id_exc_arraysize = Ly.mf2id "array_negative_size_exception"
let id_exc_memory = Ly.mf2id "out_of_memory_exception"
let id_exc_instant = Ly.mf2id "instant_caused_time_overflow_exception"
let id_exc_timestamp = Ly.mf2id "timestamp_wait_too_long_exception"
let id_exc_cast = Ly.mf2id "class_cast_exception"
let id_exc_deltat = Ly.mf2id "dt_used_in_first_instant_exception"

let id_life_cycle = Ly.mf2id "life_cycle"
let id_final = Ly.mf2id "final"
let id_init = Ly.mf2id "init"
let id_exit = Ly.mf2id "exit"
let id_timing = Ly.mf2id "timing"

let c_dont_use = Ly.c2id "???"
let id_wildcard = Ly.c2id "?_"
let id__T = Ly.c2id "T"
let id_any = Ly.c2id "any"
let id_null = Ly.c2id "Null"
let id_object = Ly.c2id "Object"
let id_array1 = Ly.c2id "array1"
let id_array2 = Ly.c2id "array2"
let id_string = Ly.c2id "String"
let id_time = Ly.c2id "time"
let id_vector = Ly.c2id "vector"
let id_matrix = Ly.c2id "matrix"

let id_delayed = Ly.c2id "DelayedSignal"
let id_flow = Ly.c2id "Flow"
let id_sensor = Ly.c2id "Sensor"
let id_signal = Ly.c2id "Signal"
let id_input = Ly.c2id "Input"
let id_output = Ly.c2id "Output"
let id_sim_input = Ly.c2id "SimInput"
let id_sim_output = Ly.c2id "SimOutput"

let id_cuint7 = Ly.c2id "__cuint7"
let id_cuint15 = Ly.c2id "__cuint15"
let id_cuint31 = Ly.c2id "__cuint31"
let id_cuint63 = Ly.c2id "__cuint63"
let id_cint8 = Ly.c2id "__cint8"
let id_cint16 = Ly.c2id "__cint16"
let id_cint32 = Ly.c2id "__cint32"
let id_cint64 = Ly.c2id "__cint64"
let id_cuint8 = Ly.c2id "__cuint8"
let id_cuint16 = Ly.c2id "__cuint16"
let id_cuint32 = Ly.c2id "__cuint32"
let id_cuint64 = Ly.c2id "__cuint64"
let id_bool = Ly.c2id "bool"
let id_int8 = Ly.c2id "int8"
let id_byte = Ly.c2id "byte"
let id_char = Ly.c2id "char"   (* a kind of uint8 *)
let id_short = Ly.c2id "short"  (* int16 *)
let id_uint16 = Ly.c2id "uint16"
let id_int = Ly.c2id "int"    (* int32 *)
let id_uint32 = Ly.c2id "uint32"
let id_long = Ly.c2id "long"   (* int64 *)
let id_uint64 = Ly.c2id "uint64"
let id_double = Ly.c2id "double"
let id_float = Ly.c2id "float"

let is_literal_type = function
	| Simple c -> List.mem c [id_cuint7 ; id_cint8 ; id_cuint8 ;
				id_cuint15; id_cint16; id_cuint16;
				id_cuint31; id_cint32; id_cuint32;
				id_cuint63; id_cint64; id_cuint64]
	| _ -> false

let id_col = Ly.mf2id "column"
let id_row = Ly.mf2id "row"

let id_dont_use = Ly.mf2id "???"

let id_zero = Ly.mf2id "0"
let id_one = Ly.mf2id "1"
let id_true = Ly.mf2id "true"
let id_false = Ly.mf2id "false"
let id_main = Ly.mf2id "main"
let id_conf = Ly.mf2id "conf_obj"
let id_other = Ly.mf2id "other"
let id_p1 = Ly.mf2id "p1"
let id_p2 = Ly.mf2id "p2"

let id_op_dotdot = Ly.mf2id "."
let id_op_value = Ly.mf2id "value"
let id_op_present = Ly.mf2id "present"
let id_op_timestamp = Ly.mf2id "timestamp"

let id_op_pre = Ly.mf2id "pre "
let id_op_upspl = Ly.mf2id "when "
let id_op_downspl = Ly.mf2id "current "
let id_op_fby = Ly.mf2id "->"

let id_op_prefix_incr = Ly.mf2id "prefixIncr"
let id_op_prefix_decr = Ly.mf2id "prefixDecr"
let id_op_postfix_incr = Ly.mf2id "postfixIncr"
let id_op_postfix_decr = Ly.mf2id "postfixDecr"

(* to avoid confusion with user - defined data all operator
have a blank as last symbol. Seems to produce no confusion *)
let id_op_all = Ly.mf2id "all "
let id_op_add = Ly.mf2id "add "
let id_op_bit_and = Ly.mf2id "bitwise_and "
let id_op_log_and = Ly.mf2id "logical_and "
let id_op_assign = Ly.mf2id " assign (=) "
let id_op_complement = Ly.mf2id "complement "
let id_op_div = Ly.mf2id "div "
let id_op_equal = Ly.mf2id " equal (==) "
let id_op_ge = Ly.mf2id "ge "
let id_op_get = Ly.mf2id "get "
let id_op_gt = Ly.mf2id "gt "
let id_op_high = Ly.mf2id "high "
let id_op_hour = Ly.mf2id "hour "
let id_op__i = Ly.mf2id "__i"
let id_op__j = Ly.mf2id "__j"
let id_op__k = Ly.mf2id "__k"
let id_op_is_digit = Ly.mf2id "is_digit "
let id_op_is_lower = Ly.mf2id "is_lower "
let id_op_is_upper = Ly.mf2id "is_upper "
let id_op_le = Ly.mf2id "le "
let id_op_leftshift = Ly.mf2id "leftshift "
let id_op_length = Ly.mf2id "length"
let id_op_diagonal = Ly.mf2id "diagonal "
let id_op_cols = Ly.mf2id "cols"
let id_op_rows = Ly.mf2id "rows"
let id_op_low = Ly.mf2id "low "
let id_op_lt = Ly.mf2id "lt "
let id_op_min = Ly.mf2id "min "
let id_op_minus = Ly.mf2id "minus "
let id_op_mod = Ly.mf2id "mod "
let id_op_msec = Ly.mf2id "msec "
let id_op_mult = Ly.mf2id "mult "
let id_op_not = Ly.mf2id "not "
let id_op_not_equal = Ly.mf2id "not equal (!=)"
let id_op_bit_or = Ly.mf2id "bitwise_or "
let id_op_log_or = Ly.mf2id "logical_or "
let id_op_plus = Ly.mf2id "plus "
let id_op_rightshift = Ly.mf2id "rightshift "
let id_op_rightshift0 = Ly.mf2id "rightshift0 "
let id_op_sec = Ly.mf2id "sec "
let id_op_set = Ly.mf2id "set "
let id_op_sub = Ly.mf2id "sub "
let id_op_usec = Ly.mf2id "usec "
let id_op_transp = Ly.mf2id "^t"
let id_op_xor = Ly.mf2id "xor "
let id_op_zero = Ly.mf2id "zero "

let id_op_pointmult = Ly.mf2id "point mult (.*)"

let id_op_to_bool = Ly.mf2id "to_bool"
let id_op_to_int8 = Ly.mf2id "to_int8"
let id_op_to_byte = Ly.mf2id "to_byte"
let id_op_to_char = Ly.mf2id "to_char"
let id_op_to_short = Ly.mf2id "to_short"
let id_op_to_int16 = Ly.mf2id "to_int16"
let id_op_to_uint16 = Ly.mf2id "to_uint16"
let id_op_to_int = Ly.mf2id "to_int"
let id_op_to_int32 = Ly.mf2id "to_int32"
let id_op_to_uint32 = Ly.mf2id "to_uint32"
let id_op_to_long = Ly.mf2id "to_long"
let id_op_to_int64 = Ly.mf2id "to_int64"
let id_op_to_uint64 = Ly.mf2id "to_uint64"
let id_op_to_float = Ly.mf2id "to_float"
let id_op_to_double = Ly.mf2id "to_double"
let id_op_to_string = Ly.mf2id "to_string"
let id_op_to_time = Ly.mf2id "to_time"
let id_op_new_val = Ly.mf2id "new_val"
let id_op_get_val = Ly.mf2id "get_val"
let id_op_put_val = Ly.mf2id "put_val"

let t_bool = Simple(id_bool)                  (* bool types *)
(* numerical types *)
let t_cuint7 = Simple(id_cuint7)                (* manifest constants *)
let t_cuint15 = Simple(id_cuint15)
let t_cuint31 = Simple(id_cuint31)
let t_cuint63 = Simple(id_cuint63)
let t_cint8 = Simple(id_cint8)
let t_cint16 = Simple(id_cint16)
let t_cint32 = Simple(id_cint32)
let t_cint64 = Simple(id_cint64)
let t_cuint8 = Simple(id_cuint8)
let t_cuint16 = Simple(id_cuint16)
let t_cuint32 = Simple(id_cuint32)
let t_cuint64 = Simple(id_cuint64)
let t_byte = Simple(id_byte)                  (* integral types *)
let t_char = Simple(id_char)
let t_short = Simple(id_short)
let t_uint16 = Simple(id_uint16)
let t_int = Simple(id_int)
let t_uint32 = Simple(id_uint32)
let t_long = Simple(id_long)
let t_uint64 = Simple(id_uint64)
let t_float = Simple(id_float)                 (* floating-point types *)
let t_double = Simple(id_double)
let t_time = Simple(id_time)

let t_object = Typ(id_object,[])
let t_string = Typ(id_string,[])

let t_input = Typ(id_input,[])
let t_output = Typ(id_output,[])

let e2e e t = { etyp = Some t; expr = e; elbl = Ly.nolbl }

let l_true = Literal (t_bool,"true" , None)
let l_false = Literal (t_bool,"false", None)
let l_zero = Literal (t_char,"0", None)

let v_deltat = e2e (DeltaT) t_time
let v_true = e2e l_true t_bool
let v_false = e2e l_false t_bool
let v_zero = e2e l_zero t_char

(* --------------------------------------------------------------------------
types for the GRAPHIC EDITOR
-------------------------------------------------------------------------- *)
type tfont = string * string * string

type tgraphic =
	| State of tstate
	| Trans of ttransition
	| Init of tinex
	| Exit of tinex
	| Cond of tcond

and tstate =
	{ st_id : int;
		mutable st_name : string;                              (* state-name *)
		mutable st_parent : int;           (* refers to the context of def'n *)
		mutable st_actions : string;                              (* action  *)
		mutable st_font : tfont option;                      (* action font  *)
		mutable st_frozen : bool;                     (* no cut, resize etc. *)
		mutable st_open : bool;                               (* open canvas *)
		mutable st_separ : int;                (* pos of hor line below name *)
		mutable st_refine : trefine;        (* refinement: subgraphic or AND *)
		mutable st_locals : string ;
		mutable st_andacts : tstate_and list;
		mutable st_x : int; mutable st_y : int;
		mutable st_x' : int; mutable st_y' : int;
		
		mutable st_tk : tstatetk option
	}

and trefine =
	| AndRefine of int * (int list)   (* int: 0 = hor, 1 = vert; int list: pos for AND - lines *)
	| Graphic of int                  (* context of refined state *)
	| Textual of int option           (* None - only textual state *)
(* Some - context of refined textual state *)

and tstatetk =
	{ mutable st_tkframe : string;                          (* tkid for the frame *)
		mutable st_tksepar : string;                      (* tkid for the separator *)
		mutable st_tkname : string;                      (* tkid for the state-name *)
		mutable st_tkact : string;                          (* tkid for the actions *)
		mutable st_tkand : string list;             (* tkid for AND-separating line *)
		mutable st_tkref : string option;      (* tkid for refinement state polygon *)
	}

and tstate_and =
	{ panel : int;
		panel_act : string;
		panel_font : tfont option;
	}
(* -------------------------------------------------------------------------- *)
and ttransition =
	{ tr_id : int;
		mutable tr_from : tgraphic;
		mutable tr_frco : tconnect;
		mutable tr_to : tgraphic;
		mutable tr_toco : tconnect;
		mutable tr_txt : string;
		mutable tr_font : tfont option;
		mutable tr_parent : int;   (* refers to the context of def'n *)
		mutable tr_midx : int;   (* the middle point is relative to trfrom *)
		mutable tr_midy : int;
		mutable tr_txtx : int;   (* the text-anchor (of kind nord-west) is *)
		mutable tr_txty : int;   (* relative to the point trmid            *)
		
		mutable tr_tk : ttranstk option
	}

and ttrans =                          (* types for parsing transitions *)
	{ tr_priority : int;
		tr_trans : tthen_part
	}

and tconnect =
	{ mutable side : int;         (* 1: top; 2: right; 3: botton; 4: left *)
		mutable step : int          (* clockwise per side, only positive    *)
	}

and ttranstk =
	{ mutable tr_tkline : string;     (* tkid for the arrow/line *)
		mutable tr_tktxt : string;     (* tkid for the text *)
	}

(* -------------------------------------------------------------------------- *)
and tinex =
	{ ie_id : int;
		mutable ie_x : int; mutable ie_y : int;
		mutable ie_r : int;
		mutable ie_parent : int;   (* refers to the context of def'n *)
		
		mutable ie_tk : tinextk option
	}

and tinextk =
	{ mutable ie_tkid : string
	}

(* -------------------------------------------------------------------------- *)
and tcond =
	{ co_id : int;
		mutable co_x : int; mutable co_y : int;
		mutable co_r : int;
		mutable co_parent : int;   (* refers to the context of def'n *)
		mutable co_txt : string;
		mutable co_font : tfont option;
		
		mutable co_tk : tcondtk option
	}

and tcondtk =
	{ mutable co_tkid : string;
		mutable co_tktxt : string
	}

(* --------------------------------------------------------------------------
Tools of the GRAPHIC EDITOR accessible for the user
-------------------------------------------------------------------------- *)
type ttool =
	| PointTool
	| StateTool
	| AndTool
	| TransTool
	| InitTool
	| ExitTool
	(*| TextTool*)
	| CondTool

and ttool_memory =
	| NullMemory
	| StateCreate of int * int
	(* mouse cordinates (x,y) *)
	| AndCreate of tgraphic
	(* go, to which the and belongs *)
	| PointSelect of int * int
	(* mouse cordinates (x,y) *)
	| PointMove of int * int
	(* mouse cordinates (x,y) *)
	| AndMove of int * int * int * int list
	(* mouse cordinates (x,y), the AND moved + the rest list *)
	| TransTextMove of int * int
	(* mouse cordinates (x,y) *)
	| PointResize of int * int * int
	(* mouse cordinates (x,y) , handle-number, see util.ml *)
	| TransCreate of tconnect * tgraphic * int * int
	(* from-connector, from-go, mouse cordinates (x,y) *)
	| TextEdit of tgraphic * int
(* go to which the text belongs, for states: 0->name;1->actions *)

(* --------------------------------------------------------------------------
Tk - events for the GRAPHIC EDITOR
-------------------------------------------------------------------------- *)
and tevent =
	| B1press
	| B1rel
	| ShiftB1press
	| ShiftB1rel
	| DoubleB1press
	| DoubleB1rel
	| ShiftDoubleB1press
	| ShiftDoubleB1rel
	| B2press
	| B2rel
	| B3press
	| B3rel
	| ActivateTool
	| Suspend_Tool

(* --------------------------------------------------------------------------
types for command files
-------------------------------------------------------------------------- *)
type tcmd_tool =
	| Sim
	| Ge
	| Edit

type tcmd_int_or_string =
	| CmdInt of int
	| CmdString of string

type tconfsig =
	| AllSig
	| InOutSig

type tcmd_to_do =
	| CmdReset
	| CmdClearwdw
	| CmdLoadTraceFile of string
	| CmdLoadFile of string
	| CmdSetConfClass of string option
	| CmdMkTestBinary
	| CmdMkCode
	| CmdMkBuild
	| CmdMkTest
	(* CmdFPGA
	| CmdVisClass
	| CmdVisAppl
	| CmdFormClass
	| CmdFormAppl
	| CmdBlifAppl *)
	| CmdLoadSimConf of string
	| CmdSaveSimConf of string
	| CmdObjectBrowser
	| CmdTraceBrowser
	| CmdConfSignal of tconfsig
	| CmdExecTool of tcmd_tool
	| CmdQuit
	| CmdUnit
	| CmdEof
	| CmdPrint of string
	| CmdRedisplay
	| CmdSetCodeStyle of string
	| CmdSetParPort of string

(* --------------------------------------------------------------------------
the global persistent data of the programming environment.
this data can be observed to guarantee consistency for different
representation. Typical use:
1. add_observer < the fun to be called if something changes >
2. se.graphic_width <- 999; ...; se_has_changed () <~~calls observer
-------------------------------------------------------------------------- *)
class observers =

object(self)
	
	val mutable observers = []
	
	method add_observer o = observers <- (o :: observers)
	
	method rm_observer o' =
		observers <- List.filter (fun o -> o = o') observers
	
	method clear_observers () = observers <- []
	
	method has_changed () =
		List.iter (fun observer -> observer ()) observers
end

type tpersistent_data =
	{ version : string;
		ge_version : string;
		sesim_version : string;
		mutable hostos : string;
		mutable home : string;
		mutable timescale : int;
		mutable classtab : tclass_table;
		mutable conf_class : ttype;
		mutable conf_classes : tclass list;
		mutable se_home : string;
		mutable ge_width : int;
		mutable ge_height : int;
		mutable se_font : tfont;
		mutable ge_font : tfont;
		mutable sim_font : tfont;
		font_sizes : string list;
		font_weights : string list;
		mutable print_format : string; (* "a4" and "a5" supported *)
		mutable file_prefix : string;
		mutable file_suffix : string;
		mutable se_file : string;
		mutable ge_file : string;
		mutable editor : string;
		mutable cmd_file : string;
		mutable cmdline_files : string list;
		mutable batchmode : bool;
		mutable projectpath : string;
		mutable workspace : string;
		mutable target_sys : tsystem_kind;
		mutable cclibs : string list;
		mutable sefiles : string list;
		mutable seutils : string list;
		mutable secfiles : string list;
		mutable hfiles : string list;
		mutable cfiles : string list;
		mutable vfiles : string list;
		mutable simbinary : string;
		mutable trace_file : string;
		mutable trace_files : string list;
		mutable scicos_model : string;
		mutable scicos_models : string list;
		mutable targets : (string * tsystem_kind) list;
		mutable mc_class : string;
		mutable mc_name : string;
		mutable sc_file : string;
		mutable code_sty : string;
		mutable debug_level : string option;  (* see function debug_level *)
		mutable project : string option;
		mutable project_kind : tproject_kind;
		logics : (string * string list) list;
		mutable open_directory: string;
		mutable save_directory: string;
		mutable uploadbutton : string;
		mutable par_port : string;
		mutable matlab_dir : string;
		mutable scilab_dir : string;
		mutable nu_smv_dir : string;
		mutable word_width : string;
	}

let se = let sehm =
		
		try Sys.getenv "SE_HOME"
		with Not_found ->
				let pwd = Filename.dirname(Sys.getcwd()) in
				if Filename.basename(pwd) = "synERJY" then (
					(* for starting in eclipse *)
					pwd
				) else (
					prerr_string "environment variable SE_HOME not set.\n\n";
					""
				)
	and workspace =
		try match Sys.os_type with
			| "Win32" -> Filename.concat
						(Sys.getenv "HOMEDRIVE") (Sys.getenv "HOMEPATH")
			| _ -> Sys.getenv "HOME"
		with Not_found ->
				prerr_string "environment variable HOME not set.\n\n";
				""
	in
	{ version = "5.3";
		ge_version = "2.0";
		sesim_version = "3.0";
		hostos = "unix";
		home = workspace;
		timescale = 1;
		classtab = Hashtbl.create 23;
		conf_class = t_object;
		conf_classes = [];
		se_home = sehm;
		ge_width = 913;
		ge_height = 675;
		se_font = "TkFixedFont","10","normal";
		ge_font = "TkFixedFont","8","normal";
		sim_font = "TkFixedFont","10","normal";
		font_sizes = [ "8"; "9"; "10"; "11"; "12"; "13";
			"14"; "16"; "18"; "20"; "24" ];
		font_weights = [ "normal"; "bold" ];
		print_format = "a4"; (* "a4" and "a5" supported *)
		file_prefix = "se.";
		file_suffix = ".se";
		editor = "";
		cmd_file = "";
		cmdline_files = [];
		batchmode = false;
		se_file = "";
		ge_file = ".sec";
		projectpath = workspace;
		workspace = workspace;
		target_sys = Simulation;
		sefiles = [];
		seutils = [];
		secfiles = [];
		cclibs = [];
		hfiles = [];
		cfiles = [];
		vfiles = [];
		trace_file = "";
		trace_files = [];
		scicos_model = "";
		scicos_models = [];
		targets = [];
		simbinary = ( match Sys.os_type with
				| "Unix" -> "se.a.out"
				| _ -> "se.a.out.exe"
			);
		mc_class = "ClassN";
		mc_name = "assertN";
		sc_file = ".sc";
		debug_level = None;
		code_sty = "expr-word";
		project = None;
		project_kind = CPrj;
		logics = [ "NuSMV",["CTL";"LTL";"RTCTL";"PSL"]; ];
		open_directory = workspace;
		save_directory = workspace;
		uploadbutton = "normal";
		par_port = "0x378";
		matlab_dir = "";
		scilab_dir = "";
		nu_smv_dir = "";
		word_width = "32";
	}
