open Ly
open Ast
open Util
open P
open Util_print

(* -------------------------------------------------------------------------
TYPES FOR DATA STRUCTURES FOR APPLICATION GENERATION

''tac_context'' is the type of a data structure for application generation.
There exist ONE OBJECT ''ac'' of this type. It is IMPLICIT PARAMETER of
all functions of files gen_application.ml and gen_c_code.ml.
''ac'' contains:
- class & a unique int & their fields FOR ALL classes used in the application
- tree of (static) reactive objects, including the signal bus
- classes created in the application
- the subtype relation
- the reactive engine option
- file name abbreviation list of all file names used in the application

each description of a reactive objects refers to its signals with
''trs_sig''. This list of ''trctsig'' records defines for each signal
its principal signal in ''trs_principal'':
- None for input / output signals in the configuration class
- None for all local signals
- a ''trctsig'' record of the signal of the highest reactive objects in
the tree, which defines an entry for it on the signal bus.

The data is valid if ''ac.ac_sca'' is not None.
------------------------------------------------------------------------- *)
type tac_context =
	{ mutable ac_classes : (ttype * int * (tmfid list)) list;
		mutable ac_rctobj_tree : trctobj list;
		
		mutable ac_new : ttype list;                  (* instantiated classes *)
		mutable ac_dyn : ttype list;        (* dynamically allocated types *)
		mutable ac_sub : (ttype * (ttype list)) list;     (* subtype relation *)
		mutable ac_sca : tsc_appl option;
		mutable ac_fna : (string * int) list;        (* file name abbreviations *)
		mutable ac_gc : ttype list;      (* not []: garbage collector needed *)
	}

let ac =
	{
		ac_classes = [];
		ac_rctobj_tree = [];
		ac_new = [];
		ac_dyn = [];
		ac_sub = [];
		ac_sca = None;
		ac_fna = [];
		ac_gc = [];
	}

let mk_invalid () =
	ac.ac_sca <- None;
	ac.ac_gc <- []

let valid () =
	not( ac.ac_sca = None )

(* -------------------------------------------------------------------------
information about the usage of classes found in the database
------------------------------------------------------------------------- *)
let inform_about_classes () =
	let classl = ac.ac_classes in
	let cidl = List.map (fun (t, _, _) -> type2id t) classl in
	let sel c _ =
		if List.mem c cidl || i_class_status c = NoUpdate
		then None
		else Some c in
	let unused = hashtbl_condense sel se.classtab in
	ps "\nthe application is made from classes:";
	List.iter (fun (t, _, _) -> ps "\n  "; ps (Err.type2str t); ps ";") classl;
	if unused = []
	then ()
	else ( ps "\nthe following classes are not used:";
		List.iter (fun c -> ps "\n  "; psc c; ps ";") unused );
	if ac.ac_gc = [] && ac.ac_dyn = []
	then (
		ps "\nall objects are allocated statically,\
		\nno memory management necessary"
	) else (
		ps "\nobjects of the following type are created at runtime:\n";
		List.iter
			( fun t -> ps "  "; ps (Err.type2str t); pn()
			) (ac.ac_gc@ac.ac_dyn);
		ps "memory management (allocator,garbage collector) is activated"
	)

(* -------------------------------------------------------------------------
MK_RTL: MAKE REACTIVE TREE REPRESENTED AS A LIST [[Reactive Tree List]]
Notation: reactive object == trctobj entry == RO
from the configuration class create the tree of ROs as a trctobj list.
Each RO has its unique path - name trs_name. The trs_name of an RO is the
name of the RO it is part of (the root of this tree is the configuration
object) extended by the name used at its declaration as reactive field.
Each RO has its list of signals as declared in its class stored in trs_sig.
Each signal ''s'' refers to its principal signal ''p'', i.e. the unique
name of the signal
1. from its enclosing class or
2. from a class upwards the tree,
which was initialized with a new signal object. In case 2. there must be
a non - interrupted sequence of constructor calls, originating at the class
which created the new signal ''p'' passing ''p'' downwards til it was
assigned to ''s''.
------------------------------------------------------------------------- *)
let chk_input cid lbl name etyp vtyp =
	let cb_cid = type2id etyp in
	try
		if is_class_subtype ~sub: (type2id etyp) ~super: id_input
		then ()
		else raise Not_found;
		if gen_for_simulation () then (
			if cb_cid = id_sim_input
			then () (* pseudo simulator input class used *)
			else Err.warn (Err.InputIgnored (cid, lbl, name, cb_cid))
		) else (
			let access_ok d = is_access_allowed ~forcid: cid ~tocid: cb_cid d.access
			and dcl = get_decl_after_tc cb_cid id_op_new_val 0 in
			if dcl.signature.rt = t_bool && dcl.signature.pt = Some [] && access_ok dcl
			then () (* new_val() ok *)
			else raise Not_found;
			if vtyp = Null
			then () (* get_val not required ... *)
			else
				let dcl = get_decl_after_tc cb_cid id_op_get_val 0 in
				if dcl.signature.rt = vtyp && dcl.signature.pt = Some [] && access_ok dcl
				then () (* T get_val() does exist, fine *)
				else raise Not_found
		)
	with _ ->
			if cb_cid = id_sim_input
			then Err.msg (Err.SimInput (cid, lbl, name))
			else Err.msg (Err.Input (cid, lbl, name, cb_cid))

let chk_output cid lbl name etyp vtyp =
	let cb_cid = type2id etyp in
	try
		if is_class_subtype ~sub: (type2id etyp) ~super: id_output
		then ()
		else raise Not_found;
		if gen_for_simulation () then (
			if cb_cid = id_sim_output
			then () (* pseudo simulator input class used *)
			else Err.warn (Err.OutputIgnored (cid, lbl, name, cb_cid))
		) else (
			if vtyp = Null
			then
				let dcl = get_decl_after_tc cb_cid id_op_put_val 0 in
				if is_access_allowed ~forcid: cid ~tocid: cb_cid dcl.access
				then () (* putval() ok *)
				else raise Not_found
			else
				let dcl = get_decl_after_tc cb_cid id_op_put_val 1 in
				if dcl.signature.rt = Null && dcl.signature.pt = Some [vtyp]
				&& is_access_allowed ~forcid: cid ~tocid: cb_cid dcl.access
				then () (* putval(T p) ok *)
				else raise Not_found
		)
	with _ ->
			if cb_cid = id_sim_input
			then Err.msg (Err.SimOutput (cid, lbl, name))
			else Err.msg (Err.Output (cid, lbl, name, cb_cid))

let chk_input_output cid sige =
	match sige.trs_principal with
	| Visible ->
			let init =
				match get_siginitexpr_after_tc sige.trs_sdecl with
				| Some i -> i
				| None -> Err.intern "chk_input_output" in
			let etyp =
				match init.expr with
				| New(_,[io]) -> some io.etyp "chk_input_output"
				| _ -> Err.intern "chk_input_output" in
			let vtyp = sig2valtyp sige in
			let lbl = sige.trs_sdecl.sig_pos in
			let name = sige.trs_sname in
			if is_sensor_typ sige.trs_styp
			then chk_input cid lbl name etyp vtyp
			else chk_output cid lbl name etyp vtyp
	| _ -> ()

let mk_sigdecls dcl =
	match dcl.entry with
	| SigDecl s -> Some (dcl.signature.rt, dcl)
	| _ -> None

let get_roinit ~name ~dcl ~cstr =
	try
		get_fldinitexpr_after_tc dcl
	with Not_found ->
			let rec sel =
				function
				| [] -> Err.msg (Err.NotLocalizedRo name)
				| (ExprStmt(_, a)):: sl -> ( match a.expr with
							| Assign(id, _, _, e)
							-> if id = dcl.name
									then e
									else sel sl
							| _ -> sel sl
						)
				| _ -> Err.intern "get_roinit"
			in
			sel cstr.cro

let formal2rctparam formal prefix prcs =
	let prcs = sig2prcsig prcs in
	let nm = prefix @ [Call(formal.p_name, None), formal.p_type] in
	React2sc.mk_rctsig formal ~prc: (Principal prcs) ~sname: nm ~ro: None

let sig2sigdecl ~cid ~prefix ~sig2prc_map (typ, dcl) =
	let name = prefix@[Call(dcl.name, None), dcl.signature.rt] in
	let sdcl =
		match dcl.entry with
		| SigDecl s -> s
		| _ -> Err.intern "sig2sigdecl" in
	let init =
		match get_siginitexpr_after_tc sdcl with
		| None -> Err.msg (Err.NotLocalizedSig name)
		| Some i -> i in
	let prcs =
		match init.expr with
		| New(_, pl) ->
				( match pl with
					| [] -> Invisible
					| [_] -> Visible
					| _ -> Err.intern "sig2sigdecl"
				)
		| Call(f, None) ->
				( try
					let prcs = get_map f sig2prc_map in
					let prcs = sig2prcsig prcs in
					Principal prcs
				with _ ->
						Err.msg (Err.NotLocalizedSig name)
				)
		| _ ->
				Err.intern "sig2sigdecl"
	in
	let sdcl =
		match dcl.entry with
		| SigDecl sdcl -> sdcl
		| _ -> Err.intern "sig2sigdecl" in
	let sige =
		{
			trs_sid = dcl.name; trs_styp = typ;
			trs_sdecl = sdcl; rct_sgndcl = None; trs_sname = name;
			trs_principal = prcs; trs_ro = None; simulinkno = 0;
		}
	in
	chk_input_output cid sige;
	sige

let rec iter_fpl_apl ~ro ~sig2prc_map =
	function
	| [],[] -> []
	| f:: fl, a:: al ->
			let formal2prcl = iter_fpl_apl ro sig2prc_map (fl, al) in
			let styp = some a.etyp "iter_fpl_apl" in
			if is_sensor_or_signal_typ styp then (
				let a =
					match a.expr with
					| Call(a, None) -> a
					| _ -> Err.msg (Err.NotLocalizedRo ro) in
				let a =
					try
						get_map a sig2prc_map
					with _ ->
							let f = mf2str f.p_name and a = mf2str a
							in
							Err.intern ("iter_fpl_apl:"^f^","^a)
				in
				(f, a) :: formal2prcl
			) else (
				formal2prcl
			)
	| _ -> Err.msg (Err.NotLocalizedRo ro)

(* ------------------------------------------------------------------------- *)
let rec mk_rtl ~typ ~name ~actuals =
	let cid = type2id typ in
	let ast = i_ast cid in
	let cdcl = rctclass2constr ast in
	let actuals = List.map (fun (f, p) -> formal2rctparam f name p) actuals in
	let f2pmap = mk_map (List.map (fun d -> (d.trs_sid, d)) actuals) in
	let sigl = list_condense mk_sigdecls ast.declseq in
	let sdcl =
		List.map ( sig2sigdecl ~cid: cid ~prefix: name ~sig2prc_map: f2pmap
			) sigl in
	let ro = { trs_type = typ; trs_name = name; trs_sig = sdcl@actuals } in
	let s2pmap = mk_map (List.map (fun d -> (d.trs_sid, d)) sdcl) in
	let s2pmap = merge_map f2pmap s2pmap in
	List.iter (fun rs -> rs.trs_ro <- Some ro) sdcl;
	List.iter (fun rs -> rs.trs_ro <- Some ro) actuals;
	
	let sel id n d =
		if d.origin = cid && is_field d && (is_reactive_type d.signature.rt)
		then Some d
		else None
	and iter =
		iter_rtl ~sig2prc_map: s2pmap ~enclosing_constructor: cdcl ~prefix: name in
	ro:: (List.fold_right iter (fromsymtab sel ast.symtab) [])

and iter_rtl ~sig2prc_map ~enclosing_constructor ~prefix dcl rtl_l =
	let typ = dcl.signature.rt in
	let cid = type2id typ in
	let init = get_roinit prefix dcl enclosing_constructor in
	let apl =
		match init.expr with
		| New(_, pl) -> pl
		| _ -> Err.msg (Err.NotLocalizedRo prefix) in
	let cdcl = rctclass2constr (i_ast cid) in
	let fpl = cdcl.cformals in
	let f2p =
		iter_fpl_apl ~ro: prefix ~sig2prc_map: sig2prc_map (fpl, apl) in
	let rtl =
		mk_rtl ~typ: typ ~name: (prefix@[Call(dcl.name, None), dcl.signature.rt]) ~actuals: f2p in
	rtl@rtl_l

(* -------------------------------------------------------------------------
INSPECT THE DEPEND_RELATION
- construct the strong dependency lattice, connecting types ct and dt, if
ct either inherits from dt or has a expandable field of type dt
- construct the list of all needed classes
for the whole application (starting with the configuration class),
- generate the list of all classes, from which direct instances exist
------------------------------------------------------------------------- *)
let processed = ref []
and created = ref []
and dyn_created = ref []
and subtypes = ref []

let cid2types = Hashtbl.create 47            (* modfied only by add_ct below *)

let add_ct typ =
	let cid = type2id typ in
	let tl = try Hashtbl.find cid2types cid
		with Not_found -> [] in
	let nt = if (List.mem typ tl)
		then tl
		else typ:: tl in
	Hashtbl.remove cid2types cid;
	Hashtbl.add cid2types cid nt

let check_exp_cb typvar2typ typ2tsort = function
	| SigDecl s ->
			let i, _ = sdecl2initkind s in
			( match i with
				| None -> None
				| Some i ->
						if is_newexpr i then (
							let dep = some i.etyp "check_exp_cb" in
							let dep = typvar2typ dep in
							typ2tsort dep;
							Some dep
						) else (
							None
						)
			)
	| _ -> Err.intern "check_exp_cb"

let get_field_signatures typvar2typ typ2tsort typ =
	let cid = type2id typ in
	let rec field_dcll2typ k =
		function
		| [] -> None
		| d:: dl -> if is_field d then (
					if (d.origin = cid) && (can_expand d) then (
						let expt = typvar2typ (get_expandtyp d) in
						typ2tsort expt;
						Some expt
					) else ( (* type cannot be expanded, but ... *)
						let dclt = typvar2typ d.signature.rt in
						if is_primitive_typ dclt
						then () (* no garbage collecor needed *)
						else ac.ac_gc <- add_sosgl dclt ac.ac_gc;
						Some dclt
					)
				) else if is_signal d && gen_for_targetsys () then (
					check_exp_cb typvar2typ typ2tsort d.entry
				) else (
					field_dcll2typ k dl
				)
	in
	hashtbl_condense field_dcll2typ (i_ast cid).symtab

let rec typ2reachable_types inh_or_exp2tsort typ =
	if is_sensor_or_signal_typ typ
	then []
	else (
		processed := typ::!processed;
		let fal = prepare_subst typ in
		let sub1 p = subst_map (snd p) fal
		and sub2 t = subst_map t fal in
		
		let cid = type2id typ in
		let ast = i_ast cid in
		
		let ext = ast.extends in
		let tinh =
			if ext = t_object || ext = Any || ext = Null
			then ast.implements
			else ext :: ast.implements in
		let tinh = List.map (fun t -> subst_map t fal ) tinh in
		let ctd = List.map (fun t -> subst_map t fal) (i_creates cid) in
		let dctd = List.map (fun t -> subst_map t fal) (i_dyn_creates cid) in
		created := join_list2sosgl ctd !created;
		dyn_created := join_list2sosgl dctd !dyn_created;
		if cid != id_object then (
			subtypes := (List.map (fun t -> (t, typ)) tinh) @ !subtypes;
			List.iter (inh_or_exp2tsort typ) tinh;
		);
		( match ast.classkind with
			| Anonymous s -> inh_or_exp2tsort (Typ(s,[])) typ
			| _ -> ()
		);
		let cll = List.map sub1 (i_called_methods cid)
		and cls = List.map sub1 (i_class_methods cid)
		and ats = get_field_signatures sub2 (inh_or_exp2tsort typ) typ
		and atl = type2tpl typ
		in
		let depend = mk_list_sosgl cll in
		let depend = join_list2sosgl depend ctd in
		let depend = join_list2sosgl depend cls in
		let depend = join_list2sosgl depend ats in
		let depend = join_list2sosgl depend tinh in
		let depend = join_list2sosgl depend atl in
		depend
	)

and inspect_type inh_or_exp2tsort typ =
	if (List.mem typ !processed) || (is_primitive_typ typ) || typ = Any || typ = Null
	then ()
	else (
		inh_or_exp2tsort typ t_object;
		add_ct typ;
		List.iter (inspect_type inh_or_exp2tsort) (typ2reachable_types inh_or_exp2tsort typ)
	)

let inspect_reachable_types conf_type =
	processed := [];
	created := [conf_type];
	subtypes := [];
	Hashtbl.clear cid2types;
	let tsort = Tsort.init 100 in
	let inh_or_exp2tsort client supplier =
		if supplier = Any then (
		(* nothing to do *)
		) else (
			let cid = type2id client
			and sid = type2id supplier in
			if cid = sid && cid = id_object || is_primitive_typ supplier
			then () (* nothing to do *)
			else Tsort.set_pre_post tsort (Ly.c2int sid) (Ly.c2int cid)
		)
	in
	inspect_type inh_or_exp2tsort conf_type; (* computes reachable types *)
	ac.ac_new <- !created;
	ac.ac_dyn <- !dyn_created;
	( match Tsort.sort tsort with
		| Tsort.Cycle cl ->
				let cl = List.map Ly.int2c cl in
				Err.msg (Err.ExpandInheritErr cl)
		| Tsort.Sorted cl ->
				let cl = List.map Ly.int2c cl in
				let cl =
					list_condense
						( fun c -> if c = id_object then None else Some c)
						cl in
				( try
					List.flatten (List.map (Hashtbl.find cid2types) cl)
				with _ ->
						Err.intern "inspect_reachable_types 2"
				)
		| _ -> Err.intern "inspect_reachable_types 3"
	)

let rec get_types_le tsort cid assembled =
	function
	| [] ->
			if is_abstract_class cid
			then assembled
			else cid:: assembled
	| c:: cl ->
			let sub = Tsort.is_pre_post tsort (Ly.c2int cid) (Ly.c2int c) in
			get_types_le tsort cid
				(if sub then c:: assembled else assembled)
				cl

let expand_cid2typel cid =
	let sel (typ, _, _) = if (type2id typ) = cid then Some typ else None in
	list_condense sel ac.ac_classes

let rec expand_cid2type_l = function
	| [] ->
			[]
	| (cid, cid_l):: l ->
			let typl = expand_cid2typel cid
			and subl = List.flatten (List.map expand_cid2typel cid_l) in
			let hl = List.map (fun t -> (t, subl)) typl
			and tl = expand_cid2type_l l in
			hl @ tl

let subtype_relation () =
	let tsort = Tsort.init 100
	and int_obj = Ly.c2int id_object in
	List.iter
		( fun (t, s) ->
					let t = Ly.c2int (type2id t)
					and s = Ly.c2int (type2id s) in
					Tsort.set_pre_post tsort t s
		) !subtypes;
	List.iter
		( fun (t, _, _) ->
					let t = Ly.c2int (type2id t) in
					Tsort.set_pre_post tsort int_obj t
		) ac.ac_classes;
	match Tsort.sort tsort with
	| Tsort.Sorted cid_l ->
			let cid_l = List.map Ly.int2c cid_l in
			let cid_subcids cid = (cid, get_types_le tsort cid [] cid_l) in
			expand_cid2type_l (List.map cid_subcids cid_l)
	| _ ->
			Err.intern "subtype_relation"

(* -------------------------------------------------------------------------
construct the table of own + inherited fields
-> guarantee, that all subtypes have the same sequence of definitions <-
-> ac.ac_classes gives the sequence of cids to consider in a sequence
compatible with the depend relation between the classes
------------------------------------------------------------------------- *)
let rec get_fields (typ : ttype) =
	function
	| [] ->
			Err.intern "get_fields"
	| (t, _, fl):: l ->
			if t = typ
			then fl
			else get_fields typ l

let field_is_used =
	function
	| Field f ->
			not ( f.assigns = [] && f.reads = [] && f.init = None )
	| _ ->
			Err.intern "field_is_used"

let mk_fields_and_rm_irrelevant_types (cnt, class2flds) typ =
	let cid = type2id typ in
	let sel _ _ dcl =
		match dcl.entry with
		| Field _ ->
				if dcl.scope = Instance && dcl.origin = cid && field_is_used dcl.entry
				then Some dcl.name
				else None
		| _ -> None
	in
	if is_sensor_or_signal_typ typ || is_array_typ typ
	then ( (cnt, class2flds)
	) else (
		let at_own = i_fromsymtab sel cid in
		let ext = subst_map (i_extends cid) (prepare_subst typ) in
		let at =
			if ext = Any
			then at_own
			else if ext = t_object || ext = t_input || ext = t_output
			then at_own
			else (get_fields ext class2flds) @ at_own
		in
		let typ2at = typ, cnt, at in
		(cnt + 1, (typ2at:: class2flds))
	)
(* -------------------------------------------------------------------------
make: generate application from the actual configuration - class
------------------------------------------------------------------------- *)
let make () =
	mk_invalid ();
	
	( let ccid = type2id se.conf_class in
		let cast = i_ast ccid in
		if cast.classkind != ConfClass || (class_status ccid (<) Synchron)
		then Err.msg Err.UnsafeConf
		else
			try
				if (get_sysprop se.conf_class id_exc_longjmp) = 0
				then ( Err.msg Err.LongJmp )
				else ()
			with _ ->
					Err.msg Err.LongJmp
	);
	mk_invalid ();
	let cl = inspect_reachable_types se.conf_class in
	let _, cl = List.fold_left mk_fields_and_rm_irrelevant_types (1,[]) cl in
	
	List.iter
		( fun (typ, _, _) ->
					let cid = type2id typ in
					let ast = i_ast cid in
					List.iter
						( fun dcl ->
									if debug_level DbgStatus then (
										print_status ps ast
									);
									if dcl.scope <> Class then (
										match ast.status with
										| Singleton _
										-> dcl.scope <- Single
										| _ when typ = se.conf_class
										-> dcl.scope <- Single
										| _ -> ()
									)
						) ast.declseq
		) cl;
	
	ac.ac_classes <- cl; (* from here it is assumed that ac_classes is valid *)
	ac.ac_fna <- [];
	ac.ac_sub <- subtype_relation ();
	if debug_level DbgOoSubtype then (
		let cmp (t1, t1l) (t2, t2l) =
			if t1 = t2 then true
			else if List.mem t1 t2l then true
			else false
		and ps (t, tl) =
			ps "  "; ps (Err.type2str t); ps " > ";
			List.iter (fun t -> ps (Err.type2str t); ps " ") tl;
			pn()
		in
		List.iter ps (Sort.list cmp ac.ac_sub)
	) else ();
	List.iter
		( fun (t, _, _) -> let cid = type2id t in
					if (class_status cid (<) Synchron)
					then Err.msg (Err.Unchecked(cid)) else ()
		) ac.ac_classes;
	inform_about_classes ();
	
	(* ----------------------------------------------------------------------
	make the rtl [reactive tree list]:
	the tree of reactive objects stored in a list,
	the tree structure is reflected in trs_name
	---------------------------------------------------------------------- *)
	let rtl =
		mk_rtl ~typ: se.conf_class
			~name:[Call(id_conf, None), se.conf_class]
			~actuals:[]
	in
	ac.ac_rctobj_tree <- rtl;
	if debug_level DbgRctDecl then (
		List.iter (print_ro_tree) rtl
	) else ();
	(* React2sc.rctappl2sc expects, that the first item of ''runl''
	defines the reactive object, that is the root of the object - tree,
	i.e. its principal signals, which are In or Out are those, which
	are visible in the environment *)
	if rtl = []
	then (
		mk_invalid ();
		Err.msg (Err.NoCode)
		
	) else (
		ac.ac_sca <- Some (React2sc.mk_rctappl rtl)
	)
