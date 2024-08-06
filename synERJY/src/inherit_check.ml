open Ly
open P
open Ast
open Util

(* =========================================================================
MAKE: CHECK THE INHERIT - RELATION FOR CONSISTENCY W.R.T. CLASSES
=========================================================================
1. detect a circular inherit
2. for type - parameters check, that number, name and constraints
are the same.
3. no interface may be extended, no class implemented
4. no interface is implemented directly twice
5. check consistency of methods / fields w.r.t. the inherit - relation
5a. no effective method inherited from a class may be declared abstract
5b. if a inherited method is abstract & not implemented here, the
class must be abstract
[ 5c. overloaded methods must differ in the number of parameters ]
5d. if a method is redefined, signatures must be identical (return - type!)
5e. a final method may not be redefined
5f. a field or a signal may not be hidden
5g. access rights may not be restricted by redefinition
------------------------------------------------------------------------- *)
let inherit_err fid cid txt =
	Err.msg (Err.InheritErr(fid, cid, txt))

let tvar2tvar tvar_map = function
	| TypeVar f ->
			( try
				TypeVar (get_map f tvar_map)
			with Not_found ->
					TypeVar f
			)
	| t -> t

let guarantee_inherit ast t2tl (mfid, arity) = function
	| [] -> Err.intern ("guarantee_inherit:super:"^mf2str mfid^"/"^i2s arity)
	| sdcl:: sdcll ->
			let tvar2tvar t = tvar2tvar t2tl t in
			let sdcl =
				{ sdcl with
					signature =
						{ rt = tvar2tvar sdcl.signature.rt;
							pt = match sdcl.signature.pt with
								| None -> None
								| Some tl -> Some (List.map tvar2tvar tl)
						}
				} in
			let sdcll = sdcl :: sdcll in
			try
				let dcl = match Hashtbl.find ast.symtab (mfid, arity) with
					| dcl:: _ -> dcl
					| [] ->
							Err.intern ("guarantee_inherit:sub:"^
									mf2str mfid^"/"^i2s arity)
				in
				if not( is_abstract sdcl ) && ( is_abstract dcl )
				then inherit_err mfid ast.classid
						"implemented method redefined as abstract"
				else ();
				if not( is_reactive sdcl = is_reactive dcl )
				then let txt = if is_reactive sdcl
						then "reactive method redefined as data method"
						else "data method redefined as reactive method"
					in
					inherit_err mfid ast.classid txt
				else ();
				if arity = - 1
				then inherit_err mfid ast.classid
						"hiding of fields (shadowing) forbidden"
				else ();
				if sdcl.final
				then inherit_err mfid ast.classid
						"final methods may not be redefined"
				else ();
				if sdcl.signature.rt = dcl.signature.rt
				then ()
				else inherit_err mfid ast.classid
						"return type of a redefining method must \
				be the same as the return type of \
				the method being redefined";
				( let srp = some sdcl.signature.pt "guarantee_inherit" in
					let rp = some dcl.signature.pt "guarantee_inherit" in
					if List.for_all2 (fun t1 t2 -> t1 = t2) srp rp
					then ()
					else inherit_err mfid ast.classid
							"formal parameter types of a redefining \
					method must be the same as \
					the types of the method being redefined"
				);
				( let sac = sdcl.access
					and ac = dcl.access in
					if sac = Private
					then inherit_err mfid ast.classid
							"names of private methods may not be \
					re - used in subclasses. This would \
					reduce readability"
					else if sac = ac
					then ()
					else inherit_err mfid ast.classid
							"the access status of a redefining method \
					must be the same as the access status of \
					the method being redefined"
				);
				if sdcl.signature.rt = dcl.signature.rt
				then ()
				else inherit_err mfid ast.classid
						"return type of a redefining method must \
				be the same as the return type of \
				the method being redefined";
				Hashtbl.remove ast.symtab (mfid, arity);
				Hashtbl.add ast.symtab (mfid, arity) (dcl:: sdcll)
			with Not_found ->
					if is_abstract sdcl && not( is_abstract_ast ast )
					then inherit_err mfid ast.classid
							"a inherited abstract method is \
					not defined here. Thus the class \
					must be an abstract class"
					else ();
					if is_constructor sdcl
					then ()
					else Hashtbl.add ast.symtab (mfid, arity) sdcll

(* -------------------------------------------------------------------------
process the typ parameters of extended classes and implemented interfaces
NOTE: there is a restriction on binding types to type parameter, which will
be REMOVED LATER (on demand :-), namely, that type parameter may only
be bound to not - parametrized types
as an example take a class Class < T, V, W <= Xc >. If this class
1. ... extends like 'extends ParentClass < T, int, V >' and ParentClass < T, U, V >
has its constraint list for type parameter like
[(T,[TypLE Xc]; (U,[]); (V,[])], the resulting constraints are
[(T,[TypLE Xc]; (U,[TypEQ int]); (V,[]); (W,[TypLE Xc])]
2. ... implements like 'implements ParentInterface < T, int, V > and
ParentInterface < I1 <= Yc, I2, I4 > has its constraint list for
type parameter [(I1,[TypLE Yc]; (I2,[]); (I3,[TypEQ int]); (I4,[])],
the resulting constraints are
[(T,[TypLE Xc; TypLE Yc]; (U,[TypEQ int]);
(V,[]); (W,[TypLE Xc]); (I2,[TypEQ int]);
(I3,[TypEQ int])
]
------------------------------------------------------------------------- *)
let is_bound ft cstrl =
	try
		List.exists (function TypEQ _ -> true | _ -> false) (get_map ft cstrl)
	with Not_found ->
			false

let add_constr_map parent_cid child_cid t2tl acc_constr (ft, constr) =
	if is_bound ft acc_constr
	then Err.msg (Err.TypeparInherit(parent_cid, child_cid))
	else ();
	let ft = try get_map ft t2tl
		with _ -> ft in
	add_maplist (ft, constr) acc_constr

let rec extend_typp parent_cid child_cid cstrl atl ftl =
	match atl, ftl with
	| [],[] -> cstrl
	| a:: atl, (f, _):: ftl ->
			let cstrl =
				if a = TypeVar f
				then cstrl (* unchanged *)
				else add_maplist (f,[TypEQ a]) cstrl in
			extend_typp parent_cid child_cid cstrl atl ftl
	| _ ->
			Err.msg (Err.TypeparInherit(parent_cid, child_cid))

let rec impl_typp parent_cid child_cid cstrl atl ftl =
	match atl, ftl with
	| [],[] ->
			(cstrl,[])
	| a:: atl, (f, c):: ftl ->
			let cstrl =
				match a with
				| TypeVar a -> cstrl
				| _ -> add_maplist (f,[TypEQ a]) cstrl
			in
			let cstrl, t2tl = impl_typp parent_cid child_cid cstrl atl ftl in
			let t2tl =
				match a with
				| TypeVar a -> (f, a):: t2tl
				| _ -> t2tl in
			(cstrl, t2tl)
	| _ -> Err.msg (Err.TypeparInherit(parent_cid, child_cid))

let impl_typp parent_cid child_cid cstrl atl ftl =
	let (cstrl, t2tl) = impl_typp parent_cid child_cid cstrl atl ftl in
	let t2tl = mk_map t2tl in
	(cstrl, t2tl)

let check_extend child_cid acc_constrl ptyp =
	let parent_cid = type2id ptyp in
	let past = i_ast parent_cid in
	let pclk = past.classkind in
	let atl = type2tpl ptyp in
	let ftl = past.typparams in
	let pconstrl = i_typ_constrl parent_cid in
	if pclk = ConfClass or
	pclk = RctClass || pclk = FinalClass or
	pclk = Interface
	then Err.msg Err.NoSuchExtension
	else ();
	(* a type parameter of a class to extend is bound or left unchanged *)
	let pconstrl = extend_typp parent_cid child_cid pconstrl atl ftl in
	List.fold_left (add_constr_map parent_cid child_cid [])
		acc_constrl pconstrl

let rec check_implement child_cid acc_constrl = function
		[] -> (acc_constrl,[])
	| ptyp:: ptl ->
			let parent_cid = type2id ptyp in
			let past = i_ast parent_cid in
			let pclk = past.classkind in
			let atl = type2tpl ptyp in
			let ftl = past.typparams in
			let pconstrl = i_typ_constrl parent_cid in
			if not(pclk = Interface)
			then Err.msg Err.NoSuchImpl
			else ();
			(* type parameter of an interface are bound or safely renamed *)
			let pconstrl, t2tl = impl_typp parent_cid child_cid pconstrl atl ftl in
			let acc_constrl = List.fold_left
					(add_constr_map parent_cid child_cid t2tl)
					acc_constrl pconstrl in
			let (acc_constrl, t2tll) = check_implement child_cid acc_constrl ptl in
			(acc_constrl, t2tl:: t2tll)

let rec mk_forced cid =
	if cid = id_any then (
	(* done *)
	) else (
		let my_ast = i_ast cid in
		let my_imp = List.map type2id my_ast.implements in
		List.iter
			( fun (tp, _) -> if class_status tp (>) Unknown
						then Err.msg (Err.TypeparIsClass tp)
						else ()
			) my_ast.typparams;
		if (list_duplicates my_imp)
		then Err.msg (Err.ImplementedTwice cid)
		else ();
		if my_ast.extends = Any
		then ()
		else make (type2id my_ast.extends);
		List.iter make my_imp;
		( match my_ast.classkind with
			| Anonymous containing_cid -> make containing_cid
			| _ -> ()
		);
		(* construct the type parameter list *)
		let tpl =
			let t2t l = function
				| (c, None) -> add_map (c,[]) l
				| (c, Some x) -> let cs = Typ(x,[]) in
						add_map (c,[TypLE cs]) l
			in
			List.fold_left t2t [] my_ast.typparams
		in
		let tpl = if my_ast.extends = Any
			then tpl (* must be a builtin *)
			else check_extend cid tpl my_ast.extends in
		let (tpl, t2tll) = check_implement cid tpl my_ast.implements in
		set_class_typ_constrl cid tpl;
		(* construct the final symbol table by adding inherited stuff *)
		if my_ast.extends = Any
		then ()
		else ( let symtab = (i_ast (type2id my_ast.extends)).symtab in
			Hashtbl.iter (guarantee_inherit my_ast []) symtab
		);
		try
			List.iter2 (fun parent_cid t2tl ->
							Hashtbl.iter (guarantee_inherit my_ast t2tl)
								(i_ast parent_cid).symtab
				) my_imp t2tll
		with Invalid_argument _ -> Err.intern "mk_forced"
	)

and make cid =
	if (class_status cid (<) Parsed) then (
		Err.msg (Err.MissingClass cid)
	) else if (class_status cid (>=) InheritChecked) || cid = id_object then (
	(* done *)
	) else (
		mk_forced cid;
		set_class_sta cid InheritChecked
	)

(* -------------------------------------------------------------------------
inheritance is acyclic, or?
------------------------------------------------------------------------- *)
let rec circularity () =
	let tsort = Tsort.init 100 in
	Hashtbl.iter
		( fun cid ast ->
					let cid' = Ly.c2int cid in
					let super s =
						let s = Ly.c2int s in
						Tsort.set_pre_post tsort cid' s
					in
					if cid = id_object then (
					(* top of ref-classes must be ignored *)
					) else if ast.extends = Any then (
					(* top type must be ignored *)
					) else (
						super (type2id ast.extends);
						List.iter (fun t -> super (type2id t)
							) ast.implements
					)
		) se.classtab;
	match Tsort.sort tsort with
	| Tsort.Sorted _ -> ()
	| Tsort.Cycle cl -> Err.msg (Err.CircularInherit (List.map int2c cl))
	| _ -> Err.intern "circularity"
