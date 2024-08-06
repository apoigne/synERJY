open Ly
open Ast
open P
open Util
open Util_print

(* -------------------------------------------------------------------------
extract all RELEVANT method and field names from a data expression
relevant is defined as all names which could be prefixed safely by ''this''
for instance: in f.g.h(k.l, m, 1 + n.o) f, k, m, n should be extracted
------------------------------------------------------------------------- *)
let mk_signature_typ el =
	List.map (fun e -> some e.etyp "mk_signature_typ") el

let rec destruct_expr expr =
	match expr.expr with
	| DotDot(_)
	| Present(_)
	| Value(_)
	| Offset(_)
	| Var(_)
	| Timestamp(_) ->
			[]
	| SigVal _ ->
			[]
	| Slice r ->
			(destruct_expr r.lower)@(destruct_expr r.upper)
	| Dot(e, f) ->
			(destruct_expr e) @ (bs_dot f)
	| Call(f, None) ->
			[RhsFieldPrec(f)]
	| Call(f, Some el) ->
			MethodPrec(f, mk_signature_typ el) ::
			(List.concat (List.map destruct_expr el))
	| Assign(f, NoArr, _, e) ->
			LhsFieldPrec(f) :: destruct_expr e
	| Assign(f, ADim1 i, _, e) ->
			LhsFieldPrec(f) :: destruct_expr e @ destruct_expr i
	| Assign(f, ADim2(x, y), _, e) ->
			LhsFieldPrec(f) :: destruct_expr e @ destruct_expr x @ destruct_expr y
	| IncrDecr(f, _) ->
			[LhsFieldPrec(f)]
	| ClassCall(c, f, None) ->
			[]
	| ClassCall(c, f, Some el) ->
			List.concat (List.map destruct_expr el)
	| New(_, el) ->
			List.concat (List.map destruct_expr el)
	| DeltaT | This
	| NullObj | Literal _
	| ArrayLiteral _ ->
			[]
	| Cast(_, e) ->
			destruct_expr e
	| Super(f, el) ->
			MethodPrec(f, mk_signature_typ el) ::
			(List.concat (List.map destruct_expr el))
	| If(tpl) ->
			List.concat (List.map bs_if tpl)
	| e -> raise Not_found

and bs_dot expr =
	match expr.expr with
	| Dot(e, f) ->
			(bs_dot e) @ (bs_dot f)
	| Call(_, None) ->
			[]
	| Call(_, Some el) ->
			List.concat (List.map destruct_expr el)
	| _ -> raise Not_found

and bs_if =
	function
	| Then(_, e1,[ExprStmt(_, e2)]) ->
			(destruct_expr e1) @ (destruct_expr e2)
	| Else(_,[ExprStmt(_, e)]) ->
			(destruct_expr e)
	| _ ->
			raise Not_found

let sign2str =
	function
	| RhsFieldPrec(f) ->
			mf2str f
	| LhsFieldPrec(f) ->
			"("^mf2str f^" = )"
	| MethodPrec(f, tl) ->
			(mf2str f)^"("^(lst_sep2str Err.type2str "," tl)^")"
	| LabelPrec ll ->
			lst_sep2str lbl2str ":" ll

let destruct_syncexpr cid expr =
	try let relevant_names = destruct_expr expr in
		let relevant_names = mk_list_sosgl relevant_names in
		if debug_level DbgDataCall then (
			ps "\nrelevant calls: ";
			List.iter (fun s -> ps (sign2str s); ps " ") relevant_names;
			pn(); pF ()
		);
		relevant_names
	with Not_found ->
			Err.msg (Err.ExprKindNotReactive(cid, expr.elbl))

let rec sync2el cid ll e =
	let el = destruct_syncexpr cid e in
	if Ly.is_int_lbl (list_last ll)
	then el
	else (LabelPrec ll):: el
and sync2ell cid =
	function
	| [] -> []
	| (_, cll):: al -> (destruct_sync_h cid cll)@(sync2ell cid al)
and destruct_sync_h cid =
	function
	| ActStmt(l, e, _) ->
			sync2el cid l e
	| ActProd(l, _, e1, e2, cc, _) ->
			(sync2el cid l e1)@(sync2el cid l e2)
			@(sync2ell cid cc)
	| ActSigInit _ -> []
	| ActEmit(l, _, _, _, Rct e, cc, _, _) ->
			(sync2el cid l v_true)@(sync2ell cid cc)
	| ActEmit(l, _, _, _, Val e, cc, _, _) ->
			(sync2el cid l e)@(sync2ell cid cc)
	| ActBlck(cc) ->
			sync2ell cid cc
	| ActSigPres _
	| ActSigVal _
	| ActSigIn _
	| ActSigOut _ ->
			[]
	| ActAssgn(l, _, e, cc, _) ->
			(sync2el cid l e)@(sync2ell cid cc)
	| ActAssgnInBlck(l, _, e, _) ->
			sync2el cid l e
	| ActBool(l, _, e, _) ->
			sync2el cid l e
	| ActTimeInit _ ->
			[]
	| ActTimeAdd _ ->
			[]
	| ActTimeCond(l, _, _, e, _) ->
			sync2el cid l e
	| ActPrm(l, _, e, _, _) ->
			sync2el cid l e
	| ActInit _
	| ActDbg _ ->
			[]
and destruct_sync cid d =
	(d, destruct_sync_h cid (some d.sc_act "destruct_sync"))

(* ---------------------------------------------------------------------------
data_check ... avoid time races effected by DATA CALLS

LOCAL FUNCTIONS ARE:
check_cl = gets a (action_id, data_call list) list
check_c = gets two items of (action_id, data_call list)
prepares ''is_concurrent'' for lazy evaluation in check_cc
check_cc = gets two items of data_call and guarantees, that a
precedence has been declared or activations are not concurrent,
otherwise report error;
looks whether precedences are consistent,
if no, an error is reported; if yes, then return ...
check_c will make a call for later topological sorting; return ...
check_cl terminates

The partial order of tprec is:

Before After
\ /
\ /
\ /
DontCare
|
|
TimeRace
|
|
DontKnow
------------------------------------------------------------------------- *)
type tdataprec =
	| DontKnow
	| DontCare
	| TimeRace
	| Before
	| After

(* generate dependencies from the specification ... for a class *)
let for_class cid sync_calls pre_post pot_race =
	let data_prec_l = i_class_precl cid in
	let was_pre = ref false
	and was_post = ref false
	and was_dc = ref false
	and was_race = ref None
	in
	
	let rec check_cl =
		function
		| [] ->
				()
		| acl:: acll ->
				List.iter (check_c acl) acll; check_cl acll
	
	and check_c (a1, cl1) (a2, cl2) =
		if debug_level DbgDataCheckPair then (
			let pl c = ps (sign2str c); ps " " in
			ps "\ncheck_c:\ncl1: "; List.iter pl cl1;
			ps "\ncl2: "; List.iter pl cl2
		);
		was_pre := false;
		was_post := false;
		was_dc := false;
		was_race := None;
		List.iter (fun c -> List.iter (check_cc a1 a2 c) cl2) cl1;
		if (!was_pre || !was_post || !was_dc) then (
			(* no timerace *)
			if !was_pre then pre_post a1 a2;
			if !was_post then pre_post a2 a1
		) else (
			match !was_race with
			| None ->
					()
			| Some (c, d) ->
					pot_race (a1, PrecEnt4Class c) (a2, PrecEnt4Class d)
		)
	
	and check_cc a1 a2 c d =
		if debug_level DbgDataCheckPair then (
			ps "\ncheck_cc: "; ps (sign2str c); ps " "; ps (sign2str d)
		);
		let is_decl_final cid f =
			try (get_decl_after_tc cid f (- 1)).final
			with _ ->
					true (* locals & formals considered final XXX *)
		in
		match (c, d) with
		| LhsFieldPrec(f), LhsFieldPrec(g) when not(f = g) ->
				() (* both are different fields, no time race possible *)
		| LhsFieldPrec(f), RhsFieldPrec(g) when not(f = g) ->
				() (* both are different fields, no time race possible *)
		| RhsFieldPrec(f), LhsFieldPrec(g) when not(f = g) ->
				() (* both are different fields, no time race possible *)
		| RhsFieldPrec(_), RhsFieldPrec(_) ->
				() (* both are RHS fields, no time race possible *)
		| RhsFieldPrec(f), _ when is_decl_final cid f ->
				() (* no problems with final fields ... *)
		| _, RhsFieldPrec(f) when is_decl_final cid f ->
				() (* no problems with final fields ... *)
		| _ ->
				( match specified_relation c d with
					| DontKnow ->
							Err.intern "check_cc 1"
					| Before ->
							was_pre := true;
					| After ->
							was_post := true;
					| DontCare ->
							was_dc := true;
					| TimeRace ->
							was_race := Some (c, d);
				)
	
	and specified_relation c d =
		let dir = ref TimeRace in
		
		let rec upd_dir n =
			match !dir with
			| TimeRace ->
					dir := n
			| o ->
					if o = n
					then ()
					else let c = sign2str c and d = sign2str d in
						Err.msg (Err.DataPrec(cid, c, d))
		and precitem_eq x prec =
			match (x, prec) with
			| RhsFieldPrec(mf1), RhsFieldPrec(mf2) when mf1 = mf2 ->
					true
			| LhsFieldPrec(mf1), LhsFieldPrec(mf2) when mf1 = mf2 ->
					true
			| MethodPrec(mf1, sg1), MethodPrec(mf2, sg2) when mf1 = mf2 ->
					List.length sg1 = List.length sg2
			| LabelPrec l1, LabelPrec l2 ->
					list_suffix l2 l1
			| _ ->
					false
		and sgle_mem s =
			function
			| [] ->
					false
			| h:: t ->
					if precitem_eq s h
					then true
					else sgle_mem s t
		and both_mem =
			function
			| [] ->
					false
			| h:: t ->
					if precitem_eq c h
					then sgle_mem d t
					else if precitem_eq d h
					then sgle_mem c t
					else both_mem t
		
		and bs_spec_rel =
			function
			| [] ->
					()
			| h:: t ->
					( if both_mem h
						then upd_dir DontCare
						else (* not both of (c & d) are members of h *)
						if sgle_mem c h
						then (* c is, d is not member of h *)
						if List.exists (sgle_mem d) t
						then upd_dir Before
						else ()
						else (* c is not, d may be member of h *)
						if sgle_mem d h
						(* d is, c is not member of h *)
						then if List.exists (sgle_mem c) t
							then upd_dir After
							else ()
						else ()
					);
					bs_spec_rel t
		in
		List.iter bs_spec_rel data_prec_l;
		!dir
	in
	check_cl (List.fold_left
				( fun l c ->
							let precl = destruct_sync cid c in
							if snd precl = [] then l else precl:: l
				) [] sync_calls
		)

(* generate dependencies from the specification ... for an application *)
let rec distr2sgn maplist =
	function
	| [] ->
			maplist
	| d :: t ->
	
			( match d.sc_act with
				| Some(ActEmit(_, _, _, _, _, _, xs, ro)) ->
						let sgn = sig2sgn xs in
						let maplist =
							add_maplist (sgn,[(d.sc_sgn, sgn, sig2id xs, ro)]) maplist in
						distr2sgn maplist t
				| _ ->
						Err.intern "distr2sgn"
			)

let rec distr2ro maplist =
	function
	| [] ->
			maplist
	| (_, _, _, ro) as sy :: t ->
			let maplist = add_maplist (ro.trs_name,[sy]) maplist in
			distr2ro maplist t

let rec only_mult =
	function
	| [] ->
			[]
	| (_, syl) :: t ->
			let maplist = distr2ro [] syl in
			let t = only_mult t in
			if List.length maplist = 1
			then t
			else maplist:: t

let rec least_common_node inv_prefix =
	function
	| [] ,[] ->
			Err.intern "get_least_common_node"
	| [] , f2:: _ ->
			(List.rev inv_prefix, This, f2)
	| f1:: _ ,[] ->
			(List.rev inv_prefix, f1, This)
	| f1:: t1, f2:: t2 ->
			if f1 = f2 then
				least_common_node (f1:: inv_prefix) (t1, t2)
			else
				(List.rev inv_prefix, f1, f2)

let rec mapped2sgn sgn coro =
	function
	| [] ->
			Err.intern "mapped2sgn:[]"
	| ro:: rol ->
			if List.map fst ro.trs_name = coro then (
				(* look for all signals mapped to sgn *)
				let sel xs =
					let xs' = sig2prcsig xs in
					match xs'.rct_sgndcl with
					| None ->
							Err.intern "mapped2sgn:None"
					| Some rd ->
							if rd.sgn = sgn
							then Some xs.trs_sid
							else None
				in
				let signals = list_condense sel ro.trs_sig in
				(type2id ro.trs_type, signals)
			) else (
				(* try the next *)
				mapped2sgn sgn coro rol
			)

let rec determine_pre_post_l fld1 fld2 =
	function
	| [] ->
			0
	| s:: sl ->
	
			if fld1 = s
			then if List.mem fld2 sl
				then 1
				else 0
			else if fld2 = s
			then if List.mem fld1 sl
				then 2
				else 0
			else determine_pre_post_l fld1 fld2 sl

let rec determine_pre_post_ll dir cid fld1 fld2 (precl : tbsexpr list list) =
	match precl with
	| [] ->
			dir
	| sl:: sll ->
			let dir' = determine_pre_post_l fld1 fld2 sl in
			let call2str =
				function
				| Call(n, None) -> Ly.mf2str n
				| _ -> assert false in
			let dir = if dir = 0
				then dir'
				else if dir' = 0
				then dir
				else if dir = dir'
				then dir
				else
					let txt = "The reactive objects "^call2str fld1^
						" and "^ call2str fld2^
						" are involved in more than one \
					object precedence decl, which refer to the \
					same principal signal and are contradicting \
					to each other" in
					Err.msg (Err.InvalidPrec(cid, txt))
			in
			determine_pre_post_ll dir cid fld1 fld2 sll

let chk_mult3 rtl pre_post pot_race (d1, sgn1, sig1, ro1) (d2, sgn2, sig2, ro2) =
	let name1 = List.map fst ro1.trs_name
	and name2 = List.map fst ro2.trs_name in
	if sgn1 = sgn2
	then ()
	else Err.intern "chk_mult3:1";
	let (coro, fld1, fld2) = least_common_node [] (name1, name2) in
	let (cid, sigs) = mapped2sgn sgn1 coro rtl in
	let sel (s, prec) =
		if List.mem s sigs
		then Some prec
		else None
	in
	let precl = list_condense sel (i_obj_precl cid) in
	let dir = determine_pre_post_ll 0 cid fld1 fld2 precl in
	(* 0=dontknow; 1=fld1 < fld2; 2=fld2 < fld1 *)
	if dir = 0 then (
		pot_race (d1, PrecEnt4Appl(ro1, sig1)) (d2, PrecEnt4Appl(ro2, sig2))
	) else if dir = 1 then (
		pre_post d1 d2
	) else if dir = 2 then (
		pre_post d2 d1
	) else (
		Err.intern "chk_mult3:2"
	)

let rec chk_mult2 rtl pre_post pot_race sy = function
	| [] ->
			()
	| (_, syl):: t ->
	
			List.iter (chk_mult3 rtl pre_post pot_race sy) syl;
			chk_mult2 rtl pre_post pot_race sy t

let rec chk_mult1 rtl pre_post pot_race = function
	| [] ->
			()
	| (_, syl):: t ->
	
			List.iter (fun sy -> chk_mult2 rtl pre_post pot_race sy t) syl;
			chk_mult1 rtl pre_post pot_race t

let for_appl rtl sync_assgns pre_post pot_race =
	let maplist = distr2sgn [] sync_assgns in
	let maplist = only_mult maplist in
	List.iter (fun syl -> chk_mult1 rtl pre_post pot_race syl) maplist

(* -------------------------------------------------------------------------
check_labels cid_tocheck reactive_labellist_list
check, whether the precedence list is valid:
- all label exist & are used as labels (i.e. not as fields)
- all fields and methods / constructor exist & are used properly
- label - lists are valid suffixes of the label - list list generated from
the reactive part of a class
- the before / after relation between label - lists, fields and
methods / constructor is acyclic
prec_valid declfn precidlll ... precedence def acyclic & all id's exist
------------------------------------------------------------------------- *)
let suff_err cid l ll =
	let l = lst_sep2str lbl2str ":" l
	and ll = lst_sep2str (fun l -> lst_sep2str lbl2str ":" (List.rev l)) "\n" ll
	in
	let t = "Label-list "^l^" is no valid suffix of a full label as generated \
		from the reactive call tree of the reactive class. The full \
		label list of class "^c2str cid^" is:\n"^ll in
	raise (Failure t)

let chk_prec_atom cid rlbll_l =
	function
	| RhsFieldPrec _
	| LhsFieldPrec _
	| MethodPrec _ ->
			()
	| LabelPrec ls ->
			let ls' = List.rev ls in
			if List.exists (fun rl -> list_prefix ~pre: ls' ~full: rl) rlbll_l
			then ( (* valid suffix *) )
			else suff_err cid ls rlbll_l

let check_labels cid rlbll_l =
	let precl = i_class_precl cid in
	try List.iter (List.iter (List.iter (chk_prec_atom cid rlbll_l))) precl
	with Failure txt ->
			Err.msg (Err.InvalidPrec(cid, txt))
