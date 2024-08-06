open Ly
open P
open Ast
open Util
open Util_parse
open Graphic_util

(* --------------------------------------------------------------------------
global data for the graphic parser
-------------------------------------------------------------------------- *)
let states = ref ([] : (tmfid * tstate) list)
let all_st = ref ([] : (tmfid * tstate) list)
let all_go = ref ([] : tgraphic list)

let state2id st =
	let rec state2id = function
		| [] -> Err.intern "state2id"
		| (id, go):: l ->
				if st = go
				then id
				else (state2id l)
	in
	state2id !all_st

let id2state stid =
	let rec id2state = function
		| [] -> Err.intern "id2state"
		| (id, st):: l ->
				if stid = id
				then st
				else (id2state l)
	in
	id2state !all_st

(* --------------------------------------------------------------------------
for error - messages
-------------------------------------------------------------------------- *)
let go_being_visited go =
	s.p_goid <- go2id go

let mk_lbl go =
	Ly.int_lbl (GraphicPos(s.p_graphic_file, go2id go))

(* --------------------------------------------------------------------------
parse texts
-------------------------------------------------------------------------- *)
let parse_trans t =
	go_being_visited (Trans t);
	Lex_wrapper.parse_string Yacc.prs_trans t.tr_txt

let parse_condtrans t =
	go_being_visited (Trans t);
	Lex_wrapper.parse_string Yacc.prs_condtrans t.tr_txt

let parse_inittrans t =
	go_being_visited (Trans t);
	Lex_wrapper.parse_string Yacc.prs_inittrans t.tr_txt

let parse_statename st =
	go_being_visited (State st);
	Lex_wrapper.parse_string Yacc.prs_id st.st_name

let parse_actions st =
	go_being_visited (State st);
	Lex_wrapper.parse_string Yacc.prs_stateaction st.st_actions

let parse_cond c =
	go_being_visited (Cond c);
	Lex_wrapper.parse_string Yacc.prs_cond c.co_txt

(* --------------------------------------------------------------------------
mk_graphic_parse_3 file_name ... the real load of graphics
-------------------------------------------------------------------------- *)
let mk_graphic_parse_3 file_name =
	let incha = open_in_bin file_name in
	if (input_value incha : float) = (float_of_string se.ge_version)
	then (
		let _ = (input_value incha : int)  (* ignore irrelevant data *)
		and _ = (input_value incha : int)
		and _ = (input_value incha : string) in
		all_go := (input_value incha : tgraphic list);
		close_in incha;
		if debug_level DbgGoLoad
		then List.iter P.dump_go (!all_go)
		else ()
	)
	else (
		close_in incha; Err.msg (Err.InvalidVersion(file_name))
	)

(* --------------------------------------------------------------------------
select_go fct ... return all go's for which ''fct'' evaluates to true
-------------------------------------------------------------------------- *)
let rec select_go_ofgol fct =
	function
	| [] -> []
	| go:: gol ->
			if (fct go)
			then (go :: (select_go_ofgol fct gol))
			else (select_go_ofgol fct gol)

let select_go fct = select_go_ofgol fct !all_go

(* --------------------------------------------------------------------------
remove go gol ... return the GO - list built from ''gol'' by removing ''go'
-------------------------------------------------------------------------- *)
let rec remove go =
	function
	| [] -> []
	| (_, g):: gol when g = go -> gol
	| ig:: gol -> ig:: (remove go gol)

(* --------------------------------------------------------------------------
toplevel_state stl st ...
true if state - GO ''st'' is not inside a state - GO from ''stl''
-------------------------------------------------------------------------- *)
let toplevel_state stl st =
	List.for_all
		( fun s ->
					not( ( s.st_id = st.st_id ) || state_in_ra s.st_parent s.st_x s.st_y s.st_x' s.st_y' st)
		) stl

(* --------------------------------------------------------------------------
generate and - or decomposition of state ''st''
-------------------------------------------------------------------------- *)
let get_frozen st parent =
	let rec bs_get_frozen =
		function
		| [] -> Err.intern "get_frozen"
		| (_, go):: stl ->
				if go.st_parent = parent && go.st_frozen && go.st_name = st.st_name
				then ( states := remove go !states; go )
				else ( bs_get_frozen stl )
	in
	bs_get_frozen !states

let gen_or parent x y x' y' =
	let state_in_ra = state_in_ra parent x y x' y' in
	let states_in_ra =
		List.fold_left
			( fun sl is -> let s = snd is in
						if (state_in_ra s)
						then (s:: sl)
						else (sl)
			) [] !states
	in
	let toplevel_state = toplevel_state states_in_ra in
	List.fold_left
		( fun orl s ->
					if (toplevel_state s)
					then (s:: orl)
					else (orl)
		) [] states_in_ra

let rec gen_and st kind start =
	function
	| [] ->
			if kind = 0
			then [gen_or st.st_parent st.st_x start st.st_x' st.st_y']
			else [gen_or st.st_parent start st.st_y st.st_x' st.st_y']
	| s:: sl ->
			if kind = 0 then (
				(gen_or st.st_parent st.st_x start st.st_x' (st.st_y + s))
				:: (gen_and st kind (st.st_y + s) sl)
			) else (
				(gen_or st.st_parent start st.st_y (st.st_x + s) st.st_y')
				:: (gen_and st kind (st.st_x + s) sl)
			)

let rec gen_andor st =
	function
	| AndRefine(_,[]) ->
			[gen_or st.st_parent st.st_x st.st_y st.st_x' st.st_y']
	| AndRefine(k, sl) ->
			gen_and st k st.st_x sl
	| Graphic(p) ->
			let fz = get_frozen st p in
			gen_andor fz fz.st_refine
	| Textual _ -> []

(* --------------------------------------------------------------------------
transition processing
- this_trans_from go ...
- this_trans_to go ...
- all_trans selection_function ...

- all_trans_from go
- all_trans_to go
- all_trans_of go
-------------------------------------------------------------------------- *)
let this_trans_from go =
	function
	| Trans(t) -> t.tr_from = go
	| _ -> false

let this_trans_to go =
	function
	| Trans(t) -> t.tr_to = go
	| _ -> false

let all_trans self =
	List.fold_left
		( fun tl t ->
					if (self t)
					then (go2trans t:: tl)
					else (tl)
		) [] !all_go

let all_trans_from go =
	all_trans (this_trans_from go)

let all_trans_to go =
	all_trans (this_trans_to go)

let all_trans_of go =
	all_trans (fun t -> (this_trans_from go t) || (this_trans_to go t))

(* --------------------------------------------------------------------------
process_all_trans st orstl ... transitions originating in state ''st''
-------------------------------------------------------------------------- *)
let get_then =
	function
	| Then(lbl, e, sl) -> (lbl, e, sl)
	| _ -> Err.intern "get_then"

let is_true e = e.expr = l_true
let is_false e = e.expr = l_false

let rec process_ctrans st orstl trans =
	let trto = trans.tr_to in
	let process_trans = process_trans st orstl in
	if (is_state trto)
	then
		if not( List.mem (go2state trto) orstl )
		then Err.msg (Err.Transition(st.st_name, (go2state trto).st_name))
		else (NextState(Ly.nolbl, state2id (go2state trto)))
	else if (is_exit trto)
	then NextState(Ly.nolbl, id_exit)
	else if (is_cond trto)
	then
		match (all_trans_from trto) with
		| [t1; t2] ->
				let (_, e, _) = get_then (parse_cond (go2cond trto))
				and (lbl1, e1, rl1) = get_then (process_trans t1).tr_trans
				and (lbl2, e2, rl2) = get_then (process_trans t2).tr_trans in
				if not (is_true e1 || is_false e1) || not (is_true e2 || is_false e2) || (e1 = e2)
				then Err.msg (Err.ConditionTrans(st.st_name))
				else
					let sl1 = if is_true e1 then rl1 else rl2
					and sl2 = if is_true e1 then rl2 else rl1 in
					let lbl1 = if is_true e1 then lbl1 else lbl2
					and lbl2 = if is_true e1 then lbl2 else lbl1 in
					let thn = [Then(lbl1, e, sl1); Else(lbl2, sl2)] in
					(ExprStmt(nolbl,{ etyp = None; expr = If(thn); elbl = nolbl }))
		| _ -> Err.msg (Err.ConditionTrans(st.st_name))
	else Err.intern "process_ctrans"

and process_trans st orstl trans =
	let lbl = mk_lbl (Trans trans) in
	let pt =
		if (is_cond trans.tr_from)
		then (parse_condtrans trans)
		else (parse_trans trans) in
	let (_, e, rct_il) = get_then pt.tr_trans in
	let rct_il' = [process_ctrans st orstl trans] in
	{ tr_priority = pt.tr_priority;
		tr_trans = Then(lbl, e, rct_il@rct_il')
	}

let rec priority_ok of_state = function
	| [] -> ()
	| [t] -> ()
	| t1:: t2:: tl ->
			if (t1.tr_priority = t2.tr_priority)
			then Err.msg (Err.TransPriority(of_state))
			else (priority_ok of_state (t2:: tl))

let process_all_trans st orstl =
	let tl = all_trans_from (State st) in
	let cl = List.map (process_trans st orstl) tl in
	let sl = Sort.list (fun t1 t2 -> t1.tr_priority <= t2.tr_priority) cl in
	priority_ok st.st_name sl;
	List.map (fun pr -> pr.tr_trans) sl

(* --------------------------------------------------------------------------
process_state st stl ... generate AST for state - GO ''st''
''stl'' is the list of all or - states
or_init ... process init of one or - part of the and of state
ck_trans_into_orset ... true if all target - states of a transition
remains in the state - list ''orl''
select_init_trans ... get the init of one or - part
-------------------------------------------------------------------------- *)
type threevalued =
	| Unknown
	| IntoOrL
	| OutOfOrL

let mk_now tvo tvv =
	if tvo = Unknown || tvv = Unknown || tvo = tvv
	then tvv
	else raise (Invalid_argument "")

let alltrans_into_orset = ref Unknown                     (* local of or_init *)

let init_err () =
	prs_err "reactive graphic with invalid init-i, exit-, or  conditional-item"

let or_init st orl =
	let rec ck_trans_into_orset orl =
		function
		| State(s) ->
				alltrans_into_orset := (
					if (List.mem s orl)
					then (mk_now !alltrans_into_orset IntoOrL)
					else (mk_now !alltrans_into_orset OutOfOrL)
				)
		| Cond(c) ->
				List.iter
					( fun t -> ck_trans_into_orset orl t.tr_to) (all_trans_from (Cond c))
		| _ -> Err.intern "or_init"
	and select_init_trans go =
		(is_trans go) &&
		let t = go2trans go in
		try alltrans_into_orset := Unknown;
			(is_init t.tr_from) &&
			(ck_trans_into_orset orl t.tr_to; !alltrans_into_orset = IntoOrL )
		with Invalid_argument "" ->
				go_being_visited go;
				init_err ()
	in
	match (all_trans (select_init_trans)) with
	| [t] -> (parse_inittrans t) @ [process_ctrans st orl t]
	| _ -> Err.msg (Err.Inits(state2id st))

let rec process_or st orl =
	let lbl = mk_lbl (State st) in
	let or_init = or_init st orl in
	let st_init =
		{ slbl = lbl; sname = id_init;
			strans =[Else(nolbl, or_init)];
			sentry =[]; sduring =[]; sexit =[]; sdo =[]
		} in
	let st_ste = List.map (fun s -> process_state s orl) orl in
	[TextStM(lbl,{ a_init = st_init; a_states = st_ste })]

and process_state st orstl = (* orstatel includes st *)
	states := remove st !states;
	go_being_visited (State st);
	let stid = state2id st in
	let lbl = mk_lbl (State st) in
	let andor = gen_andor st st.st_refine in
	let acts = parse_actions st in
	let acdo = stact_do acts in
	{ sname = stid;
		slbl = lbl;
		sentry = stact_entry acts;
		sduring = stact_during acts;
		sexit = stact_exit acts;
		sdo =
			if andor = [[]] && acdo = []
			then []
			else if andor = [[]]
			then acdo
			else ( let andor = List.map (process_or st) andor in
				if acdo = []
				then [Par(lbl, andor)]
				else [Par(lbl, acdo:: andor)]
			);
		strans = process_all_trans st orstl
	}

(* --------------------------------------------------------------------------
generate and - or decomposition of state ''st''
-------------------------------------------------------------------------- *)
let hierarchy_gen_or parent x y x' y' =
	let state_in_ra = state_in_ra parent x y x' y' in
	let states_in_ra =
		List.fold_left
			( fun sl is ->
						let s = snd is in
						if (state_in_ra s) then (s:: sl) else (sl)
			) [] !states in
	let toplevel_state = toplevel_state states_in_ra in
	List.fold_left
		( fun orl s ->
					if (toplevel_state s) then (s:: orl) else (orl)
		) [] states_in_ra

let rec hierarchy_gen_and st kind start =
	function
	| [] ->
			if kind = 0
			then [hierarchy_gen_or st.st_parent st.st_x start st.st_x' st.st_y']
			else [hierarchy_gen_or st.st_parent start st.st_y st.st_x' st.st_y']
	| s:: sl ->
			if kind = 0
			then ( (gen_or st.st_parent st.st_x start st.st_x' (st.st_y + s))
				:: (gen_and st kind (st.st_y + s) sl)
			) else (
				(gen_or st.st_parent start st.st_y (st.st_x + s) st.st_y')
				:: (gen_and st kind (st.st_x + s) sl)
			)

let rec hierarchy_gen_andor st =
	function
	| AndRefine(_,[]) -> [hierarchy_gen_or st.st_parent st.st_x st.st_y st.st_x' st.st_y']
	| AndRefine(k, sl) -> gen_and st k st.st_x sl
	| Graphic(p) ->
			let fz = get_frozen st p in
			hierarchy_gen_andor fz fz.st_refine
	| Textual _ -> []

let hierarchy_or_init st orl =
	let rec ck_trans_into_orset orl =
		function
		| State(s) ->
				alltrans_into_orset :=
				if (List.mem s orl)
				then (mk_now !alltrans_into_orset IntoOrL)
				else (mk_now !alltrans_into_orset OutOfOrL)
		| Cond(c) ->
				List.iter
					( fun t -> ck_trans_into_orset orl t.tr_to )
					(all_trans_from (Cond c))
		| _ -> Err.intern "or_init"
	and select_init_trans go =
		(is_trans go) &&
		let t = go2trans go in
		try
			alltrans_into_orset := Unknown;
			(is_init t.tr_from) &&
			(ck_trans_into_orset orl t.tr_to; !alltrans_into_orset = IntoOrL )
		with Invalid_argument "" ->
				go_being_visited go;
				init_err ()
	in
	match (all_trans (select_init_trans)) with
	| [t] -> (parse_inittrans t) @ [process_ctrans st orl t]
	| _ -> Err.msg (Err.Inits(state2id st))

let rec hierarchy_process_or st orl =
	let lbl = mk_lbl (State st) in
	let or_init = or_init st orl in
	let st_init =
		{ slbl = lbl; sname = id_init;
			strans =[Else(nolbl, or_init)];
			sentry =[]; sduring =[]; sexit =[]; sdo =[]
		} in
	let st_ste = List.map (fun s -> process_state s orl) orl in
	[TextStM(lbl,{ a_init = st_init; a_states = st_ste })]

and hierarchy_process_state st orstl = (* orstatel includes st *)
	states := remove st !states;
	go_being_visited (State st);
	let stid = state2id st in
	let lbl = mk_lbl (State st) in
	let andor = gen_andor st st.st_refine in
	let acts = parse_actions st in
	let acdo = stact_do acts in
	{ sname = stid;
		slbl = lbl;
		sentry = stact_entry acts;
		sduring = stact_during acts;
		sexit = stact_exit acts;
		sdo =
			if andor = [[]] && acdo = []
			then []
			else if andor = [[]]
			then acdo
			else (
				let andor = List.map (process_or st) andor in
				if acdo = []
				then [Par(lbl, andor)]
				else [Par(lbl, acdo:: andor)]
			);
		strans = process_all_trans st orstl
	}

(* --------------------------------------------------------------------------
check_inex_single_trans go ... inex - nodes are either source or target of
exactly one transition
check_cond_wellformed go ... one input to cond req'd (at least))
-------------------------------------------------------------------------- *)
let check_init_single_trans init =
	if (is_init init)
	then (
		go_being_visited init;
		match (all_trans_of init) with
		| [t] ->
				if (is_exit t.tr_to)
				then init_err ()
				else ( )
		| _ -> init_err ()
	)
	else ()

let check_exit_single_trans exit =
	if (is_exit exit)
	then (
		go_being_visited exit;
		match (all_trans_to exit) with
		| [t] ->
				if (is_init t.tr_from)
				then init_err ()
				else ( )
		| _ -> init_err ()
	)
	else ()

let check_cond_wellformed cond =
	if (is_cond cond)
	then (
		go_being_visited cond;
		if (all_trans_to cond) = []
		then init_err ()
		else ( )
	)
	else ()

(* --------------------------------------------------------------------------
mk_graphic_parse ast
-------------------------------------------------------------------------- *)
let mk_graphic2automaton file =
	init_parse_graphic file;
	(* init_parse_graphic MUST ALWAYS be complemented by exit_parse_graphic   *)
	mk_graphic_parse_3 file;
	List.iter check_init_single_trans !all_go;
	List.iter check_exit_single_trans !all_go;
	List.iter check_cond_wellformed !all_go;
	states := List.fold_left
		( fun stl go ->
					if (is_state go)
					then let st = go2state go in
						let stid = parse_statename st in
						(stid, st):: stl
					else stl
		) [] !all_go;
	all_st := !states;
	let lc = id2state id_life_cycle in           (* start with "lifecycle" *)
	let fsm = process_state lc [lc] in
	if not( !states = [] ) then (
		(* irregular exit: exit_parse_graphic () called elsewhere *)
		Err.msg (Err.UnusedStates(List.map (fst) !states))
	) else (
		exit_parse_graphic ();
		if fsm.sentry =[] && fsm.sduring =[] && fsm.sexit =[]
		then
			if fsm.strans =[]
			then ()
			else Err.intern "mk_graphic2automaton"
		else Err.msg (Err.InvalidLifecycle file);
		match fsm.sdo with
		| [s] -> s
		| _ -> Err.intern "mk_graphic2automaton:dcmp"
	)
(* REMEMBER:
if an EXCEPTION escapes the regular flow of control, it must be
GUARANTEED, that exit_parse_graphic () is called.
THIS IS DONE AFTER ERROR REPORTING in puctrl.ml
*)

let mk_graphic_parse gstm =
	let file = gstm.gstm_file in
	if not( Sys.file_exists file ) then (
		Err.msg (Err.FileName(file^" (reactive graphic)"))
	) else (
		let name = mf2id gstm.gstm_file in
		mk_graphic_call name [mk_graphic2automaton file];
		gstm.gstm_mf <- Some name
	)
