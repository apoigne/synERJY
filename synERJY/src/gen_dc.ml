open Ast
open Ly
open P
open Util
open Util_print

let tick = React2sya.Sc.tick
(* -------------------------------------------------------------------------
access functions for the tables
- table_clear table
- table_add table entry
- table_get table an_unused_key number
- table_iter table an_unused_key
(fun entry number -> unit) with increasing numbers
the unused_key must be an unused key in the hashtable - inconvenient design

further here the names for the tables to be produced are introduced
. table . entries . sE - name dc - number .
------------------------------------------------------------------------- *)

let typs_tbl = Hashtbl.create 53
let cnst_tbl = Hashtbl.create 53
let fnct_tbl = Hashtbl.create 53
let flow_tbl = Hashtbl.create 1991

class ['a,'b] dc_counter (tbl: ('a,'b)Hashtbl.t) =

object (self)
	val mutable c = - 1;
	method get key =
		try Hashtbl.find tbl key
		with Not_found ->
				c <- c + 1;
				let e = i2s c in
				Hashtbl.add tbl key e;
				e
	method set x = c <- x
	method reset = c <- - 1; self#clear
	method add x y = Hashtbl.add tbl (x:'a) (y:'b)
	method clear = Hashtbl.clear tbl
end

let typs = new dc_counter typs_tbl
let cnst = new dc_counter cnst_tbl
let fnct = new dc_counter fnct_tbl
let flow = new dc_counter flow_tbl
(* let proc = new dc_tbl (Hashtbl.create 1991) "proc"
let node = new dc_tbl (Hashtbl.create 1991) "node"
*)

(* -------------------------------------------------------------------------
predefined types
------------------------------------------------------------------------- *)
let _ = typs#add t_bool "$0";
	typs#add t_int "$1";
	typs#add t_float "$2";
	typs#add t_double "$3"

(* -------------------------------------------------------------------------
predefined constants
------------------------------------------------------------------------- *)
let _ =
	cnst#add (t_bool, id_false) "$@0";
	cnst#add (t_bool, id_true) "$@1"

(* -------------------------------------------------------------------------
predefined functions
------------------------------------------------------------------------- *)
let _ = fnct#add (t_bool, mf2id "if if") "$0";
	fnct#add (t_bool, id_op_equal) "$1";
	fnct#add (t_bool, id_op_not_equal) "$2";
	fnct#add (t_bool, id_op_log_or) "$3";
	fnct#add (t_bool, id_op_log_and) "$4";
	fnct#add (t_bool, id_op_not) "$5";
	(* no exclusive *)
	(* ---- Integer ---- *)
	fnct#add (t_int, id_op_equal) "$7";
	fnct#add (t_int, id_op_not_equal) "$8";
	fnct#add (t_int, id_op_lt) "$9";
	fnct#add (t_int, id_op_le) "$10";
	fnct#add (t_int, id_op_gt) "$11";
	fnct#add (t_int, id_op_ge) "$12";
	fnct#add (t_int, id_op_add) "$13";
	fnct#add (t_int, id_op_sub) "$14";
	fnct#add (t_int, id_op_mult) "$15";
	fnct#add (t_int, id_op_mod) "$16";
	fnct#add (t_int, id_op_div) "$17";
	fnct#add (t_int, id_op_minus) "$18";
	(* ---- Real ---- *)
	fnct#add (t_float, id_op_equal) "$19";
	fnct#add (t_float, id_op_not_equal) "$20";
	fnct#add (t_float, id_op_lt) "$21";
	fnct#add (t_float, id_op_le) "$22";
	fnct#add (t_float, id_op_gt) "$23";
	fnct#add (t_float, id_op_ge) "$24";
	fnct#add (t_float, id_op_add) "$25";
	fnct#add (t_float, id_op_sub) "$26";
	fnct#add (t_float, id_op_mult) "$27";
	fnct#add (t_float, id_op_div) "$28";
	fnct#add (t_float, id_op_minus) "$29";
	(* ---- Double ---- *)
	fnct#add (t_double, id_op_equal) "$30";
	fnct#add (t_double, id_op_not_equal) "$31";
	fnct#add (t_double, id_op_lt) "$32";
	fnct#add (t_double, id_op_le) "$33";
	fnct#add (t_double, id_op_gt) "$34";
	fnct#add (t_double, id_op_ge) "$35";
	fnct#add (t_double, id_op_add) "$36";
	fnct#add (t_double, id_op_sub) "$37";
	fnct#add (t_double, id_op_mult) "$38";
	fnct#add (t_double, id_op_div) "$39";
	fnct#add (t_double, id_op_minus) "$40";
	(* ---- Conversion operators ---- *)
	fnct#add (t_int, id_op_to_float) "$41";
	fnct#add (t_float, id_op_to_float) "$42";
	fnct#add (t_double, id_op_to_double) "$43"

(* -------------------------------------------------------------------------
dc_print_table: generic print function for a DC table
------------------------------------------------------------------------- *)
let dc_print_tbl kind offset start entry_list print =
	let length = List.length entry_list
	and lst = ref entry_list in
	if length > 0
	then (
		pnT offset; ps kind; ps ": "; pi length;
		for i = start to start + length - 1 do
			pnT (offset + 1);
			pi i; ps ": ";
			print i (List.hd !lst);
			lst := List.tl !lst
		done;
		pnT offset;
		ps "end:"; ps " -- "; ps kind
	)

(* -------------------------------------------------------------------------
dc_print_types_tbl
------------------------------------------------------------------------- *)
let dc_print_type_tbl tl =
	let p _ x = ps (typs#get x) in
	dc_print_tbl "types" 4 0 tl p

(* -------------------------------------------------------------------------
dc_print_constants_tbl
------------------------------------------------------------------------- *)
let dc_print_const_tbl tl cl =
	let p _ (t, x) = ps (cnst#get (t, x)) in
	dc_print_tbl "const" 4 0 cl p

(* -------------------------------------------------------------------------
dc_print_function_tbl
------------------------------------------------------------------------- *)
let dc_print_function_tbl fl =
	let param2str (n, t) = (i2s n)^" "^(mf2str t) in
	let p _ (n, il, o, t, p) =
		ps (fnct#get (t, n));
		lst_sep2BpsB "(" ")" ps "," (List.map param2str il);
		ps (param2str o);
		ps p; ps ":"
	in
	dc_print_tbl "functions" 4 0 fl p

(* -------------------------------------------------------------------------
dc_print_procedure_tbl
-------------------------------------------------------------------------
let dc_print_procedure_tbl pl =
let param2str (n, t) =
(i2s n)^" "^(mf2str t) in
let p _ (n, il, ol, t, p) =
ps (proc#get (t, n));
lst_sep2BpsB "(" ")" ps "," (List.map param2str il);
lst_sep2BpsB "(" ")" ps "," (List.map param2str ol);
ps p; ps ":"
in
dc_print_tbl "functions" 4 0 pl p *)

(* -------------------------------------------------------------------------
dc_print_flow_tbl
------------------------------------------------------------------------- *)
let dc_print_flow_tbl kind start fl =
	let p _ (t, f, c, v) =
		ps (flow#get f); ps " "; ps "$0"(*;ps (flow#get c)*);
		( match v with
			|	None -> ()
			|	Some v -> ps " value: "; ps (cnst#get (t, v))
		);
		ps " ; "
	in
	dc_print_tbl kind 1 start fl p

(* -------------------------------------------------------------------------
dc_print_def_tbl: generate DC definition table
------------------------------------------------------------------------- *)
let dc_p_sc_exp e =
	let rec e2s =
		function
		| S x -> flow#get x
		| TT -> "@$0"
		| FF -> "@$1"
		| NNot x -> "$5("^(e2s x)^")"
		| AAnd [] -> "@$1"
		| AAnd el -> "$3("^(le2s "$3" el)^")"
		| OOr [] -> "@$0"
		| OOr el -> "$4("^(le2s "$4" el)^")"
		| CC _ -> Err.intern "dc_print_def_tbl"
	and le2s op =
		function
		| [] -> ""
		| [e] -> e2s e
		| [e1; e2] -> (e2s e1)^","^(e2s e2)
		| e:: el -> op^"("^(e2s e)^","^(le2s op el)^")"
	in
	e2s e

let dc_print_def_tbl kind def_l =
	let p_def _ d =
		if React2sya.Sc.is_reg d.sc_sgn
		then ps "memo: "
		else ps "equ:  ";
		ps (flow#get d.sc_sgn); ps "  "; ps (dc_p_sc_exp d.sc_def);
		ps " at: "; ps "@"; ps (flow#get d.sc_trg);
		ps " ;"
	in
	if kind = "extern"
	then ( pnT 1; ps "extern:" )
	else ( dc_print_tbl kind 1 0 def_l p_def )

(* -------------------------------------------------------------------------
dc_print_dependance_tbl: generate DC node table of bool flows only
------------------------------------------------------------------------- *)
let dc_print_dpnd_tbl dpnds_l =
	let p_dependency _ (dep, cause, f) =
		lst_sep2BpsB "" "" ps "," (List.map flow#get dep);
		ps " depends: ";
		lst_sep2BpsB "" "" ps "," (List.map flow#get cause);
		ps " at: "; ps (flow#get f)
	in
	dc_print_tbl "dependencies" 4 0 dpnds_l p_dependency

(* -------------------------------------------------------------------------
dc_print_node_tbl: generate DC node table of bool flows only
------------------------------------------------------------------------- *)
type dc_node =
	{
		dc_node : string;
		dc_inflws : (ttype * tsc_id * tsc_id * tmfid option) list;
		dc_outflws : (ttype * tsc_id * tsc_id * tmfid option) list;
		dc_assflws : (ttype * tsc_id * tsc_id * tmfid option) list;
		dc_locflws : (ttype * tsc_id * tsc_id * tmfid option) list;
		dc_defs : tsc_ctrl list;
		dc_ass_defs : tsc_ctrl list;
		dc_dpnds : (tsc_id list * tsc_id list * tsc_id) list;
	}

let dc_print_node_tbl name node_l =
	let p_node cnt node =
		let l x = List.length x in
		let l_inp = 0 in
		let l_out = l node.dc_inflws in
		let l_ass = l_out + (l node.dc_outflws) in
		let l_loc = l_ass + (l node.dc_assflws) in
		let ll = (l node.dc_inflws) + (l node.dc_outflws) +
			(l node.dc_assflws) + (l node.dc_locflws) in
		ps name; ps "_"; pi cnt; pnT 1; ps "flows: "; pi ll;
		dc_print_flow_tbl "inflows" l_inp node.dc_inflws;
		dc_print_flow_tbl "outflows" l_out node.dc_outflws;
		dc_print_flow_tbl "assert" l_ass node.dc_assflws;
		dc_print_flow_tbl "locflows" l_loc node.dc_locflws;
		dc_print_def_tbl "definitions" node.dc_defs;
		dc_print_def_tbl "assertions" node.dc_ass_defs;
		dc_print_dpnd_tbl node.dc_dpnds
	in
	dc_print_tbl "nodes" (- 1) 0 node_l p_node

(* -------------------------------------------------------------------------
dc_print_package_tbl: generate DC package table of bool flows only
------------------------------------------------------------------------- *)
let dc_print_package_tbl name sub_pckgs typ_l feat_l node_l =
	ps "package: "; ps name;
	if sub_pckgs <> []
	then ( dc_print_tbl "packages" 0 0 sub_pckgs (fun _ s -> ps s) )
	else ();
	(* pnT 2;ps "actions: ";ps (name^".acts"); *)
	dc_print_type_tbl typ_l;
	(* dc_print_fnct_tbl feat_l; *)
	dc_print_node_tbl name node_l;
	ps "\nendpackage:\n"

(* ==========================================================================
GENERATE the DC - format for the DATA PART of an application
==========================================================================*)
let dc_gen_types cid = ()

(* ==========================================================================
GENERATE the DC - format for the REACTIVE PART of an application
==========================================================================*)
let get_typ xs =
	match sig2valtyp xs with
	| Null -> t_bool
	| _ -> Err.msg (Err.NYI "valued input in DC-format not yet implemented")

let external_inputs sa =
	List.fold_left
		( fun l xs ->
					if is_input_sig xs then (
						let typ = get_typ xs in
						(typ, sig2sgn xs, (sig2frq xs).ff, None):: l
					) else (
						l
					)
		) [] sa.sa_sigs

let internal_inputs sa =
	[1; 2; 3]@
	(List.fold_right
			( fun c l ->
						match c.sc_act with
						| None -> l
						| Some a ->
								match a with
								| ActBool(_, b, _, _) -> b:: l
								| ActTime(_, b, _, _, _) -> b:: l
								| _ -> l
			) sa.sa_eqns [])

let external_outputs sa =
	List.fold_right
		( fun xs l ->
					if is_output_sig xs then (
						let typ = get_typ xs in
						(typ, sig2sgn xs, (sig2frq xs).ff, None):: l
					) else (
						l
					)
		) sa.sa_sigs []

let external_locals sa =
	List.fold_right
		( fun xs l ->
					if not( is_input_sig xs || is_output_sig xs) then (
						let typ = get_typ xs in
						(typ, sig2sgn xs, (sig2frq xs).ff, None):: l
					) else (
						l
					)
		) sa.sa_sigs []

let internal_locals sa =
	List.fold_right
		( fun d l ->
					if React2sya.Sc.is_inp d.sc_sgn || React2sya.Sc.is_ext d.sc_sgn
					then l
					else (t_bool, d.sc_sgn, d.sc_trg, None):: l
		) sa.sa_eqns []

let registers sa =
	List.fold_right
		( fun d l ->
					(t_bool, d.sc_sgn, tick, None):: l
		) sa.sa_mems []

(* -------------------------------------------------------------------------
list all the calls
------------------------------------------------------------------------- *)
let dc_gen_call = ()

(* -------------------------------------------------------------------------
generate the dependencies
------------------------------------------------------------------------- *)
let dc_gen_dep (x, y) = (flow#get y, flow#get x, 0)

(* ------------------------------------------------------------------------
generate DC
------------------------------------------------------------------------- *)
let make_dc () =
	typs#reset;
	cnst#reset;
	fnct#reset;
	flow#reset;
	(* proc#reset;
	node#reset; *)
	let sa =
		match Gen_application.ac.Gen_application.ac_sca with
		| None -> Err.msg Err.NoCode
		| Some sa -> sa in
	let cc = String.lowercase (c2str (Util.type2id se.conf_class)) in
	let fn = se.file_prefix^cc^".dc" in
	let oc = open_out fn in
	push_print_fct (output_string oc);
	try
		dc_print_package_tbl
			cc
			[]  (* *)
			[]  (* list of externally defined types *)
			[]
			[
			{ dc_node = cc;
				dc_inflws = external_inputs sa;
				dc_outflws = external_outputs sa;
				dc_assflws = [];
				dc_locflws = (external_locals sa)
					@(internal_locals sa)
					@(registers sa);
				dc_defs = sa.sa_eqns@sa.sa_mems;
				dc_ass_defs = []  (* assertions *);
				dc_dpnds = []  (* was: List.map dc_gen_dep sc.sc_prec *)
			}
			] ;
		pop_print_fct ();
		close_out oc;
		ps "\nDC generated\n"
	with x ->
			pop_print_fct ();
			close_out oc;
			ps "\nerror during generation of DC (\"";
			ps (Printexc.to_string x);
			ps "\")\n"

let make_dc () =
	try
		se.target_sys <- Simulation;
		Gen_application.make ();
		make_dc ()
	with
	| Error(t) -> pn (); ps t; pF ()
	| x -> ps "make_dc"; ps (Printexc.to_string x)
