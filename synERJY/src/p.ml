open Ly
open Ast

(* ==========================================================================
the basic print functions are supplied here.
They may be directed to stdout, to a Tk - window or to a file.
This is effected by implementing all print - functions with the very basic
functions:
- bs_ps (output of a string)
- bs_clr (clearing the screen, if any)
- bs_flush (forcing output to appear)
These functions are implemented as a reference to either a
- stdout version, supplied in this module
- tk - version, supplied in module tk.ml
Switching between these variants is done by the functions
- init_output2wdw
- stop_output2wdw implemented in this module

The separation into two modules and the allocation of function to modules
is necessary, because:
- two variants of eE are needed: one with and one without Tk (e.g. debug)
- a problem of ocaml's module systems (cyclic calls between modules are
not trivial)
========================================================================== *)
let impl_bs s = (print_string) s
let impl_clr () = ()
let impl_flush () = flush stdout

let bs_ps = ref impl_bs
let bs_clr = ref impl_clr
let bs_flush = ref impl_flush

let start_output2wdw ref_ps ref_clr ref_flush =
	bs_ps := ref_ps;
	bs_clr := ref_clr;
	bs_flush := ref_flush

let stop_output2wdw () =
	bs_ps := (impl_bs);
	bs_clr := (impl_clr);
	bs_flush := (impl_flush)

(* --------------------------------------------------------------------------
output may be directed to files. The ''old'' output function is remembered
in the reference cell ''old_bs_ps'', togehther with a flag, whether the
cell is free or occupied ''old_bs_free''
-------------------------------------------------------------------------- *)
let old_bs_ps = ref (print_string)
let old_bs_free = ref true

let primary_ps s =
	if (!old_bs_free)
	then (!bs_ps) s
	else (!old_bs_ps) s

let push_print_fct fct =
	if (!old_bs_free)
	then (
		old_bs_free := false;
		old_bs_ps := (!bs_ps);
	);
	bs_ps := fct

let pop_print_fct () =
	if (not !old_bs_free)
	then (
		old_bs_free := true;
		bs_ps := (!old_bs_ps)
	)

(* ==========================================================================
basic print functions, may be directed to stdout, to a Tk - window or to a file
========================================================================== *)
let pC () = (!bs_clr) ()
let pF () = (!bs_flush) ()
let pn () = (!bs_ps) "\n"
let pT i = if (i > 0) then for x = 1 to i do (!bs_ps) "  " done else ()
let pnT i = pn(); if (i <= 0) then () else (pT i)
let ps s = (!bs_ps) s
let psn s = (!bs_ps) s; pn()
let psc c = (!bs_ps) (c2str c)
let psmf mf = (!bs_ps) (mf2str mf)
let pc () = (!bs_ps) ", "
let psp () = (!bs_ps) " "
let pi i = (!bs_ps) (string_of_int i)
let pf f = (!bs_ps) (string_of_float f)
let pb b = (!bs_ps) (if b then "true" else "false")

let ps_opt =
	function
	| None -> ps "-None-"
	| Some (s: string) -> ps s

let pl_basic fx tab l =
	let sep = if tab <= 0
		then (fun () -> ps " " )
		else (fun () -> pnT tab)
	in
	let rec pl_basic =
		function
		| [] -> ()
		| [h] -> fx h
		| h:: l -> fx h; sep (); pl_basic l
	in
	pT tab;
	pl_basic l

let pl_string tab l = (pl_basic (ps) tab l)
and pl_int tab l = (pl_basic (pi) tab l)
and pl_bool tab l = (pl_basic (pb) tab l)
and pl_float tab l = (pl_basic (pf) tab l)
and pl_symb tab l = (pl_basic (psmf) tab l)

(* -------------------------------------------------------------------------
dump_go go ... dump a graphical objects for debugging purposes
output is to STDERR, independant from STDOUT (tk!)
------------------------------------------------------------------------- *)
let dump_go =
	let trans =
		function
		| State s -> s.st_id
		| Init i -> i.ie_id
		| Exit i -> i.ie_id
		| _ -> (- 1)
	and d pfn key value = prerr_string key; prerr_string ": "; pfn value; prerr_string "; " in
	let di k v = d (prerr_int) k v
	and ds k v = d (prerr_string) k v
	and db k v = d (prerr_string) k (if v then "true" else "false") in
	function
	| State(s) ->
			di "state" s.st_id; ds "name" s.st_name;
			di "\n  parent" s.st_parent; db "frozen" s.st_frozen;
			( match s.st_refine with
				| AndRefine _ -> ds "refine" "as AND"
				| Graphic p -> di "refined w.r.t. parent" p
				| Textual None -> ds "refined" "as TEXT"
				| Textual Some p -> di "refined textual w.r.t. parent" p
			);
			ds "\n  actions" s.st_actions;
			prerr_newline()
	| Init(i) ->
			di "init" i.ie_id; di "parent" i.ie_parent;
			prerr_newline()
	| Exit(i) ->
			di "exit" i.ie_id; di "parent" i.ie_parent;
			prerr_newline()
	| Cond(c) ->
			di "cond" c.co_id; di "parent" c.co_parent;
			ds "\n  if" c.co_txt;
			prerr_newline()
	| Trans(t) ->
			di "trans" t.tr_id;
			di "from" (trans t.tr_from); di "to" (trans t.tr_to);
			di "\n  parent" t.tr_parent;
			ds "\n  label" t.tr_txt;
			prerr_newline()
