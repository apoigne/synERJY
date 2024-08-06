open Ly
open Util
open P
open Util_print
open Ast

(* ==========================================================================
GENERATE CTL - FORMULAS in the concrete syntax of VIS
========================================================================== *)

(* ==========================================================================
a formula is unparsed into a file. This file is used by a model - checker
to prove assertions about the synERJY - classes w.r.t. their reactive behavior
========================================================================== *)
type logic_t = CTL | LTL | RTCTL | PSL | PTL | ACTL

(* ==========================================================================
GENERATE CTL - FORMULAS in the concrete syntax of SMV
========================================================================== *)
let mk_ctl lbl sigs ro ctl =
	let rec bs_ctl i =
		function
		| CtlAx(c) -> pnT i; ps "AX "; bs_ctl (i + 1) c;
		| CtlAf(c) -> pnT i; ps "AF "; bs_ctl (i + 1) c;
		| CtlAg(c) -> pnT i; ps "AG "; bs_ctl (i + 1) c;
		| CtlEx(c) -> pnT i; ps "EX "; bs_ctl (i + 1) c;
		| CtlEf(c) -> pnT i; ps "EF "; bs_ctl (i + 1) c;
		| CtlEg(c) -> pnT i; ps "EG "; bs_ctl (i + 1) c;
		| CtlAu(c1, c2) -> pnT i; ps "A["; bs_ctl (i + 1) c1; ps " U "; bs_ctl (i + 1) c2; ps "]"
		| CtlEu(c1, c2) -> pnT i; ps "E["; bs_ctl (i + 1) c1; ps " U "; bs_ctl (i + 1) c2; ps "]"
		| CtlNot(c) -> ps "!( "; bs_ctl (i + 1) c; ps ")"
		| CtlAnd(c1, c2) -> ps "("; bs_ctl (i + 1) c1; ps "&"; bs_ctl (i + 1) c2; ps ")"
		| CtlOr(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " | "; bs_ctl (i + 1) c2; ps " )"
		| CtlXor(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " xor "; bs_ctl (i + 1) c2; ps " )"
		| CtlImplies(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " -> "; bs_ctl (i + 1) c2; ps " )"
		| CtlEquiv(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " <-> "; bs_ctl (i + 1) c2; ps " )"
		| CtlSig(i) ->
				if List.mem (mf2str i) sigs then (
					ps (ro_mf2str ro i)
				) else (
					Err.msg
						(Err.CTL ("Identifier '"^mf2str i^"' is \
								not declared as signal or label. \
								Hence it cannot be used in \
								CTL specifications.",
								type2id ro.trs_type, lbl))
				)
		| CtlSigVal(i) ->
				if List.mem (mf2str i^"__v") sigs then (
					ps (ro_mf2str ro i); ps "__v"
				) else (
					Err.msg
						(Err.CTL ("Identifier '$"^mf2str i^"' is \
								not declared as valued signal \
								of type 'bool'. Hence it is \
								not allowed in CTL specifications.",
								type2id ro.trs_type, lbl))
				)
		| CtlConst(c) when c = v_true ->
				ps "1"
		| CtlConst(c) when c = v_false ->
				ps "0"
		| CtlConst(c) ->
				Err.intern "mk_ctl"
	in
	bs_ctl 3 ctl

let mk_ltl_smv lbl sigs ro ctl =
	let rec bs_ctl i =
		function
		| LtlX(c) -> pnT i; ps "X"; bs_ctl (i + 1) c
		| LtlG(c) -> pnT i; ps "G"; bs_ctl (i + 1) c
		| LtlF(c) -> pnT i; ps "F"; bs_ctl (i + 1) c
		| LtlU(c1, c2) -> pnT i; bs_ctl (i + 1) c1; ps " U "; bs_ctl (i + 1) c2
		| LtlV(c1, c2) -> pnT i; bs_ctl (i + 1) c1; ps " V "; bs_ctl (i + 1) c2
		| LtlY(c) -> pnT i; ps "Y"; bs_ctl (i + 1) c
		| LtlZ(c) -> pnT i; ps "Z"; bs_ctl (i + 1) c
		| LtlH(c) -> pnT i; ps "H"; bs_ctl (i + 1) c
		| LtlO(c) -> pnT i; ps "O"; bs_ctl (i + 1) c
		| LtlS(c1, c2) -> pnT i; bs_ctl (i + 1) c1; ps " S "; bs_ctl (i + 1) c2
		| LtlT(c1, c2) -> pnT i; bs_ctl (i + 1) c1; ps " T "; bs_ctl (i + 1) c2
		
		| LtlNot(c) -> pnT i; ps "!( "; bs_ctl (i + 1) c; pnT i; ps ")"
		| LtlAnd(c1, c2) -> ps "("; bs_ctl (i + 1) c1; ps "*"; bs_ctl (i + 1) c2; ps ")"
		| LtlOr(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " | "; bs_ctl (i + 1) c2; ps " )"
		| LtlXor(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " xor "; bs_ctl (i + 1) c2; ps " )"
		| LtlImplies(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " -> "; bs_ctl (i + 1) c2; ps " )"
		| LtlEquiv(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " <-> "; bs_ctl (i + 1) c2; ps " )"
		| LtlSig(i) ->
				if List.mem (mf2str i) sigs then (
					ps (ro_mf2str ro i)
				) else (
					Err.msg
						(Err.LTL ("Identifier '"^mf2str i^"' is \
								not declared as signal or label. \
								Hence it cannot be used in \
								LTL specifications.",
								type2id ro.trs_type, lbl))
				)
		| LtlSigVal(i) ->
				if List.mem (mf2str i^"__v") sigs then (
					ps (ro_mf2str ro i); ps "__v"
				) else (
					Err.msg
						(Err.LTL ("Identifier '$"^mf2str i^"' is \
								not declared as valued signal \
								of type 'bool'. Hence it is \
								not allowed in LTL specifications.",
								type2id ro.trs_type, lbl))
				)
		| LtlConst(c) ->
				Err.intern "mk_ltl"
	in
	bs_ctl 2 ctl

let mk_ptl_smv lbl sigs ro ctl =
	let rec bs_ctl i = function
			PtlPrevious(c) -> pnT i; ps "AX ("; bs_ctl (i + 1) c; pnT i; ps ")"
		| PtlSince(c1, c2) -> pnT i; ps "AF ("; bs_ctl (i + 1) c1; pnT i; ps ")"
		| PtlOnce(c) -> pnT i; ps "AG ("; bs_ctl (i + 1) c; pnT i; ps ")"
		| PtlHasBeen(c) -> pnT i; ps "EX ("; bs_ctl (i + 1) c; pnT i; ps ")"
		| PtlNot(c) -> pnT i; ps "!( "; bs_ctl (i + 1) c; pnT i; ps ")"
		| PtlAnd(c1, c2) -> ps "("; bs_ctl (i + 1) c1; ps "*"; bs_ctl (i + 1) c2; ps ")"
		| PtlOr(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " + "; bs_ctl (i + 1) c2; ps " )"
		| PtlImplies(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " -> "; bs_ctl (i + 1) c2; ps " )"
		| PtlEquiv(c1, c2) -> ps "( "; bs_ctl (i + 1) c1; ps " <-> "; bs_ctl (i + 1) c2; ps " )"
		| PtlSig(i) -> ps "("; psmf i; ps "=1)"
		| PtlConst(c) -> Err.intern "mk_ptl"
	in
	bs_ctl 2 ctl

let make_smv_spec sigs (sp, ro) =
	match sp with
	| Ctl(lbl, ctl, _) -> ps "\nSPEC "; mk_ctl lbl sigs ro ctl
	| Ptl(lbl, ptl) -> ps "\nSPEC ptl "; mk_ptl_smv lbl sigs ro ptl
	| Ltl(lbl, ltl) -> ps "\nSPEC ltl "; mk_ltl_smv lbl sigs ro ltl
	| ValConstraint _ -> ()
	| ACtl _ -> Err.intern "make_smv_spec"

(* ==========================================================================
ACTL - Generation of models

========================================================================== *)
let sgn2__str s = "__"^Sc.sgn2str s

module ACTL =

struct
	
	type actl_t =
		{ omga : string;
			vars : string list;
			fair : string list;
			defs : (string * string) list;
			ins : (string * string) list;
			outs : (string * string) list;
			dout : (string * string) list;
			mems : (string * string * string) list;
		}
	
	let rec gen_actl_cond lbl ins ro =
		function
		| CtlNot(c) -> "!"^(gen_actl_cond lbl ins ro c)
		| CtlAnd(c1, c2) -> "("^gen_actl_cond lbl ins ro c1^" & "^gen_actl_cond lbl ins ro c2^")";
		| CtlOr(c1, c2) -> "("^gen_actl_cond lbl ins ro c1^" | "^gen_actl_cond lbl ins ro c2^")";
		| CtlImplies(c1, c2) -> gen_actl_cond lbl ins ro (CtlOr(CtlNot c1, c2))
		| CtlSig(i) ->
				let sn = ro_mf2str ro i in
				( try let (_, sn) = List.find (fun (_, sn') -> sn = sn') ins in
					sn;
				with Not_found ->
						Err.msg (Err.ACTL ("Identifier '"^mf2str i^"' is \
									not declared as a signal or a label \
									that is used as a virtual signal. \
									Hence it cannot be used in \
									the precondition of an assumption.",
									type2id ro.trs_type, lbl))
				)
		| CtlSigVal(i) ->
				let sv = ro_mf2str ro i^"__v" in
				( try let (_, sv) = List.find (fun (_, sv') -> sv = sv') ins in
					sv;
				with Not_found ->
						Err.msg (Err.ACTL ("Identifier '$"^mf2str i^"' is \
									not declared as a valued signal \
									of type 'bool'. Hence it \
									cannot be used in the precondition of \
									an assumption.",
									type2id ro.trs_type, lbl))
				)
		| _ ->
				Err.msg (Err.ACTL ("In a condition of an assumption only Boolean \
							operators are allowed.",
							type2id ro.trs_type, lbl))
	
	let gen_actl_model lbl ins dins outs douts ro ass cond =
		let rec actl_model e cond =
			match e with
			| CtlAx(c) ->
					let r = sgn2__str (Sc.next_reg()) in
					let m = actl_model c r in
					{ omga = m.omga;
						vars = r:: m.vars;
						fair = m.fair;
						defs = m.defs;
						ins = m.ins;
						outs = m.outs;
						dout = m.dout;
						mems = (r, cond,"0"):: m.mems;
					}
			| CtlAf(c) ->
					let x = sgn2__str (Sc.next_inp())
					and r = sgn2__str (Sc.next_reg()) in
					let m = actl_model c ("("^r^" & "^x^")") in
					{ omga = m.omga;
						vars = x:: r:: m.vars;
						fair = x:: m.fair;
						defs = m.defs;
						ins = m.ins;
						outs = m.outs;
						dout = m.dout;
						mems = (r, cond,"("^r^" & !"^x^")"):: m.mems;
					}
			| CtlAg(c) ->
					let r = sgn2__str (Sc.next_reg()) in
					let m = actl_model c r in
					{ omga = m.omga;
						vars = r:: m.vars;
						fair = m.fair;
						defs = m.defs;
						ins = m.ins;
						outs = m.outs;
						dout = m.dout;
						mems = (r, cond, r):: m.mems;
					}
			| CtlAu(c1, c2) ->
					let x = sgn2__str (Sc.next_inp())
					and r = sgn2__str (Sc.next_reg()) in
					let m1 = actl_model c1 ("(!"^x^" & "^r^")")
					and m2 = actl_model c2 ("("^x^" & "^r^")") in
					{ omga = m1.omga;
						vars = r:: x:: m1.vars@m1.vars;
						fair = x:: m1.fair@m2.fair;
						defs = m1.defs@m2.defs;
						ins = m1.ins@m2.ins;
						outs = m1.outs@m2.outs;
						dout = m1.dout@m2.dout;
						mems = (r, cond,"("^x^" & "^r^")"):: m1.mems@m2.mems;
					}
			| CtlNot(c) ->
					let m = actl_model c ("(!"^cond) in
					{ omga = "(!"^m.omga;
						vars = m.vars;
						fair = m.fair;
						defs = m.defs;
						ins = m.ins;
						outs = m.outs;
						dout = m.dout;
						mems = m.mems;
					}
			| CtlAnd(c1, c2) ->
					let m1 = actl_model c1 cond
					and m2 = actl_model c2 cond in
					{ omga = "("^m1.omga^" & "^m2.omga^")";
						vars = m1.vars@m1.vars;
						fair = m1.fair@m2.fair;
						defs = m1.defs@m2.defs;
						ins = m1.ins@m2.ins;
						outs = m1.outs@m2.outs;
						dout = m1.dout@m2.dout;
						mems = m1.mems@m2.mems;
					}
			| CtlOr(c1, c2) ->
					let x1 = sgn2__str (Sc.next_inp())
					and x2 = sgn2__str (Sc.next_inp())
					and g = sgn2__str (Sc.next_gamma()) in
					let m1 = actl_model c1 ("("^g^" & !"^x2^")")
					and m2 = actl_model c2 ("("^g^" & !"^x1^")") in
					{ omga = "("^m1.omga^" | "^m2.omga^")";
						vars = x1:: x2:: m1.vars@m1.vars;
						fair = m1.fair@m2.fair;
						defs = (g, cond):: m1.defs@m2.defs;
						ins = m1.ins@m2.ins;
						outs = m1.outs@m2.outs;
						dout = m1.dout@m2.dout;
						mems = m1.mems@m2.mems;
					}
			| CtlImplies(c1, c2) ->
					actl_model(CtlOr(CtlNot c1, c2)) cond
			| CtlSig(i) ->
					let sn = ro_mf2str ro i in
					( try let (_, sn) = List.find (fun (_, sn') -> sn = sn') outs in
						{ omga = sgn2__str (Sc.next_inp());
							vars = [];
							fair = [];
							defs = [];
							ins = [];
							outs = [sn, cond];
							dout = [];
							mems = [];
						}
					with Not_found ->
							try let (_, sn) = List.find (fun (_, sn') -> sn = sn') douts in
								let x = sgn2__str (Sc.next_inp()) in
								{ omga = x;
									vars = [x];
									fair = [];
									defs = [];
									ins = [];
									outs = [];
									dout = [sn,"("^cond^" | "^x^")"];
									mems = [];
								}
							with Not_found ->
									Err.msg (Err.ACTL ("Identifier '"^mf2str i^"' is \
												not declared as a sensor or a \
												label that is used as virtual sensor. \
												Hence it cannot be used in an \
												assumption.",
												type2id ro.trs_type, lbl))
					)
			| CtlSigVal(i) ->
					let sv = ro_mf2str ro i^"__v" in
					( try let (_, sn) = List.find (fun (_, sv') -> sv = sv') outs in
						{ omga = sgn2__str (Sc.next_inp());
							vars = [];
							fair = [];
							defs = [];
							ins = [];
							outs = [sn, cond];
							dout = [];
							mems = [];
						}
					with Not_found ->
							Err.msg (Err.ACTL ("Identifier '$"^mf2str i^"' is \
										not declared as a valued sensor \
										of type 'bool'. Hence it \
										cannot be used in an assumption.",
										type2id ro.trs_type, lbl))
					)
			| _ -> Err.intern "actl_model"
		in
		actl_model ass (gen_actl_cond lbl (ins@dins) ro cond)
	
	let rec gen_model ins dins outs douts spec =
		match spec with
		| ACtl (lbl, ass, cond), ro ->
				gen_actl_model lbl ins dins outs douts ro ass cond
		| _ ->
				Err.intern "ACTL.gen_model"
	and gen_models ins dins outs douts spec =
		match spec with
		| [] ->
				{ omga = "0";
					vars = [];
					fair = [];
					defs = [];
					ins = [];
					outs = [];
					dout = [];
					mems = [];
				}
		| spec:: specl ->
				let m1 = gen_model ins dins outs douts spec
				and m2 = gen_models ins dins outs douts specl in
				{ omga = "0";
					vars = m1.vars@m2.vars;
					fair = m1.fair@m2.fair;
					defs = m1.defs@m2.defs;
					ins = m1.ins@m2.ins;
					outs = m1.outs@m2.outs;
					dout = m1.dout@m2.dout;
					mems = m1.mems@m2.mems;
				}
	
end

let gen_model ~insig ~indatasig ~outsig ~outdatasig ~spec =
	let m = ACTL.gen_models insig indatasig outsig outdatasig spec in
	m.ACTL.vars, m.ACTL.fair,
	m.ACTL.defs, m.ACTL.outs, m.ACTL.dout, m.ACTL.ins, m.ACTL.mems
