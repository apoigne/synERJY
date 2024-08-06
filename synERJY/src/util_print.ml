open P
open Ast
open Util

(* --------------------------------------------------------------------------
utilities for printing - produces
- either a string
- or calls user - supplied print functions
-------------------------------------------------------------------------- *)
let rec lst_sep2str fct sep =
  function
  | [] -> ""
  | [h] -> (fct h)
  | h:: t -> (fct h)^sep^(lst_sep2str fct sep t)

let rec lst_sep2str_rev fct sep =
  function
  | [] -> ""
  | [h] -> (fct h)
  | h:: t -> (lst_sep2str fct sep t)^sep^(fct h)

let rec lst_sepf2ps fct sep =
  function
  | [] -> ()
  | [h] -> (fct h)
  | h:: t -> (fct h); sep (); lst_sepf2ps fct sep t

let lst_sep2BstrB lft rgt fct sep l =
  if l = []
  then ""
  else lft^(lst_sep2str fct sep l)^rgt

(* --------------------------------------------------------------------------
utilities for printing - calls !print_fct to print to stdout
-------------------------------------------------------------------------- *)
let lst_sep2ps fct sep =
  lst_sepf2ps fct (fun () -> ps sep)

let lst_sep2BpsB lft rgt fct sep l =
  if l = []
  then (lst_sep2ps fct sep l)
  else ( ps lft; lst_sep2ps fct sep l; ps rgt )

(* -------------------------------------------------------------------------
print a nice comment line: /* ---------- < text > ------ */
ending at col 79: 79 - 3 - 3 - 10 - 1 - strlen - 1
------------------------------------------------------------------------- *)
let print_detailed_comment b s e =
  let l = String.length s in
  let r = 61 - l in
  if r < 0
  then (
    ps b; ps s; ps e
  ) else (
    ps b; ps "---------- "; ps s; ps " ";
    for i = 1 to r do ps "-" done; ps e
  )

let print_comment s =
  print_detailed_comment "\n/* " s " */\n"

(* ========================================================================= *)
let call2str =
  function
  | Call(n, None), _ -> Ly.mf2str n
  | _ -> Err.intern "call2str"

let callthis2str =
  function
  | Call(n, None) -> Ly.mf2str n
  | This -> "this"
  | _ -> Err.intern "calll2str"

let rec calll2str sep cl = lst_sep2str call2str sep cl

let obj2str ro = calll2str "." ro.trs_name
let sig2str xs =
  match xs.trs_sname with
  | (Call(_, None), _):: cl -> calll2str "." cl
  | _ -> Err.intern "sig2dotstr"

let ro_sgn2str ro sn =
  let sn = Sc.sgn2str sn in
  let on =
    try calll2str "_" (List.tl ro.trs_name)
    with _ -> assert false in
  if on = "" then "__"^sn else "__"^on^"__"^sn

let ro_lbl2str ro lbl =
  let sn = Ly.lbl2str lbl in
  let on =
    try calll2str "_" (List.tl ro.trs_name)
    with _ -> assert false in
  if on = "" then sn else on^"__"^sn

let ro_mf2str ro mf =
  let mf = Ly.mf2str mf in
  let on =
    try calll2str "_" (List.tl ro.trs_name)
    with _ -> assert false in
  if on = "" then mf else on^"__"^mf

(* -------------------------------------------------------------------------
print status of instances
------------------------------------------------------------------------- *)
let print_status ps ast =
  ps "\n";
  ps (Ly.c2str ast.classid);
  ps ":";
  ( match ast.status with
    | NotTouched -> ps "NotTouched"
    | Singleton (cid, fm) -> ps "Singleton"; ps (Ly.c2str cid);
        ps "_"; ps (Ly.mf2str fm);
    | Multiple -> ps "Multiple"
  )

(* -------------------------------------------------------------------------
print the tree of reactive objects
------------------------------------------------------------------------- *)
let rec print_ro_tree ro =
  ps (calll2str "." ro.trs_name); ps " : "; psc (type2id ro.trs_type);
  pn(); List.iter print_ro_sig ro.trs_sig

and print_ro_sig rosig =
  ps (Err.type2str rosig.trs_styp);
  ps (calll2str "." rosig.trs_sname);
  ( match rosig.trs_principal with
    | Principal p -> ps " : connected to "; ps (calll2str "." p.trs_sname)
    | _ -> ps " : principal signal"
  );
  ps "\n"

(* --------------------------------------------------------------------------
meaning of variables:
- p ... precedence
- r ... representation
- a ... already assembled

op_table encoding:
(internalName, externalRep, precedence, associativity)
associativity = 0 not associative or unary operator
= precedence right associative
< precedence left associative
-------------------------------------------------------------------------- *)
exception OpNotFound
type top = { opi : tmfid; opr : string; opp : int; opa : int }

let op_table = [
  (* {opi=id_op_assign,"=",3,3) ; not needed, but used in this way *)
  { opi = id_op_fby; opr ="->"; opp = 6; opa = 5 };
  { opi = id_op_downspl; opr ="at"; opp = 9; opa = 8 };
  { opi = id_op_bit_or; opr ="|"; opp = 12; opa = 12 };
  { opi = id_op_log_or; opr ="||"; opp = 12; opa = 12 };
  { opi = id_op_xor; opr ="^"; opp = 12; opa = 12 };
  { opi = id_op_bit_and; opr ="&"; opp = 15; opa = 15 };
  { opi = id_op_log_and; opr ="&&"; opp = 15; opa = 15 };
  { opi = id_op_equal; opr ="=="; opp = 21; opa = 21 };
  { opi = id_op_not_equal; opr ="!="; opp = 21; opa = 21 };
  { opi = id_op_lt; opr ="<"; opp = 24; opa = 23 };
  { opi = id_op_gt; opr =">"; opp = 24; opa = 23 };
  { opi = id_op_le; opr ="<="; opp = 24; opa = 23 };
  { opi = id_op_ge; opr =">="; opp = 24; opa = 23 };
  { opi = id_op_add; opr ="+"; opp = 27; opa = 27 };
  { opi = id_op_sub; opr ="-"; opp = 27; opa = 27 };
  { opi = id_op_mult; opr ="*"; opp = 30; opa = 30 };
  { opi = id_op_pointmult; opr =".*"; opp = 30; opa = 30 };
  { opi = id_op_div; opr ="/"; opp = 30; opa = 30 };
  { opi = id_op_not; opr ="!"; opp = 33; opa = 0 };
  { opi = id_op_upspl; opr ="current "; opp = 39; opa = 0 };
  { opi = id_op_pre; opr ="pre "; opp = 39; opa = 0 };
  { opi = id_op_minus; opr ="-"; opp = 42; opa = 0 };
  { opi = id_op_plus; opr ="+"; opp = 42; opa = 0 };
  ]

let find_op op =
  let rec bs_find =
    function
    | [] -> raise OpNotFound
    | o:: ot -> if o.opi = op
        then o
        else bs_find ot
  in
  bs_find op_table

type expr_data = { p: int; r: string }

(* --------------------------------------------------------------------------
conversion of reactive signals and expressions to strings
-------------------------------------------------------------------------- *)
let rec rctexp2str =
  function
  | S x -> Sc.sgn2str x
  | CC x -> "CC("^(Sc.sgn2str x)^")"
  | TT -> "true"
  | FF -> "false"
  | NNot x -> "!("^(rctexp2str x)^")"
  | AAnd el -> "("^(lst_sep2str rctexp2str " && " el)^")"
  | OOr el -> "("^(lst_sep2str rctexp2str " || " el)^")"

(* --------------------------------------------------------------------------
utilities for printing expressions:
paren a p parentheses around expr ''a'' if priority ''p'' requires
asgop o representation for assign operator ''o''
-------------------------------------------------------------------------- *)
let paren a p =
  if a.p < p
  then "("^a.r^")"
  else a.r

let asgop o =
  if o = id_op_assign
  then (Ly.mf2str id_op_assign)
  else (Ly.mf2str o)^(Ly.mf2str id_op_assign)

(* --------------------------------------------------------------------------
print expressions
-------------------------------------------------------------------------- *)
let rec bs_call a e =
  
  match e.expr with
  | Dot (e1, e2) -> bs_call (bs_call a e1) e2
  | Call(n, None) ->
      { p = 99;
        r = (paren a 99)^(if a.r = "" then "" else ".")^(Ly.mf2str n)
      }
  | Call(n, Some al) ->
      ( try
          let n = find_op n in
          if n.opa = 0  (* unary operator? *)
          then if al = []
            then { p = n.opp; r = n.opr^(paren a n.opp) }
            else Err.intern "bs_call"
          else let e = bs_call { p = 99; r ="" } (List.hd al)
            and p = n.opp in
            { p = p; r = (paren a p)^n.opr^(paren e p) }
      with
      | Failure _ ->  (* List.hd failed, why? *)
          Err.intern "bs_call"
      | OpNotFound -> (* no operator, ordinary call ... *)
          { p = 99;
            r = (paren a 99)^
              (if a.r = "" then "" else ".")^(Ly.mf2str n)^
              (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      )
  | ClassCall(t, n, None) ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r = (Ly.c2str t)^"."^(Ly.mf2str n) }
      else (Err.intern "bs_call")
  | ClassCall(t, n, Some al) ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r = (Ly.c2str t)^"."^(Ly.mf2str n)^
            (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      else (Err.intern "bs_call")
  | New(t, al) ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r ="New "^(Err.type2str t)^
            (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      else (Err.intern "bs_call")
  | This ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="this" }
      else (Err.intern "bs_call")
  | Cast(t, e) ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="("^(Err.type2str t)^") "^(expr2str e) }
      else (Err.intern "bs_call")
  | ThisConstr al ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r ="this"^
            (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      else (Err.intern "bs_call")
  | NullObj ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="null" }
      else (Err.intern "bs_call")
  | Super(n, al) ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r ="super "^(Ly.mf2str n)^
            (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      else (Err.intern "bs_call")
  | SuperConstr al ->
      if a = { p = 99; r ="" }
      then { p = 99;
          r ="super"^
            (lst_sep2BstrB "(" ")" (expr2str) "," al) }
      else (Err.intern "bs_call")
  | IncrDecr(f, op) ->
      if a = { p = 99; r ="" }
      then let f = Ly.mf2str f in
        { p = 99;
          r = if op = id_op_prefix_incr then
              "++"^f
            else if op = id_op_prefix_decr then
              "--"^f
            else if op = id_op_postfix_incr then
              f^"++"
            else if op = id_op_postfix_decr then
              f^"--"
            else Err.intern "bs_call"
        }
      else (Err.intern "bs_call")
  
  | Assign(f, NoArr, k, e) ->
      let e = bs_call { p = 99; r ="" } e in
      { p = 3; r = (Ly.mf2str f)^(Ly.mf2str k)^e.r }
  | Assign(f, ADim1 i, k, e) ->
      let e = bs_call { p = 99; r ="" } e
      and i = bs_call { p = 99; r ="" } i
      and f = Ly.mf2str f in
      { p = 3; r = f^"["^i.r^"] "^f^e.r }
  | Assign(f, ADim2(x, y), k, e) ->
      let e = bs_call { p = 99; r ="" } e
      and x = bs_call { p = 99; r ="" } x
      and y = bs_call { p = 99; r ="" } y
      and f = Ly.mf2str f in
      { p = 3; r = f^"["^x.r^","^y.r^"] "^f^e.r }
  | Slice r ->
      let e1 = bs_call { p = 99; r ="" } r.lower
      and e2 = bs_call { p = 99; r ="" } r.upper in
      if a = { p = 99; r ="" }
      then { p = 99; r = e1.r^".."^e2.r }
      else (Err.intern "bs_call")
  | DotDot(i, _, o) ->
      let i = (Sc.sgn2str) i and o = i2s o in
      if a = { p = 99; r ="" }
      then { p = 99; r = i^".."^"[>"^o^"]" }
      else (Err.intern "bs_call")
  | Present(i) ->
      let i = (Sc.sgn2str) i in
      if a = { p = 99; r ="" }
      then { p = 99; r ="?"^i }
      else (Err.intern "bs_call")
  | Timestamp(i) ->
      let i = (Sc.sgn2str) i in
      if a = { p = 99; r ="" }
      then { p = 99; r ="@"^i }
      else (Err.intern "bs_call")
  | SigVal(i, xs, o) ->
      let i = (Sc.sgn2str) i and o = i2s o in
      let xs = (Sc.sgn2valstr) (sig2sgn xs) in
      let i = i^"(="^xs^")"^"[>"^o^"]" in
      if a = { p = 99; r ="" }
      then { p = 99; r = i }
      else (Err.intern "bs_call")
  | Value(i) ->
      let i = (Sc.sgn2str) i in
      if a = { p = 99; r ="" }
      then { p = 99; r = i }
      else (Err.intern "bs_call")
  | Var(i) ->
      let i = "Var("^i2s i^")" in
      if a = { p = 99; r ="" }
      then { p = 99; r = i }
      else (Err.intern "bs_call")
  | Offset(_, _) ->
      let i = "Offset" in
      if a = { p = 99; r ="" }
      then { p = 99; r = i }
      else (Err.intern "bs_call")
  | DeltaT ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="dt" }
      else (Err.intern "bs_call")
  | Instant ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="instant()" }
      else (Err.intern "bs_call")
  | If(_) ->
      { p = 99;
        r = (if a.p < 99 then "("^a.r^")" else a.r)^
          (if a.r = "" then "" else ".")^"(if .. end)" }
  | Literal(_, s, _) ->
      if a = { p = 99; r ="" }
      then { p = 99; r = s }
      else Err.intern "bs_call"
  | ArrayLiteral (Dim1 l) ->
      if a = { p = 99; r ="" }
      then { p = 99; r = arr2str l.av1 }
      else Err.intern "bs_call"
  | ArrayLiteral (Dim2 l) ->
      if a = { p = 99; r ="" }
      then { p = 99; r ="{"^(lst_sep2str arr2str "," l.av2)^"}" }
      else Err.intern "bs_call"

and arr2str el =
  "{"^(lst_sep2str (expr2str ) "," el)^"}"

and expr2str (e : texpr) =
  let ed = bs_call { p = 99; r ="" } e in
  ed.r

(* =========================================================================
print routines for "reactive entities"
========================================================================= *)
let print_sgn s = ps (Sc.sgn2str s)
let print_sgn_l sl = List.iter ( fun x -> pn(); ps (Sc.sgn2str x)) sl

let print_expr e = ps (expr2str e)
let print_rctexp e = ps (rctexp2str e)

let print_sat s =
  if (String.length s) > 50
  then ( ps s; ps " @ " )
  else ps (Printf.sprintf "%-50s @ " s)

let rec print_rctcall n cll =
  let prl ro l =
    if Ly.is_int_lbl (list_last l) then (
    (* don't print internal labels [[to reduce debug output]] *)
    ) else (
      ps (calll2str "." ro.trs_name); ps ":";
      lst_sep2ps (fun l -> ps (Ly.lbl2str l)) ":" l;
      ps ": "
    )
  and prs s = print_sgn s
  and pre e = print_expr e
  and prb cc =
    List.iter
      ( fun (e, a) ->
            pnT (n * 5); ps "if "; print_rctexp e; ps " {";
            print_rctcall (n + 1) a;
            pnT (n * 5); ps "}"
      ) cc
  and prve = function
    | Rct e -> ps "Rct: "; print_rctexp e
    | Val e -> ps "Val: "; print_expr e
  in
  match cll with
  | ActStmt(l, e, ro) -> pnT (5 * n); prl ro l; pre e
  | ActProd(l, s, e1, e2, cc, ro) ->
      prb cc;
      pnT (5 * n); prl ro l; prs s; ps " = ";
      pre e1; ps" * "; pre e2
  | ActInit(l, s, _, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " init"
  | ActSigInit(l, s, xs, ro) ->
      pnT (5 * n); prl ro l; prs s; ps "((";
      prs (- (sig2sgn xs)); ps ")) init"
  (* sgn2val !!!!!!!! *)
  | ActEmit(l, f, s, il, e, cc, _, ro) ->
      prb cc;
      ( match e with
        | Val _ -> pnT (5 * n)
        | Rct _ -> pnT(5)
      );
      prl ro l; prs s;
      ( match il with
        | [] -> ()
        | [i] -> ps"["; pre i; ps"]"
        | [i1; i2] -> ps"["; pre i1; ps","; pre i2; ps "]"
        | _ -> Err.intern "print_rctcall:ind"
      );
      if f then ps " := " else ps " <= "; prve e
  | ActBlck(cc) ->
      prb cc
  | ActSigPres(l, s, _, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " ?"
  | ActSigVal(l, s, _, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " $"
  | ActSigIn(l, xs, ro) ->
      pnT (5 * n); prl ro l; prs (sig2sgn xs); ps " in"
  | ActSigOut(l, xs, ro) ->
      pnT (5 * n); prl ro l; prs (sig2sgn xs); ps " out"
  | ActAssgn(l, s, e, cc, ro) ->
      prb cc; pnT (5 * n); prl ro l; prs s; ps " = "; pre e
  | ActAssgnInBlck(l, s, e, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " = "; pre e
  | ActBool(l, s, e, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " <- "; pre e
  | ActTimeInit(l, s, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " = 0";
  | ActTimeAdd(l, s, ro) ->
      pnT (5 * n); prl ro l; prs s; ps " = ";
      prs s; ps "+deltat"
  | ActTimeCond(l, b, s, e, ro) ->
      pnT (5 * n); prl ro l; prs b; ps " <- @";
      prs s; ps " <= "; pre e
  | ActPrm(l, s, e, x, ro) ->
      pnT (5 * n); prl ro l; prs s; ps "(";
      ps (Ly.mf2str x.p_name); ps ") = "; pre e
  | ActDbg(l, ro) ->
      pnT (5 * n); prl ro l; ps ":(";
      List.iter
        (fun x -> pi (Ly.mf2int(Ly.lbl2mf x))) l
      ; ps")"

let print_rctdef knd x =
  if x.sc_knd = knd
  then (
    pnT 2; ps (Printf.sprintf "%-6s <- " (Sc.sgn2str x.sc_sgn));
    print_sat (rctexp2str x.sc_def); ps":"; pi x.sc_occ;
    match x.sc_act with
    | None -> ()
    | Some a -> print_rctcall 1 a
  )

let print_sig_valtyp vt =
  if vt = Null
  then ( ps " " )
  else ( ps "<"; ps (Err.type2str vt); ps "> " )

let print_dim x =
  match x with
  | DimLen n, o -> ps " ("; pi n; ps ","; pi o; ps ")"
  | DimVar id, o -> ps " (var,"; pi o; ps ")"
  | _ -> ps " ref"

let print_sig xs =
  let ff = (sig2frq xs).ff in
  pnT 2; ps (Err.type2str xs.trs_styp);
  if ff <> 1 (* tick *) then ( ps "{"; print_sgn ff; ps "}"); ps " ";
  print_sgn (sig2sgn xs); ps " : "; ps (calll2str "." (sig2sname xs));
  ps "   buffer:"; List.iter print_dim (sig2buf xs)

let print_param = function
  | SigPrm xs -> print_sig xs
  | SigFld _ -> Err.intern "print_param: SigFld"
  | VarLcl _ -> Err.intern "print_param: VarLcl"
  | SigPrc _ -> Err.intern "print_param: SigPrc"
  | SigLcl _ -> Err.intern "print_param: SigLcl"
  | VarPrm { prm_ent ={ p_name = n; p_type = t }} ->
      pnT 2; ps (Err.type2str t); ps "   "; ps (Ly.mf2str n)

let print_lcl_dcl = function
  | LclFld (s,{ p_name = n; p_type = t }) ->
      pnT 2; ps (Err.type2str t); ps "   "; print_sgn s;
      ps "("; ps (Ly.mf2str n); ps ")"
  | IntSig (s, t, n) ->
      pnT 2; if n = 0 then (
        ps (Err.type2str t); ps "   "; print_sgn s
      ) else (
        ps (Err.type2str t); ps "["; pi n; ps "] "; print_sgn s
      )
  | LclSig xs -> print_sig xs

let print_rctcomp sc =
  pn ();
  pnT 1; ps "fields: ";
  List.iter print_sig (ro2sigs sc.sc_ro);
  pnT 1; ps "parameters: ";
  List.iter print_param sc.sc_fmls;
  pnT 1; ps "locals: ";
  List.iter print_lcl_dcl sc.sc_dcls;
  pn (); pnT 1; ps "equations:";
  pn (); List.iter (print_rctdef Eqn) sc.sc_defs;
  pn (); pnT 1; ps "memorisations:";
  pn (); List.iter (print_rctdef Mem) sc.sc_defs;
  pn (); pnT 1; ps "pres:";
  pn (); List.iter (print_rctdef Nxt) sc.sc_defs;
  ()

let print_rctappl sa =
  pn ();
  pnT 1; ps "application: ";
  pnT 1; ps "signals: ";
  List.iter print_sig sa.sa_sigs;
  pnT 1; ps "locals: ";
  List.iter print_lcl_dcl sa.sa_dcls;
  pn (); pnT 1; ps "equations:";
  pn (); List.iter (print_rctdef Eqn) sa.sa_eqns;
  pn (); pnT 1; ps "memorisations:";
  pn (); List.iter (print_rctdef Mem) sa.sa_mems;
  pn (); pnT 1; ps "pres:";
  pn (); List.iter (print_rctdef Nxt) sa.sa_mems;
  ()

(* ========================================================================= *)
(* let rec pp_sl i sl = List.iter (pp_s (i + 1)) sl

and pp_al al = lst_sep2BpsB "(" ")" pp_e "," al
and pp_n i s = pnT i; ps s
and pp_e e = ps "("; ps (expr2str e); ps ")"

and pp_tl i = function
| [Then(_, e,[])] -> pp_e e
| tl -> pp_te i tl

and pp_te i = function
| Then(_, e, sl):: t -> ps " "; pp_e e; ps " {"; pp_sl i sl;
if t = []
then ( pp_n i "}" )
else ( pp_n i "} else if"; pp_te i t )
| [Else(_, sl)] -> ps " (true) {"; pp_sl i sl; pp_n i "}"
| _ -> Err.intern "pp_te"

and pp_state i s =
let psl i s sl = pp_n i s; ps " {"; pp_sl i sl; pp_n i "}" in
pp_n i "state "; psmf s.sname;
let l = s.sdo in
if l = [] then () else ( psl i "do" l);
let l = s.sentry in
if l = [] then () else ( psl i "entry" l);
let l = s.sduring in
if l = [] then () else ( psl i "during" l);
let l = s.sexit in
if l = [] then () else ( psl i "exit" l);
pp_n i "when"; pp_tl i s.strans

and pp_s i s =
pnT i;
pp_S i s;
ps ";"

and pp_label lbl =
if lbl = Ly.nolbl
then ()
else ( ps ":"; ps (Ly.lbl2str lbl); ps ": " )

and pp_next n =
if n
then ps " next"
else ps " "

and pp_S i = function
(* special cases *)
| ExprStmt(lbl,{ expr = If(tl) }) -> pp_label lbl; ps "if"; pp_tl i tl

(* standard cases *)
| Activate(lbl, n, sl, e) -> pp_label lbl; ps "activate";
pp_next n; ps "{"; pp_sl i sl;
pp_n i "} when "; pp_e e
| AssertStmt(lbl, spl) -> pp_label lbl; ps "assert {...}"
| TextStM(lbl, a) -> pp_label lbl; ps "automaton {";
pp_state (i + 1) a.a_init;
List.iter (pp_state (i + 1)) a.a_states;
pp_n i "}"
| GraphicStM(lbl, g) -> pp_label lbl; ps "automaton \"";
ps g.gstm_file; ps "\""
| Await(lbl, n, tl) -> pp_label lbl; ps "await"; pp_next n; pp_tl i tl
| Break(lbl, None) -> pp_label lbl; ps "break"
| Break(lbl, Some n) -> pp_label lbl; ps "break "; psmf n
| Continue(lbl, None) -> pp_label lbl; ps "continue"
| Continue(lbl, Some n) -> pp_label lbl; ps "continue "; psmf n;
| DoStmt(lbl, e, sl) -> pp_label lbl; ps "do {"; pp_sl i sl;
ps "} when "; pp_e e
| Emit(lbl, fe, s, i, e) -> ( let p s
if fe then (

pp_label lbl; ps "emit"; psmf s; pp_al [e]
| FlowEqu(lbl, s, i, e) -> pp_label lbl; psmf s;
( match i with
| [] -> ()
| [e] -> ps "["; print_expr e; ps "]"
| [e1; e2] -> ps "["; print_expr e1; ps ",";
print_expr e2; ps "]"
| _ -> Err.intern "pp_S"
);
ps " := "; pp_al [e]
| ExprStmt(lbl, e) -> pp_label lbl; pp_e e
| ForStmt(lbl, f) -> pp_label lbl; ps "for (";
( match f.forinit with
| None -> ()
| Some e -> pp_e e;
); ps ";";
( match f.fortest with
| None -> ()
| Some e -> pp_e e;
); ps ";";
( match f.forupd with
| None -> ()
| Some e -> pp_e e;
);
ps ") {"; pp_sl i f.forstmtl
| Halt(lbl) -> pp_label lbl; ps "halt"
| LetStmt(lbl, l) -> pp_label lbl; ps "local ";
psmf l.letent.p_name; ps "; ..."
| Next(lbl) -> pp_label lbl; ps "next"
| NextState(lbl, n) -> pp_label lbl; ps "next state "; psmf n
| Nothing(lbl) -> pp_label lbl; ps "nothing"
| FlowContext(lbl, fc) -> pp_label lbl; ps "{|";
let pp_iter sl =
List.iter (fun s -> pp_n i "  "; pp_S i s) sl
in
( match fc.f_dcls, fc.f_equ with
| [],[] -> ()
| [], e:: equ -> pp_S i e;
pp_iter equ
| d:: dcls, equ -> pp_S i d;
pp_iter dcls;
pp_iter equ
);
pp_n i "|}"
| Par(lbl, sll) -> pp_label lbl; ps "[["; pp_sl i (List.hd sll);
List.iter (fun sl -> pp_n i "||"; pp_sl i sl)
(List.tl sll);
pp_n i "]]"
| Cancel(lbl, b, n, sl, tl) -> pp_label lbl; ps "cancel";
if b then ps " strongly" else (); pp_next n;
ps "{"; pp_sl i sl ; pp_n i "}"; pp_tl i tl
| RctLoop(lbl, sl) -> pp_label lbl; ps "loop {";
pp_sl i sl; pp_n i "}"
| Return(lbl, e) -> pp_label lbl; ps "return";
( match e with
| None -> ()
| Some e -> ps " "; pp_e e
)
| Schedule(lbl, e) -> pp_label lbl; ps "schedule "; pp_e e
| Sustain(lbl, sl) -> pp_label lbl; ps "sustain {";
pp_sl i sl; pp_n i "}"
| Switch(lbl, sl) -> pp_label lbl; ps "switch {...}"
| Throw(lbl, e) -> pp_label lbl; ps "throw new RuntimeException(";
( try
let cid = type2id se.conf_class in
let thw = eval_const_expr cid e in
pi (Int64.to_int thw)
with _ ->
ps "..."
); ps ")";
| While(lbl, e, sl) -> pp_label lbl; ps "while "; pp_e e; ps " {";
pp_sl i sl; pp_n i "}" *)
