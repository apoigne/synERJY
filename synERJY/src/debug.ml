open P
open Ly
open Ast
open Util_print
open Util
open Puctrl
open Gen_application
;;

(* =========================================================================
show_entry "class" "name" arity ... show a symtab entry
show_class "class" ... show all symtab entries
show_classtab () ... show statsu of all classes
========================================================================= *)
let signature2ps dcl =
  let rt =
    match dcl.signature.rt with
    | Null -> "void"
    | t -> Err.type2str t
  and pt =
    match dcl.signature.pt with
    | None -> ""
    | Some l -> " ("^(lst_sep2str Err.type2str ", " l)^")"
  in
  Printf.printf "%-20s " (rt^pt)

let dcl2ps arity dcll level =
  let dcl =
    match dcll with
    | dcl:: _ -> dcl
    | _ -> raise Not_found in
  let kd =
    match dcl.entry with
    | Field _ -> "field       "
    | Method m ->
        ( match m.method_kind with
          | BttmMethod -> "mthd (bttm) "
          | RctMethod _ -> "mthd (rct)  "
          | RctNode _ -> "mthd (node) "
          | DataMethod -> "mthd (data) "
          | Abstract -> "mthd (abst) "
          | Internal -> "mthd (int)  "
          | NativeMethod _ -> "mthd (natv) "
          | NativeIntr _ -> "mthd (intr) "
          | _ -> Err.intern "dcl2ps"
        )
    | Constructor _ ->
        if arity = (- 2)
        then "CONSTR LIST "
        else "constructor "
    | SigDecl _ -> "signal      "
    | _ -> raise Not_found
  in
  ps kd;
  signature2ps dcl;
  Printf.printf "%-12s [%s,1of%d]\n"
    (mf2str dcl.name) (c2str dcl.origin) (List.length dcll)

let show_entry c m a =
  let c = c2id c in
  let m = mf2id m in
  let ast = i_ast c in
  let dcll = Hashtbl.find ast.symtab (m, a) in
  dcl2ps a dcll 0

let show_class c =
  let c = c2id c in
  let ast = i_ast c in
  Hashtbl.iter (fun (k, a) dcll -> dcl2ps a dcll 0) ast.symtab

let show_classtab cid ast =
  if ast.class_sta.class_status = NoUpdate then (
  (* not too much info *)
  ) else (
    psc cid;
    ps "\n  ";
    ps ( match ast.class_sta.class_status with
        | Unknown -> "Unknown"
        | Parsed -> "Parsed"
        | InheritChecked -> "InheritChecked"
        | TypeChecked -> "TypeChecked"
        | Synchron -> "Synchron"
        | NoUpdate -> "NoUpdate"
      );
    ps "\n  reactive component: ";
    ps ( match ast.class_sta.class_behavior with
        | None -> "no"
        | Some _ -> "YES"
      );
    pn ()
  )

let show_classtab () =
  ps "\nsummary of Ast.se.Ast.classtab hashtable:\n";
  Hashtbl.iter show_classtab Ast.se.Ast.classtab; pn ();
  pF ()

(* ========================================================================= *)
let ro2str ro =
  "[name:"^obj2str ro^",typ:"^Err.type2str ro.trs_type^
  ",signals:"^string_of_int (List.length ro.trs_sig)^"]"

let tsc_call2str = function
  | ActStmt (_, e, ro) ->
      "ActStmt  expr:"^expr2str e^",ro:"^ro2str ro
  | ActProd (_, ac, e1, e2, _, ro) ->
      "ActProd  expr:"^expr2str e1^" * "^expr2str e2^",ro:"^ro2str ro
  | ActInit (_, sgn, typ, ro) ->
      "ActInit  typ:"^Err.type2str typ^",ro:"^ro2str ro
  | ActSigInit (_, s, xs, ro) ->
      "ActSigInit  signal:"^mf2str (sig2id xs)^",ro:"^ro2str ro
  | ActEmit(_, _, sgn, i, Rct e, cc, xs, ro) ->
      let index =
        match i with
        | [] -> ""
        | [e] -> "["^expr2str e^"]"
        | [e1; e2] -> "["^expr2str e1^","^expr2str e2^"]"
        | _ -> Err.intern "tsc_call2str"
      in
      "ActEmit expr:"^rctexp2str e^",signal:"^mf2str (sig2id xs)^",index:"^index^",ro:"^ro2str ro
  | ActEmit(_, _, sgn, i, Val e, _, xs, ro) ->
      let index =
        match i with
        | [] -> ""
        | [e] -> ",index:"^"["^expr2str e^"]"
        | [e1; e2] -> ",index:"^"["^expr2str e1^","^expr2str e2^"]"
        | _ -> Err.intern "tsc_call2str"
      in
      "ActEmit expr:"^expr2str e^",signal:"^mf2str (sig2id xs)^index^",ro:"^ro2str ro
  | ActBlck(_) ->
      "ActBlck"
  | ActSigPres(_, sgn, xs, ro) ->
      "ActSigPres,signal:"^mf2str (sig2id xs)^",ro:"^ro2str ro
  | ActSigVal(_, sgn, xs, ro) ->
      "ActSigVal,signal:"^mf2str (sig2id xs)^",ro:"^ro2str ro
  | ActSigIn(_, xs, ro) ->
      "ActSigIn,signal:"^mf2str (sig2id xs)^",ro:"^ro2str ro
  | ActSigOut(_, xs, ro) ->
      "ActSigOut,signal:"^mf2str (sig2id xs)^",ro:"^ro2str ro
  | ActPrm(_, s, e, x, ro) ->
      "ActPrm expr:"^expr2str e^",formal:"^
      Sc.sgn2str s^"["^mf2str x.p_name^"]"^
      ",ro:"^ro2str ro
  | ActAssgn (_, sgn, e, _, ro) ->
      "ActAssgn   expr:"^expr2str e^",ro:"^ro2str ro
  | ActAssgnInBlck (_, sgn, e, ro) ->
      "ActAssgnInBlck   expr:"^expr2str e^",ro:"^ro2str ro
  | ActBool (_, sgn, e, ro) ->
      "ActBool  expr:"^expr2str e^",ro:"^ro2str ro
  | ActTimeInit (_, s, ro) ->
      "ActTimeInit  sig:"^Sc.sgn2str s^",ro:"^ro2str ro
  | ActTimeAdd (_, s, ro) ->
      "ActTimeAdd  sig:"^Sc.sgn2str s^",ro:"^ro2str ro
  | ActTimeCond (_, elpsd, stamp, e, ro) ->
      "ActTimeCond  expr:"^expr2str e^",ro:"^ro2str ro
  | ActDbg(ll, ro) ->
      "SyncDbg ro:"^ro2str ro^" state: "^lst_sep2str lbl2str ":" ll

(* =========================================================================
print - functions for #install_printer
========================================================================= *)
open Format ;;

let fmt_pc (c: tclass ) = print_string (c2str c)
let fmt_pr (c: tmfid ) = print_string (mf2str c)
let fmt_t (c: ttype ) = print_string (Err.type2str c)
let fmt_ex (c: texpr ) = print_string (expr2str c)
let fmt_ast (c: tast ) = print_string ("ast of "^c2str c.classid)
let fmt_l (c: tlbl ) = print_string (lbl2str c)
let fmt_ll (c: tlbl list ) = print_string (lst_sep2str lbl2str ":" c)

let fmt_ro (c: trctobj ) = print_string (ro2str c)
let fmt_sync (c: tsc_call ) = print_string (tsc_call2str c)

let fmt_rctexp(c: tsc_exp ) = print_string (rctexp2str c)
let fmt_sgn (c: tsc_id ) =
  try print_string (Sc.sgn2str c)
  with _ -> print_string (string_of_int c)
;;
