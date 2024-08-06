open Ast
open Ly
open P

(* -------------------------------------------------------------------------
miscellaneous functions
------------------------------------------------------------------------- *)
let i2s i = string_of_int i
let s2i s = int_of_string s

let some x t =
  match x with
  | Some x -> x
  | _ -> Err.intern t

let decl2arity dcl =
  match dcl.signature.pt with
  | Some fpl -> List.length fpl
  | None -> - 1

(* -------------------------------------------------------------------------
int64 -> int conversion
------------------------------------------------------------------------- *)
let int64_2_int v =
  if (Int64.compare v (Int64.of_int max_int)) <= 0
  then Int64.to_int v
  else Err.msg (Err.ConversionErr "int64 -> int")

(* -------------------------------------------------------------------------
check, whether debug is requested or not
------------------------------------------------------------------------- *)
type tdebug_level =
  | DbgAssign
  | DbgBinding
  | DbgField
  | DbgDataCall
  | DbgDataCheckPair
  | DbgDataPotRace
  | DbgGoLoad
  | DbgGoRedisplay
  | DbgImport
  | DbgOoSubtype
  | DbgRctCausality
  | DbgRctComp
  | DbgRctCopy
  | DbgRctDecl
  | DbgRctEqu
  | DbgRctReduce
  | DbgRctSplit
  | DbgRctStar
  | DbgSfunction
  | DbgTsort
  | DbgStatus
  
  | DbgParamList

let get_debug_level = function
  | DbgAssign -> ["data";"ass"]
  | DbgBinding -> ["data";"call";"bind";"dyn";"sta"]
  | DbgField -> ["data";"field";"exp";"ref";"ali";"sta";"final"]
  | DbgDataCall -> ["data";"call";"relevant"]
  | DbgDataCheckPair -> ["data";"call";"check";"pair"]
  | DbgDataPotRace -> ["data";"call";"check";"race"]
  | DbgGoLoad -> ["gra";"go";"load"]
  | DbgGoRedisplay -> ["gra";"go";"edit";"redis"]
  | DbgImport -> ["syn";"import";"pack"]
  | DbgOoSubtype -> ["typ";"sub"]
  | DbgRctCausality -> ["rct";"caus"]
  | DbgRctComp -> ["rct";"comp"]
  | DbgRctCopy -> ["rct";"equ";"copy"]
  | DbgRctDecl -> ["rct";"equ";"decl"]
  | DbgRctEqu -> ["rct";"equ"]
  | DbgRctReduce -> ["rct";"equ";"red"]
  | DbgRctSplit -> ["rct";"equ";"split"]
  | DbgRctStar -> ["rct";"equ";"start"]
  | DbgSfunction -> ["rct";"equ";"sfunct"]
  | DbgTsort -> ["sort";"equ"]
  | DbgStatus -> ["data";"field";"exp";"ref";"ali";"sta";"final"]
  
  | DbgParamList -> ["data";"call";"prec";"det";"left";"right"]

let debug_contains str part = (* inefficient string contains substring fn *)
  try let ls = String.length str
      and pl = String.length part in
      let l = ls - pl in
      if l < 0
      then
        false
      else (
        if l = 0
        then str = part
        else (
          for i = 0 to l do
            if (String.sub str i pl) = part
            then raise Pervasives.Exit
            else ()
          done;
          false
        )
      )
  with Pervasives.Exit -> true
  | _ -> Err.intern "debug_contains"

let debug_level code =
  match se.debug_level with
  | None -> false
  | Some str -> if debug_contains str "all"
      then true
      else let sl = get_debug_level code in
        List.exists (debug_contains str) sl

(* -------------------------------------------------------------------------
string auxiliaries
------------------------------------------------------------------------- *)
let is_prefix str pre =
  let lstr = String.length str
  and lpre = String.length pre in
  if lstr < lpre
  then false
  else (
    if lstr = lpre
    then str = pre
    else (String.sub str 0 lpre) = pre
  )

let is_suffix str pre =
  let lstr = String.length str
  and lpre = String.length pre in
  if lstr < lpre
  then false
  else if lstr = lpre
  then str = pre
  else (String.sub str (lstr - lpre) lpre) = pre

let rec is_substring_of sub str =
  let lsub = String.length sub
  and lstr = String.length str in
  if lsub <= lstr
  then
    if sub = String.sub str 0 lsub
    then true
    else is_substring_of sub (String.sub str 1 (String.length str - 1))
  else false

(* normalize_text t ... return text without heading&trailing '\n' *)
let normalize_text t =
  let len = String.length t in
  let f = ref 0
  and l = ref (len - 1) in
  if len = 0
  then " "
  else (
    while f <= l && t.[!f] = '\n' do
      f := !f + 1
    done;
    while f < l && t.[!l] = '\n' do
      l := !l - 1
    done;
    if f > l
    then " "
    else String.sub t !f (!l - !f + 1)
  )
  
(* -------------------------------------------------------------------------
type testing
------------------------------------------------------------------------- *)
let is_reference_typ = function
  | TypeVar _
  | Array _
  | Simple _
  | Any
  | Null -> false
  | _ -> true

let is_array_typ = function
  | Array _ -> true
  | _ -> false

let is_matrix_typ = function
  | Array(_, Arbitrary, _) -> false
  | Array(_, _, Arbitrary) -> false
  | Array _ -> true
  | _ -> false

let is_sensor_typ = function
  | Typ(cid, _) -> cid = id_sensor
  | _ -> false

let is_signal_typ = function
  | Typ(cid, _) -> cid = id_signal || cid = id_delayed
  | _ -> false

let is_delayed_typ = function
  | Typ(cid, _) -> cid = id_delayed
  | _ -> false

let is_sensor_or_signal_typ = function
  | Typ(cid, _) -> cid = id_sensor || cid = id_signal || cid = id_delayed
  | _ -> false

let is_typevar = function
  | TypeVar _ -> true
  | _ -> false

let is_sim_input_or_output_typ = function
  | Typ(cid, _) -> cid = id_sim_input || cid = id_sim_output
  | _ -> false

let is_primitive_typ = function
  | Simple _ -> true
  | Typ(cid, _) -> cid = id_string
  | _ -> false

let is_integral_typ = function
  | Simple cid -> List.mem cid [id_byte; id_char; id_short; id_uint16;
        id_int; id_uint32; id_long; id_uint64]
  | _ -> false

let is_double_or_float_typ = function
  | Simple cid -> cid = id_double || cid = id_float
  | _ -> false

let is_matrix_op mf = mf = id_op_mult || mf = id_op_add
  || mf = id_op_sub || mf = id_op_pointmult
  || mf = id_op_div

(* -------------------------------------------------------------------------
type2id
type2tpl
------------------------------------------------------------------------- *)
let type2id = function
  | Array(cid, DimLen 1 , Arbitrary) -> id_array1
  | Array(cid, Arbitrary, Arbitrary) -> id_array2
  | Array(cid, DimLen 1, _) -> id_vector
  | Array(cid, _, _) -> id_matrix
  | Typ(cid, _) -> cid
  | Simple(cid) -> cid
  | Any -> Err.intern "type2id:any type"
  | Null -> Err.intern "type2id:null type"
  | TypeVar(_) -> Err.intern "type2id:typevar"

let type2tpl = function
  | Array(t, _, _) -> [t]
  | Typ(_, tpl) -> tpl
  | _ -> []

let sigtyp2valtyp typ =
  match typ with
  | Typ(_,[t]) when is_sensor_or_signal_typ typ -> Some t
  | _ -> None

let arraytyp2valtyp = function
  | Array(t, _, _) -> t
  | _ -> Err.intern "arraytyp2valtyp"

(* -------------------------------------------------------------------------
computing dimensions:
------------------------------------------------------------------------- *)
let dim2value = function
  | DimLen n -> n
  | DimRef r -> ( match !(!r) with
        | DimLen n -> n
        | Arbitrary -> 0
        | _ -> Err.intern "dim_value:1"
      )
  | Arbitrary -> 0
  | DimVar _ -> Err.intern "dim_value:Var"

let rec dims2value = function
  | [] -> 0
  | (d, n):: dl -> max (dim2value d + n) (dims2value dl)

(* -------------------------------------------------------------------------
scanning of strings:
------------------------------------------------------------------------- *)
let scanstring valid charfn str pos =
  let l = (String.length str) - 1
  and r = ref pos in
  if (l <= 0)
  then ( 0 )
  else if (l < pos)
  then Err.intern "scanstring"
  else try
        for i = pos to l do
          let result = charfn (String.get str i) in
          if (valid && result) || ( not valid && not result)
          then (r := !r + 1)
          else (raise Not_found)
        done;
        !r
    with Not_found ->
        !r

let whitespace c = (c =' ') || (c ='\t')

let scanwhitespace valid str pos = scanstring valid whitespace str pos

let rmwhitespace str =
  let len = String.length str in
  let left = scanwhitespace true str 0 in
  if left = len
  then ""
  else let rght = scanwhitespace false str left in
    String.sub str left (rght - left)

(* -------------------------------------------------------------------------
(portable) operations for file names
------------------------------------------------------------------------- *)
(* concatenation *)

let is_win32_drive_name fn =
  String.length fn = 2 && String.get fn 1 = ':'

let rec filename_concat = function
  | [] -> ""
  | [f] -> f
  | f:: fl -> Filename.concat
        (if is_win32_drive_name f then f^"\\" else f)
        (filename_concat fl)

(* splitting a file name into list of its components *)
let rec filename_split fn =
  let split_h () =
    let hd = Filename.dirname fn
    and tl = Filename.basename fn in
    if hd = fn || tl = fn then [fn] else
      (filename_split hd) @ [tl]
  in
  match Sys.os_type with
  | "Win32"
  -> if is_win32_drive_name fn
      then [fn]
      else split_h()
  | _ -> split_h()

(* checking whether a path is prefix of a file name
* if yes : the tail is result
* otherwise: None *)
let filename_minus_path file path =
  let rec filename_minus_path_h l p =
    match l, p with
    | [],[] -> Some []
    | [], _ -> None
    | _,[] -> Some l
    | x:: l, y:: p when x = y
    -> filename_minus_path_h l p
    | x:: l, y:: p
    -> None
  in
  let fl = filename_split file and pl = filename_split path in
  filename_minus_path_h fl pl

(* mk_file_relative, mk_file_absolute
* specific for synERJY
* Files are classified whether they occur
* - in the project
* - in the synERJY distribution
* - in the workspace.
* mk_file_relative constructs the relative path
* mk_file_absolute reconstructs the absolute path *)
let mk_path_relative file =
  match filename_minus_path file (Sys.getcwd()) with
  | Some fl -> filename_concat (".":: fl)
  | None ->
      ( match filename_minus_path file se.se_home with
        | Some fl -> filename_concat ("<synERJY>":: fl)
        | None ->
            ( match filename_minus_path file se.workspace with
              | Some fl -> filename_concat ("<workspace>":: fl)
              | None -> file
            )
      )

let mk_path_absolute file =
  let fl = filename_split file in
  match fl with
  | ".":: fl
  -> filename_concat (Sys.getcwd():: fl)
  | "<synERJY>":: fl
  -> filename_concat (se.se_home:: fl)
  | "<workspace>":: fl
  -> filename_concat (se.workspace:: fl)
  | _ -> filename_concat fl

(* -------------------------------------------------------------------------
LIST FUNCTIONS
list_condense condense list: ''condense'' is applied to all elements of
''list''. Returned ''None'' are ignored. The values ''x'' of returned
''Some x'' make up the condensed list.
list_last lst: return the last element from list ''lst''
list_duplicates lst: true if two items of list ''lst'' are ''=''
list_first_match fct lst: return the first item of list ''lst'' for
which ''fct'' returns true
list_replace ...
list_insert ...
list_suffix ...
list_prefix
HASHTBL FUNCTIONS
hashtbl_condense behaves like list_condense
------------------------------------------------------------------------- *)
let rec list_condense condense = function
  | [] -> []
  | h:: t -> let t = list_condense condense t in
      ( match condense h with
        | None -> t
        | Some h -> h:: t
      )

let hashtbl_condense condense hashtbl =
  let acc = ref [] in
  let hashtbl_condense key vl =
    match condense key vl with
    | None -> ()
    | Some a -> acc := a :: !acc
  in
  Hashtbl.iter hashtbl_condense hashtbl;
  !acc

let rec list_last = function
  | [] -> Err.intern "list_last"
  | [l] -> l
  | h:: l -> list_last l

let rec list_duplicates = function
  | [] -> false
  | h:: t -> if List.mem h t
      then true
      else list_duplicates t

let rec list_first_match f = function
  | [] -> raise Not_found
  | h:: t -> if (f h)
      then h
      else list_first_match f t

let list_replace l1 s e l2 =
  let n = List.length l1 in
  if s < 0 || e < s || e >= n || n = 0 then (
    Err.intern "list_replace"
  ) else (
    let rec list_replace n = function
      | [] -> []
      | x:: l -> if n <= s
          then x:: (list_replace (n + 1) l)
          else if n <= e
          then list_replace (n + 1) l
          else l2@l
    in
    list_replace 1 l1
  )

let list_insert l1 s l2 =
  let n = List.length l1 in
  if s < 0 || s > n then (
    Err.intern "list_insert"
  ) else (
    if n = 0 then (
      l2
    ) else if s = n then (
      l1@l2
    ) else (
      let rec list_insert n = function
        | [] -> []
        | x:: l -> if n <= s
            then x:: list_insert (n + 1) l
            else l2@(x:: l)
      in
      list_insert 1 l1
    )
  )

let rec list_suffix suf = function
  | [] -> suf
  | l:: ll -> match list_suffix suf ll with
      | [] -> []
      | s:: sl -> if s = l
          then sl
          else raise Not_found

let list_suffix suf lst =
  let suf = List.rev suf in
  try
      ( match list_suffix suf lst with
        | [] -> true
        | _ -> false
      )
  with _ -> false

let rec list_prefix = function
  | [], _ -> true
  | _,[] -> false
  | p:: pl, l:: ll -> if p = l
      then list_prefix (pl, ll)
      else false

let list_prefix ~pre ~full =
  list_prefix (pre, full)

(* =========================================================================
functions on sorted lists, members of any type [sosgl]
functions on t1 * t2 list sorted by the first component [map]
========================================================================= *)
let rec add_sosgl x = function
  | [] -> [x]
  | (h:: t) as l -> if x < h
      then (x:: l)
      else if x = h then l
      else (h:: (add_sosgl x t))

let rec mem_sosgl x = function
  | [] -> false
  | (h:: t) -> if x > h
      then (mem_sosgl x t)
      else if x = h then true
      else false

let rec join_sosgl l1 l2 =
  match (l1, l2) with
  | ([], l) -> l
  | (l,[]) -> l
  | (h1:: t1, h2:: t2) -> if h1 < h2
      then (h1:: (join_sosgl t1 l2))
      else if h1 > h2
      then (h2:: (join_sosgl l1 t2))
      else (h2:: (join_sosgl t1 t2))

let rec cut_sosgl l1 l2 =
  match (l1, l2) with
  | ([], l) -> []
  | (l,[]) -> []
  | (h1:: t1, h2:: t2) -> if h1 < h2
      then (cut_sosgl t1 l2)
      else if h1 > h2
      then (cut_sosgl l1 t2)
      else (h1:: (cut_sosgl t1 t2))

let rec mk_list_sosgl = function
   [] -> []
  | h:: t -> let s = mk_list_sosgl t in
      if (mem_sosgl h s)
      then s
      else (add_sosgl h s)

let join_list2sosgl l sosgl =
  (join_sosgl (mk_list_sosgl l) sosgl)

let imp_sosgl = ref [0]
let imp_begin_sosgl () = imp_sosgl := []
let imp_end_sosgl () = !imp_sosgl
let imp_add_sosgl x = imp_sosgl := add_sosgl x !imp_sosgl
let imp_join_sosgl x = imp_sosgl := join_sosgl x !imp_sosgl
let imp_addl_sosgl x = imp_sosgl := join_sosgl (mk_list_sosgl x) !imp_sosgl

(* ------------------------------------------------------------------------- *)
let rec get_map x = function
  | [] -> raise Not_found
  | (k, v) :: t -> if x > k
      then (get_map x t)
      else if x = k
      then v
      else raise Not_found

let rec iter_map f = function
  | [] -> ()
  | (k, v):: t -> f k v; iter_map f t

let is_mapped x kvl =
  try
      let _ = get_map x kvl in
      true
  with Not_found ->
      false

let rec add_map kv = function
  | [] -> [kv]
  | h:: t -> if (fst kv) < (fst h)
      then kv:: h:: t
      else if (fst kv) > (fst h)
      then h:: (add_map kv t)
      else if (snd kv) = (snd h)
      then h:: t
      else Err.intern "add_map"

let rec mk_map l =
  List.fold_left (fun l kv -> add_map kv l) [] l

let rec add_maplist kv = function
  | [] -> [kv]
  | h:: t -> if (fst kv) < (fst h)
      then kv:: h:: t
      else if (fst kv) > (fst h)
      then h:: (add_maplist kv t)
      else (fst kv, (snd kv)@(snd h)):: t

let rec merge_map l1 l2 =
  match (l1, l2) with
  | ([], l) -> l
  | (l,[]) -> l
  | (h1:: t1, h2:: t2) -> let f1 = fst h1 and f2 = fst h2 in
      if f1 < f2
      then (h1:: (merge_map t1 l2))
      else if f1 > f2
      then (h2:: (merge_map l1 t2))
      else Err.intern "merge_map"

let rec merge_maplist l1 l2 =
  match (l1, l2) with
  | ([], l) -> l
  | (l,[]) -> l
  | (h1:: t1, h2:: t2) -> let f1 = fst h1 and f2 = fst h2 in
      if f1 < f2
      then (h1:: (merge_maplist t1 l2))
      else if f1 > f2
      then (h2:: (merge_maplist l1 t2))
      else let s = (snd h1)@(snd h2) in
        (f1, s):: (merge_maplist t1 t2)

let rec rm_map x = function
  | [] -> raise Not_found
  | h:: t -> if (fst h) = x
      then (snd h, t)
      else let (selected, reduced) = rm_map x t
        in
        (selected, h:: reduced)

(* -------------------------------------------------------------------------
accessor functions: extract data from abstract syntax tree
- a_..... expects the ast as argument
- i_..... expects the id of a class as argument
- st_..... expects a symbol table as argument
- o_.... expects an object string as argument (typically trs_name)
------------------------------------------------------------------------- *)
let i_ast cid =
  try Hashtbl.find se.classtab cid
  with Not_found ->
      Err.msg (Err.MissingClass(cid))

let i_extends cid =
  (i_ast cid).extends

let i_implements cid =
  (i_ast cid).implements

let i_lbl2pos cid lbl =
  let lbl2pos = (i_ast cid).lbl2sp in
  Ly.lbl2srcp lbl lbl2pos

let i_entry cid mfid len =
  match Hashtbl.find (i_ast cid).symtab (mfid, len) with
  | entry:: _ -> entry
  | _ -> Err.intern "i_entry"

let i_class_precl cid =
  let xtract = function
    | ClassPrec(il) -> Some il
    | _ -> None
  in
  list_condense xtract (i_ast cid).specs

let i_obj_precl cid =
  let xtract = function
    | ObjPrec(s, il) -> Some (s, il)
    | _ -> None
  in
  list_condense xtract (i_ast cid).specs

let rctclass2constr ast =
  let cid = Ly.c2mf ast.classid in
  let entry = ref NoEntry in
  let tst (c, a) decll =
    if c = cid then (
      match decll with
      | [dcl] -> entry := dcl.entry
      | _ -> Err.intern "rctclass2constr:st"
    ) else ()
  in
  Hashtbl.iter tst ast.symtab;
  match !entry with
  | Constructor c -> c
  | _ -> Err.intern "rctclass2constr:nocstr"

(* -------------------------------------------------------------------------
type variable substitution as used in the code generator
prepare_subst : ttype -> (type variable, concrete type) list [[~~map]]
subst_map : type variable -> (type variable, concrete type) list ->
concrete type
------------------------------------------------------------------------- *)
let rec combine_to_map = function
  | [],[] -> []
  | f:: fl, a:: al -> if is_typevar a
      then raise (Invalid_argument "")
      else add_map (f, a) (combine_to_map (fl, al))
  | _ -> raise (Invalid_argument "")

let prepare_subst typ =
  let cid = type2id typ in
  let ast = i_ast cid in
  let atl = type2tpl typ in
  let ftl = List.map (fun (f, _) -> f) ast.typparams in
  try
      combine_to_map (ftl, atl)
  with Invalid_argument _ ->
      Err.intern ("prepare_subst: "^Err.type2str typ)

let rec subst_map typ tv2t_l =
  match typ with
  | TypeVar(n) -> ( try get_map n tv2t_l
      with _ -> Err.intern "subst_map"
      )
  | Typ(cid, tl) -> Typ(cid, List.map (fun t -> subst_map t tv2t_l) tl)
  | Array(t, d1, d2) -> Array(subst_map t tv2t_l, d1, d2)
  | t -> t

(* -------------------------------------------------------------------------
check, whether a field can be expanded
------------------------------------------------------------------------- *)
let can_expand dcl =
  match dcl.entry with
  | Field fld -> ( match fld.assigns with
        | [NewInit _] -> true
        | [NewCons _] -> true
        | [AtInit _] when dcl.scope = Single
        -> true
        | [AtCons _] when dcl.scope = Single
        -> true
        | _ -> false )
  | _ -> Err.intern "can_expand"

let get_expandtyp dcl =
  let gn = "get_expandtyp" in
  match dcl.entry with
  | Field fld -> ( match fld.assigns with
        | [NewInit e] -> some e.etyp gn
        | [NewCons e] -> some e.etyp gn
        | [AtInit e] when dcl.scope = Single
        -> let t = dcl.signature.rt in
            ( match t with
              | Simple _
              -> t
              | Typ(cid, _) when cid = id_string
              -> t
              | _ -> dcl.signature.rt
            )
        | [AtCons e] when dcl.scope = Single
        -> some e.etyp gn
        | _ -> Err.intern gn )
  | _ -> Err.intern gn

(* -------------------------------------------------------------------------
clear_symtab_from_inheritance cid ...
removes all entries from the symboltable, that are not part of ''cid'',
but inherited
------------------------------------------------------------------------- *)
let mk_symtab_entry cid st dcl =
  let key = dcl.name in
  let arity = decl2arity dcl in
  try
      let _ = Hashtbl.find st (key, arity) in
      Err.intern "mk_symtab_entry"
  with Not_found ->
      Hashtbl.add st (key, arity) [dcl]

let clear_symtab_from_inheritance cid =
  let ast = i_ast cid in
  let symtab = ast.symtab in
  Hashtbl.clear symtab;
  List.iter (fun d -> mk_symtab_entry cid symtab d) ast.declseq

(* -------------------------------------------------------------------------
manage the status of classes, deliver and update status values
the constructors of upd_status are totally ordered, Unknown is bottom,
NoUpdate is top element.
------------------------------------------------------------------------- *)
let set_class_sta cid sta =
  try
      let ast = Hashtbl.find se.classtab cid in
      ast.class_sta.class_status <- sta
  with Not_found -> Err.intern "set_class_sta"

(* class status may exist, if not, return the class status Unknown ---------- *)
let i_class_status cid =
  try
      (Hashtbl.find se.classtab cid).class_sta.class_status
  with Not_found ->
      Unknown

let class_sta2int = function
  | Unknown -> 1
  | Parsed -> 2
  | InheritChecked -> 3
  | TypeChecked -> 4
  | Synchron -> 5
  | NoUpdate -> 9

let a_guarantee_status ast sta =
  let t = ast.class_sta in
  if class_sta2int (t.class_status) < (class_sta2int sta)
  then Err.msg (Err.Unchecked(ast.classid))
  else t

let i_guarantee_status cid sta =
  try
      a_guarantee_status (Hashtbl.find se.classtab cid) sta
  with Not_found ->
      Err.msg (Err.Unchecked(cid))

let decrease_to_parsed cid =
  try
      let t = (Hashtbl.find se.classtab cid).class_sta in
      if t.class_status = NoUpdate
      then ()
      else ( t.class_status <- Parsed;
        clear_symtab_from_inheritance cid
      )
  with Not_found -> ()

(* store the methods from other classes, that are called as:
- ordinary methods - class methods - constructors *)
let set_class_use cid meth_called class_crtd class_dyn_crtd
  class_meth_called array_lits =
  let cs = i_guarantee_status cid TypeChecked
  in
  cs.methods_called <- meth_called;
  cs.classes_created <- class_crtd;
  cs.classes_dyn_created <- class_dyn_crtd;
  cs.class_methods_called <- class_meth_called;
  cs.array_lits <- array_lits

let set_class_typ_constrl cid constrl =
  let cs = i_guarantee_status cid Parsed in
  cs.typ_constrl <- constrl

let set_class_sync_beh cid rc =
  let cs = i_guarantee_status cid TypeChecked in
  cs.class_behavior <- rc

let i_called_methods c = (i_guarantee_status c TypeChecked).methods_called
let i_creates c = (i_guarantee_status c TypeChecked).classes_created
let i_dyn_creates c = (i_guarantee_status c TypeChecked).classes_dyn_created
let i_class_methods c = (i_guarantee_status c TypeChecked).class_methods_called
let i_arraylits c = (i_guarantee_status c TypeChecked).array_lits
let i_typ_constrl c = (i_guarantee_status c InheritChecked).typ_constrl
let a_typ_constrl a = (a_guarantee_status a InheritChecked).typ_constrl

let i_sync_beh c = let c = (i_guarantee_status c Synchron).class_behavior
  in
  some c "i_sync_beh"

let class_status cid compf sta =
  let cs = i_class_status cid in
  compf (class_sta2int cs) (class_sta2int sta)

(* =========================================================================
rm_from_classtab
add_to_classtab
REMOVE / REPLACE an entry in the class hash table.
========================================================================= *)
let rm_from_classtab cid =
  try
      let ast = Hashtbl.find se.classtab cid in
      let sta = ast.class_sta in
      if sta.class_status = NoUpdate
      then ( Err.intern "rm_from_classtab" )
      else ( Hashtbl.remove se.classtab cid )
  with Not_found -> ()

let add_to_classtab ast =
  let cid = ast.classid in
  rm_from_classtab cid;
  Hashtbl.add se.classtab cid ast

(* -------------------------------------------------------------------------
get_initexpr: extract init expression out of field decl
get_siginitexpr_after_tc: extract init expression out of typecheck result,
i.e. either from decl or constructor
get_fldinitexpr_after_tc: extract init expression out of typecheck result,
i.e. either from decl or constructor
------------------------------------------------------------------------- *)
let get_initexpr dcl =
  match dcl.entry with
  | Field f -> ( match f.init with
        | Some e -> e
        | _ -> raise Not_found
      )
  | SigDecl s -> ( match s.sig_init with
        | Some e -> e
        | _ -> raise Not_found
      )
  | _ -> raise Not_found

let get_siginitexpr_after_tc sdecl =
  match sdecl.sig_assigns with
  | [AtInit e] -> Err.intern "get_siginitexpr_after_tc"
  | [NewInit e]
  | [AtCons e]
  | [NewCons e] -> Some e
  | _ -> None

let get_fldinitexpr_after_tc dcl =
  let fdcl = match dcl.entry with
    | Field f -> f
    | _ -> Err.intern "get_fldinitexpr_after_tc:1" in
  match fdcl.assigns with
  | [AtInit e]
  | [NewInit e]
  | [AtCons e]
  | [NewCons e] -> e
  | [AtCode e] when dcl.scope = Single
  -> e
  | _ -> Err.intern "get_fldinitexpr_after_tc:2"

(* -------------------------------------------------------------------------
sdecl2initkind ... for callback objects ...
------------------------------------------------------------------------- *)
let sdecl2initkind sdcl =
  let v = { etyp = Some Null; expr = NullObj; elbl = Ly.nolbl } in
  let e, at_decl, visible = match sdcl.sig_assigns with
    | [NewInit e] -> e, true , true
    | [NewCons e] -> e, false, true
    | [AtCons _] -> v, false, false
    | _ -> Err.intern "sdecl2initkind" in
  if not visible
  then ( None, at_decl )
  else ( let cbobj = match e.expr with
      | New(_,[cb]) -> Some cb
      | New(_,[]) -> None
      | _ -> Err.intern "sdecl2initkind" in
    cbobj, at_decl )

(* -------------------------------------------------------------------------
get_decl_after_tc cid mf plen =
get the declaration of identifier mf with arity plen (field == (- 1))
--> this is a simplified version from typecheck.ml <-
--> all error checking is removed <-
--> may only be used AFTER the type checker <-
------------------------------------------------------------------------- *)
let rec get_decl_after_tc cid fid plen =
  let ast = i_ast cid in
  try
      match Hashtbl.find ast.symtab (fid, plen) with
      | dcl:: _ -> dcl
      | _ -> Err.intern "get_decl_after_tc:[]"
  with Not_found ->
      ( match ast.classkind with
        | Anonymous cid' -> get_decl_after_tc cid' fid plen
        | _ -> Err.intern ("get_decl_after_tc:"^(c2str cid)
                ^":"^(mf2str fid)^"/"^(i2s plen))
      )

(* ------------------------------------------------------------------------- *)
let is_field dcl =
  match dcl.entry with
  | Field _ -> true
  | _ -> false

let native2name dcl =
  match dcl.entry with
  | Field f -> ( match f.fldkind with
        | NativeFromC s -> Some s
        | NativeToC s -> Some s
        | _ -> None
      )
  | _ -> None

let native2fromC dcl =
  match dcl.entry with
  | Field f -> ( match f.fldkind with
        | NativeFromC s -> Some s
        | _ -> None
      )
  | _ -> None

let native2toC dcl =
  match dcl.entry with
  | Field f -> ( match f.fldkind with
        | NativeToC s -> Some s
        | _ -> None
      )
  | _ -> None

let is_signal dcl =
  match dcl.entry with
  | SigDecl _ -> true
  | _ -> false

let is_label dcl =
  match dcl.entry with
  | Label -> true
  | _ -> false

let is_input_sig xs =
  match xs.trs_principal with
  | Visible -> is_sensor_typ xs.trs_styp
  | Invisible -> false
  | _ -> Err.intern "is_input_sig"

let is_output_sig xs =
  match xs.trs_principal with
  | Visible -> not (is_sensor_typ xs.trs_styp)
  | Invisible -> false
  | _ -> Err.intern "is_output_sig"

let is_local_sig xs =
  match xs.trs_principal with
  | Visible -> false
  | Invisible -> true
  | _ -> Err.intern "is_local_sig"

(* ------------------------------------------------------------------------- *)
let is_method_or_constructor dcl =
  match dcl.entry with
  | Method _
  | Constructor _ -> true
  | _ -> false

let is_rct_method dcl =
  match dcl.entry with
  | Method m -> ( match m.method_kind with
        | BttmMethod -> Err.intern "is_method_or_constructor"
        | DataMethod
        | Abstract
        | Internal
        | NativeMethod _
        | NativeIntr _ -> false
        | RctMethod _
        | GraphicCall _
        | RctNode _ -> true
      )
  | _ -> false

let is_constructor dcl =
  match dcl.entry with
  | Constructor _ -> true
  | _ -> false

let is_staticfinal dcl =
  dcl.scope = Class && dcl.final

let is_inherited = function
  | NoEntry -> Err.intern "is_inherited"
  | Field _ -> true
  | Method m -> ( match m.method_kind with
        | Internal -> false
        | _ -> true
      )
  | Constructor _ -> false
  | SigDecl _ -> true
  | ConstLocal
  | MutableLocal -> Err.intern "is_inherited"
  | Label -> true

let is_abstract dcl =
  match dcl.entry with
  | Method m -> ( match m.method_kind with
        | Abstract -> true
        | _ -> false
      )
  | _ -> false

let is_reactive dcl =
  match dcl.entry with
  | Method m -> ( match m.method_kind with
        | RctMethod _ -> true
        | _ -> false
      )
  | _ -> false

let is_abstract_ast ast =
  let ck = ast.classkind in
  ck = AbstractClass || ck = Interface

let is_abstract_class cid =
  is_abstract_ast (i_ast cid)

(* -------------------------------------------------------------------------
is_class_subtype ~sub ~super ...
le_type sup super ... is true if ''sub'' is a sub - type of or equal
to ''super''
most_general_type tl ... generates the most general type of a list of types

sE does not typecheck its simple types as Java does typecheck its
numeric types. Simple types have to be identical, no coersion is
applied automatically.
------------------------------------------------------------------------- *)
let rec is_class_subtype ~sub ~super =
  if sub = super
  then true
  else let ast = i_ast sub in
    if List.exists (fun c -> is_class_subtype ~sub: (type2id c) ~super)
      ast.implements
    then true
    else if ast.extends = Any
    then false
    else is_class_subtype ~sub: (type2id ast.extends) ~super

let cint8l = id_cint8 :: id_cuint7 ::[]
let cint16l = id_cint16:: id_cuint8 :: id_cuint15:: cint8l
let cint32l = id_cint32:: id_cuint16:: id_cuint31:: cint16l
let cint64l = id_cint64:: id_cuint32:: id_cuint63:: cint32l

let cuint7l = id_cuint7 ::[]
let cuint8l = id_cuint8 :: cuint7l
let cuint15l = id_cuint15:: cuint8l
let cuint16l = id_cuint16:: cuint15l
let cuint31l = id_cuint31:: cuint16l
let cuint32l = id_cuint32:: cuint31l
let cuint63l = id_cuint63:: cuint32l
let cuint64l = id_cuint64:: cuint63l

let is_simple_subtype ~sub ~super =
  if sub = super
  then true
  else let l = if super = id_byte then cint8l else
      if super = id_char then cuint8l else
      if super = id_short then cint16l else
      if super = id_uint16 then cuint16l else
      if super = id_int then cint32l else
      if super = id_uint32 then cuint32l else
      if super = id_long then cint64l else
      if super = id_uint64 then cuint64l else
      if super = id_cint8 then cint8l else
      if super = id_cuint7 then cuint7l else
      if super = id_cuint8 then cuint8l else
      if super = id_cint16 then cint16l else
      if super = id_cuint15 then cuint15l else
      if super = id_cuint16 then cuint16l else
      if super = id_cint32 then cint32l else
      if super = id_cuint31 then cuint31l else
      if super = id_cuint32 then cuint32l else
      if super = id_cint64 then cint64l else
      if super = id_cuint63 then cuint63l else
      if super = id_cuint64 then cuint64l else
      if super = id_float || super = id_double ||
      super = id_time || super = id_bool
      then []
      else Err.intern "is_simple_subtype" in
    List.mem sub l

let rec le_dim_h compare ~sub ~super =
  match sub, super with
  | _, Arbitrary -> true
  | DimVar _, DimLen _
  | DimLen _, DimVar _ -> true
  | DimLen x, DimLen y -> compare x y
  | DimVar x, DimVar y -> x = y
  | DimRef r, DimVar _
  | DimRef r, DimLen _ -> !r := super; true
  | DimVar _ , DimRef r
  | DimLen _ , DimRef r -> !r := sub; true
  | DimRef r1, DimRef r2 -> r1 = r2 || !r1 = !r2 ||
      ( match !(!r1),!(!r2) with
        | _ , Arbitrary -> !r2 := !(!r1);
            r2 := !r1;
            true
        | Arbitrary, _ -> !r1 := !(!r2);
            r1 := !r2;
            true
        | _, DimLen n2 -> r2 := !r1;
            true
        | DimLen n1, _ -> r1 := !r2;
            true
        | _ -> false
      )
  | _ -> false

let eq_dim ~sub ~super = le_dim_h (=) ~sub: sub ~super: super
let le_dim ~sub ~super = le_dim_h (<=) ~sub: sub ~super: super

let rec le_type ~sub ~super =
  match (sub, super) with
  | (t1, t2) when t1 = t2 -> true (* incl identitiy of type vars *)
  | (Simple i1, Simple i2) -> is_simple_subtype ~sub: i1 ~super: i2
  | (Simple t1, _) -> false
  | (_, Simple t2) -> false
  | Array(t1, d11, d12), Array(t2, d21, d22) ->
      t1 = t2 && (eq_dim ~sub: d11 ~super: d21) && (eq_dim ~sub: d12 ~super: d22)
  | (Typ(i1, t1), Typ(i2, t2)) ->
      (i1 = i2) && (t1 = t2)
      || i2 = id_object && t2 = []
      || (is_class_subtype ~sub: i1 ~super: i2) &&
      if t1 = [] && t2 = []
      then true
      else ( try
          let cmp t1 t2 = t1 = t2 || le_type ~sub: t1 ~super: t2 in
          List.for_all2 cmp t1 t2
      with _ -> false
      )
  | (Null, Typ(_, _)) -> true
  | (_, Any) -> true
  | _ -> false

let rec most_general_type = function
  | [] -> Null
  | [t] -> t
  | t1:: tl -> let t2 = most_general_type tl in
      if le_type ~sub: t1 ~super: t2 then (
        t2
      ) else if le_type ~sub: t2 ~super: t1 then (
        t1
      ) else (
        Null
      )

(* -------------------------------------------------------------------------
features of builtin classes / datatypes
------------------------------------------------------------------------- *)
let builtin_info cid =
  let cid = Err.ctyp2typ cid in
  
  if cid = id_bool then (
    "se_Bool", "B","SS_BOOLEAN","", "(se_Bool) False"
  ) else if cid = id_char then (
    "se_Char", "c","SS_UINT8", "%hd","(se_Char) 0"
  ) else if cid = id_byte then (
    "se_Byte", "b","SS_INT8", "%hd","(se_Byte) 0"
  ) else if cid = id_short then (
    "se_Short", "s","SS_INT16", "%hd","(se_Short) 0"
  ) else if cid = id_uint16 then (
    "se_Uint16","S","SS_UINT16", "%hu","(se_Uint16) 0"
  ) else if cid = id_int then (
    "se_Int", "i","SS_INT32", "%d", "(se_Int) 0"
  ) else if cid = id_uint32 then (
    "se_Uint32","I","SS_UINT32", "%u", "(se_Uint32) 0"
  ) else if cid = id_long then (
    "se_Long", "l","N Y I", "%ld","(se_Long) 0"
  ) else if cid = id_uint64 then (
    "se_Uint64","L","N Y I", "%lu","(se_Uint64) 0"
  ) else if cid = id_float then (
    "se_Float", "f","SS_SINGLE", "%f", "(se_Float) 0.0"
  ) else if cid = id_double then (
    "se_Double","d","SS_DOUBLE", "%f", "(se_Double) 0.0"
  ) else if cid = id_string then (
    "se_String","Q","N Y I", "\\\"%s\\\"", "(se_String) \"\""
  ) else if cid = id_time then (
    "se_Time", "t","N Y I", "", "(se_Time) 0"
  ) else (
    raise Not_found
  )

(* ------------------------------------------------------------------------- *)
let is_access_allowed ~forcid ~tocid access =
  match access with
  | Protected -> is_class_subtype ~sub: forcid ~super: tocid
  | Public cl -> cl = [id_object] ||
      is_class_subtype ~sub: forcid ~super: tocid ||
      List.exists (fun c -> is_class_subtype ~sub: forcid ~super: c)
        cl
  | Private -> false

let is_newexpr expr =
  match expr.expr with
  | New(_, _) -> true
  | _ -> false

let classkind2str ast =
  match ast.classkind with
  | AbstractClass -> "abstract class"
  | EffectiveClass -> "class"
  | Anonymous cid -> "anon class [in: "^(c2str cid)^"]"
  | FinalClass -> "final class"
  | RctClass -> "reactive class"
  | ConfClass -> "configuration class"
  | Interface -> "interface"

(* -------------------------------------------------------------------------
extract a specification by name from a list
------------------------------------------------------------------------- *)
let rec get_tag fid = function
  | [] -> raise Not_found
  | h:: t -> ( match h with
        | Ptl(f, _) when f = fid -> h
        | Ctl(f, _, _) when f = fid -> h
        | _ -> get_tag fid t
      )

(* -------------------------------------------------------------------------
operate on [ast.] symtab
------------------------------------------------------------------------- *)
let fromsymtab selfn symtab =
  let sel = ref [] in
  let get (id, n) = function
    | d:: _ -> ( match selfn id n d with
          | None -> ()
          | Some x -> sel := x :: !sel
        )
    | _ -> Err.intern "fromsymtab"
  in
  Hashtbl.iter get symtab;
  !sel

let i_fromsymtab selfn cid =
  let symtab = (i_ast cid).symtab in
  fromsymtab selfn symtab

let symtabiter iterfn symtab =
  let iter (id, n) = function
    | d:: _ -> iterfn id n d
    | _ -> Err.intern "symtabiter"
  in
  Hashtbl.iter iter symtab

let i_symtabiter iterfn cid =
  symtabiter iterfn (i_ast cid).symtab

let symtab_entry mfid len symtab =
  match Hashtbl.find symtab (mfid, len) with
  | entry:: _ -> entry
  | _ -> Err.intern "symtab_entry"
(*
(* -------------------------------------------------------------------------
from_stmtl: select stmt - parts selected by a selector - fn
------------------------------------------------------------------------- *)
let fromstmtl selfn_stmt stmtl =
let sel = ref [] in
let rec f_sl sl =
List.iter f_s sl
and f_s s =
( match selfn_stmt s with
| None -> ()
| Some r -> sel := r::!sel
);
( match s with
| Await(_, _, tl) -> f_thenl tl
| Activate(_, _, sl, e) -> f_sl sl; f_e e
| AssertStmt(_, _) -> ()
| TextStM(_, a) -> f_state a.a_init; List.iter f_state a.a_states
| GraphicStM(_, g) -> ( match g.gstm_stmt with
| None -> Err.intern "fromstmtl"
| Some s -> f_s s
)
| Break(_, _) -> ()
| Continue(_, _) -> ()
| DoStmt(_, e, sl) -> f_sl sl; f_e e
| Emit(_, _, e) -> f_e e
| FlowEqu(_, _, e) -> f_e e
| ExprStmt(_, e) -> f_e e
| ForStmt(_, f) -> ( match f.forinit with
| None -> ()
| Some e -> f_e e
);
( match f.fortest with
| None -> ()
| Some e -> f_e e
);
( match f.forupd with
| None -> ()
| Some e -> f_e e
);
f_sl f.forstmtl
| Halt(_) -> ()
| LetStmt(_, l) -> ( match l.letexpr with
| None -> ()
| Some e -> f_e e
);
f_sl l.letil
| Next(_) -> ()
| NextState(_, _) -> ()
| Nothing(_) -> ()
| FlowContext(_, fc) -> f_sl fc.f_dcls; f_sl fc.f_equ
| Par(_, sll) -> List.iter f_sl sll
| Cancel(_, _, _, sl, tl) -> f_sl sl; f_thenl tl
| RctLoop(_, sl) -> f_sl sl
| Return(_, e) -> ( match e with
| None -> ()
| Some e -> f_e e
)
| Schedule(_, e) -> f_e e
| Sustain(_, sl) -> f_sl sl
| Switch(_, s) -> f_e s.swexpr;
List.iter (fun c -> f_sl c.sstmtl) s.swcase;
f_sl s.swdflt
| Throw(_, e) -> f_e e
| While(_, e, sl) -> f_e e; f_sl sl
)
and f_state a =
f_sl a.sdo; f_sl a.sentry; f_sl a.sexit; f_sl a.sduring;
f_thenl a.strans
and f_thenl tl =
List.iter f_then tl
and f_then = function
| Then(_, stl) -> f_sl stl
| Else(_, stl) -> f_sl stl
and f_e e =
match e.expr with
| Dot(e1, e2) -> f_e e1; f_e e2
| Call(_, None) -> ()
| Call(_, Some el) -> List.iter f_e el
| ClassCall(_, _, None) -> ()
| ClassCall(_, _, Some el) -> List.iter f_e el
| New(_, el) -> List.iter f_e el
| Cast(_, e) -> f_e e
| This -> ()
| Super(_, el) -> List.iter f_e el
| ThisConstr(el) -> List.iter f_e el
| SuperConstr(el) -> List.iter f_e el
| NullObj -> ()
| IncrDecr(_, _) -> ()
| Assign(_, NoArr, _, e) -> f_e e
| Assign(_, ADim1 i, _, e) -> f_e i; f_e e
| Assign(_, ADim2(x, y), _, e) -> f_e x; f_e y; f_e e
| Literal(_) -> ()
| ArrayLiteral(_) -> ()
| If(tl) -> f_thenl tl
| RealTime(_) -> ()
| DotDot(_) -> ()
| Present(_) -> ()
| Timestamp(_) -> ()
| SigVal(_) -> ()
| Value(_) -> ()
| Var(_) -> ()
| Instant -> ()

in
f_sl stmtl;
!sel
*)
(* -------------------------------------------------------------------------
some aux functions
------------------------------------------------------------------------- *)
let is_reactive_classkind ck =
  ck = RctClass || ck = ConfClass

let is_reactive_class cid =
  is_reactive_classkind (i_ast cid).classkind

let is_reactive_type typ =
  is_reactive_class (type2id typ)

let is_literal expr =
  match expr.expr with
  | Literal _ -> true
  | ArrayLiteral _ -> true
  | _ -> false

let is_assign expr =
  match expr.expr with
  | Assign(_, _, _, _) -> true
  | IncrDecr(_, _) -> true
  | _ -> false

let gen_for_simulation () =
  match se.target_sys with
  | Simulation
  | Simulink
  | Scicos
  | VerilogSimulation
  | Verilog _
  | Verification -> true
  | Host
  | Makefile
  | Platform _ -> false

let gen_for_targetsys () =
  match se.target_sys with
  | Host
  | Makefile
  | Platform _ -> true
  | _ -> false

(* -------------------------------------------------------------------------
get literal value of a field AFTER typechecking
------------------------------------------------------------------------- *)
let field2literal cid fid =
  try
      let dcl = get_decl_after_tc cid fid (- 1) in
      if is_staticfinal dcl then (
        match (get_fldinitexpr_after_tc dcl).expr (* may raise exc *) with
        | Literal l -> l
        | _ -> raise Pervasives.Exit
      ) else (
        raise Pervasives.Exit
      )
  with _ -> Err.intern "field2literal"

(* -------------------------------------------------------------------------
evaluate constant int expressions
------------------------------------------------------------------------- *)
let rec bs_const_expr cid expr =
  match expr.expr with
  | Dot(e1, e2)
  -> let v1 = bs_const_expr cid e1 in
      bs_const_op cid v1 e2
  | Call(f, None)
  -> let dcl = get_decl_after_tc cid f (- 1) in
      if is_staticfinal dcl
      then ( bs_const_expr cid (get_initexpr dcl) )
      else ( raise Not_found )
  | ClassCall(cid, f, None)
  -> let dcl = get_decl_after_tc cid f (- 1) in
      if is_staticfinal dcl
      then ( bs_const_expr cid (get_initexpr dcl) )
      else ( raise Not_found )
  | _ -> bs_literal expr.expr

and bs_literal = function
  | Literal (t, s, Some i) -> i
  | _ -> raise Not_found

and bs_const_op cid vlhs erhs =
  match erhs.expr with
  | Call(o, Some [e]) -> let vrhs = bs_const_expr cid e in
      if o = id_op_add then Int64.add vlhs vrhs else
      if o = id_op_sub then Int64.sub vlhs vrhs else
      if o = id_op_mult then Int64.mul vlhs vrhs else
      if o = id_op_div then Int64.div vlhs vrhs else
        raise Not_found
  | Call(o, Some []) -> if o = id_op_minus
      then Int64.neg vlhs
      else raise Not_found
  | _ -> raise Not_found

let eval_const_expr enc_cid expr =
  try
      bs_const_expr enc_cid expr
  with Not_found ->
      Err.msg (Err.NYI "this expression should be a constant integral \
          expression, but contains illegal (variable) components")

let try_eval_const_expr_2_int enc_cid expr =
  try Some (int64_2_int (bs_const_expr enc_cid expr))
  with Not_found -> None

let eval_const_expr_2_int enc_cid expr =
  int64_2_int (eval_const_expr enc_cid expr)

(* -------------------------------------------------------------------------
get static final int that describe system characteristics
------------------------------------------------------------------------- *)
let get_sysprop conf_class name =
  let cid = type2id conf_class in
  try
      match Hashtbl.find (i_ast cid).symtab (name,- 1) with
      | dcl:: _ -> if dcl.signature.rt = t_int &&
          dcl.signature.pt = None &&
          dcl.scope = Class &&
          dcl.final
          then ( match dcl.entry with
            | Field { init = Some init }
            -> int64_2_int (bs_const_expr cid init)
            | _ -> raise Not_found
          )
          else ( raise Not_found )
      | _ -> raise Not_found
  with _ ->
      Err.intern "get_sysprop"

(* -------------------------------------------------------------------------
ACCESS FUNCTIONS
- for reactive objects / reactive signals
------------------------------------------------------------------------- *)
let rec sig2prcsig xs =
  match xs.trs_principal with
  | Principal xs -> sig2prcsig xs
  | _ -> xs

let sig2id xs = xs.trs_sid
let sig2clck xs = xs.trs_sdecl.sig_clock
let sig2typ xs = xs.trs_styp
let sig2valtyp xs = match xs.trs_styp with
  | Typ(_,[t]) when is_sensor_or_signal_typ xs.trs_styp
  -> t
  | _ -> Err.intern "sig2valtyp"
let sig2sname xs = xs.trs_sname
let sig2pos xs = xs.trs_sdecl.sig_pos
let sig2prcsname xs = (sig2prcsig xs).trs_sname
let sig2ro xs = match xs.trs_ro with
  | Some ro -> ro
  | None -> Err.intern "sig2ro"
let sig2sgn xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2sgn"
  | Some d -> d.sgn
let sig2frq xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2frq"
  | Some d -> d.frq
let sig2mem xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2mem"
  | Some d -> d.mem
let sig2buf xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2buf"
  | Some d -> d.buf
let sig2alpha xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2alpha"
  | Some d -> d.aa
let sig2sgndcl xs = match (sig2prcsig xs).rct_sgndcl with
  | None -> Err.intern"sig2buf"
  | Some d -> d

let ro2name ro = ro.trs_name
let ro2type ro = ro.trs_type
let ro2sigs ro = ro.trs_sig

(* -------------------------------------------------------------------------
TOPOLOGICAL SORT
to use the module in debug mode, i.e. without hiding implementation
details, make the signature to a comment
------------------------------------------------------------------------- *)
module Tsort

(* begin signature
: sig
type ttstate =
| Undefined
| Sorted of int list
| ClosureSorted of int list
| Cycle of int list

type ttsort

val init : int -> ttsort
val statistics : ttsort -> bool -> unit
val size : ttsort -> int
val used : ttsort -> int
val set_pre_post : ttsort -> int -> int -> unit
val set_node : ttsort -> int -> unit
val sort : ttsort -> ttstate
val get_pres : ttsort -> int -> int list
val get_posts : ttsort -> int -> int list
val is_pre_post : ttsort -> int -> int -> bool
val number_of_posts : ttsort -> int -> int
val number_in_posts : ttsort -> int -> int
val collapse : ttsort -> int -> int list -> unit
end
end signature *)

=
struct
  exception ItemFound of int
  
  type ttstate =
    | Undefined
    | Sorted of int list
    | ClosureSorted of int list
    | Cycle of int list
  
  let ttstate2str = function
    | Undefined -> "not sorted"
    | ClosureSorted _ -> "sorted with closure"
    | Sorted _ -> "sorted"
    | Cycle _ -> "has cycle"
  
  type ttsort =
    { mutable used : int;
      mutable ext2int : (int, int) Hashtbl.t;
      mutable rel_a : ttitem array;
      mutable sortp : int;
      mutable sorted : ttitem list;
      mutable state : ttstate;
    }
  
  and ttitem =
    { mutable extern : int;
      mutable is_sorted : bool;
      mutable rel_l : int list;
      mutable rel_c : int list;
    }
  
  let init size =
    let initf _ = { extern = 0; is_sorted = false; rel_l =[]; rel_c =[] } in
    { used = 0;
      ext2int = Hashtbl.create 1979;
      rel_a = Array.init size initf;
      sortp = 0;
      sorted = [];
      state = Undefined;
    }
  
  let resize tsort =
    let oldsize = Array.length tsort.rel_a in
    let newsize = 2 * oldsize in
    let initf c = if c < oldsize
      then tsort.rel_a.(c)
      else { extern = 0; is_sorted = false; rel_l =[]; rel_c =[] } in
    let newrel = Array.init newsize initf in
    tsort.rel_a <- newrel
  
  let fanout tsort =
    let ct = ref 0 in
    for i = 0 to tsort.used - 1 do
      ct := !ct + (List.length tsort.rel_a.(i).rel_l)
    done;
    pf ((float_of_int !ct) /. (float_of_int tsort.used))
  
  let fanout_star tsort =
    let ct = ref 0 in
    for i = 0 to tsort.used - 1 do
      ct := !ct + (List.length tsort.rel_a.(i).rel_c)
    done;
    pf ((float_of_int !ct) /. (float_of_int tsort.used))
  
  let statistics tsort long =
    let pii i j = pi i; ps "->"; pi j; ps " "
    and pis i = pi i; ps " "
    and piInt i = pi (Hashtbl.find tsort.ext2int i.extern); ps " "
    and piExt i = pi i.extern; ps " "
    in
    let pit i it = ps "\n    "; pi i;
      ps (if it.is_sorted then " : " else " ? ");
      List.iter pis it.rel_l
    and pic i it = ps "\n    "; pi i; ps "* : ";
      ps (if it.is_sorted then " :  " else " ?  ");
      List.iter pis it.rel_c
    in
    ps "\nTsort object with:";
    ps "\n  size:    "; pi (Array.length tsort.rel_a);
    ps "\n  used:    "; pi tsort.used;
    if long then (
      ps "\n  ext2int: "; Hashtbl.iter pii tsort.ext2int
    );
    ps "\n  sortp:   "; pi tsort.sortp;
    if long then (
      ps "\n  Isorted: "; List.iter piInt tsort.sorted;
      ps "\n  Esorted: "; List.iter piExt tsort.sorted
    );
    ps "\n  fanout:  "; fanout tsort;
    ps "\n  fanout*: "; fanout_star tsort;
    ps "\n  state:   "; ps (ttstate2str tsort.state);
    if long then (
      ps "\n  rel_a:" ; Array.iteri pit tsort.rel_a
    );
    ps "\n";
    if long then (
      ps "\n  rel_c:" ; Array.iteri pic tsort.rel_a
    );
    ps "\n"
  
  let size tsort = Array.length tsort.rel_a
  let used tsort = tsort.used
  
  let ext2int tsort ext =
    try
        Hashtbl.find tsort.ext2int ext
    with Not_found ->
        let used = tsort.used in
        if used >= Array.length tsort.rel_a
        then resize tsort
        else ();
        tsort.used <- used + 1;
        Hashtbl.add tsort.ext2int ext used;
        (tsort.rel_a.(used)).extern <- ext;
        used
  
  let set_pre_post tsort pre post =
    let pre = ext2int tsort pre
    and post = ext2int tsort post in
    let item = tsort.rel_a.(pre) in
    if List.mem post item.rel_l
    then ()
    else ( item.rel_l <- post:: item.rel_l )
  
  let set_node tsort node =
    let _ = ext2int tsort node in ()
  
  let rec first_unsorted tsort = function
    | [] -> raise Not_found
    | h:: t -> let item = tsort.rel_a.(h) in
        if item.is_sorted
        then first_unsorted tsort t
        else item
  
  let rec cut_tail i = function
    | [] -> Err.intern "Tsort.cut_tail"
    | item:: iteml -> if i = item.extern
        then [item]
        else item :: (cut_tail i iteml)
  
  let rec is_item_in_path i = function
    | [] -> false
    | item:: iteml -> if item.extern = i
        then true
        else is_item_in_path i iteml
  
  let rec find_top tsort path succ =
    try
        let first_item = first_unsorted tsort succ in
        if is_item_in_path first_item.extern path
        then ( let path = cut_tail first_item.extern path in
          let cl = List.map (fun i -> i.extern) path in
          tsort.state <- (Cycle cl);
          raise (Failure "") )
        else ( find_top tsort (first_item:: path) first_item.rel_l )
    with Not_found -> List.hd path
  
  let rec sort tsort i =
    if i >= tsort.used then (
      if tsort.sortp >= tsort.used then (
        tsort.state <- Sorted (List.map (fun i -> i.extern) tsort.sorted)
      ) else (
        sort tsort 0
      )
    ) else (
      let item = tsort.rel_a.(i) in
      if item.is_sorted then (
      (* check next *)
      ) else (
        let top = find_top tsort [item] item.rel_l in
        let sortp = tsort.sortp in
        top.is_sorted <- true;
        tsort.sortp <- sortp + 1;
        tsort.sorted <- top :: tsort.sorted
      );
      sort tsort (i + 1)
    )
  
  let sort tsort =
    if tsort.state = Undefined then (
    (* sorting the first time, not incrementally, no reset *)
    ) else if tsort.used = 0 then (
    (* nothing to sort, no reset *)
    ) else (
      for i = 0 to tsort.used - 1 do
        let item = tsort.rel_a.(i) in
        item.is_sorted <- false;
        item.rel_c <- []
      done );
    ( try
        tsort.sorted <- [];
        tsort.sortp <- 0;  (* nothing is sorted *)
        sort tsort 0
    with _ -> ()
    );
    if debug_level DbgTsort then (
      statistics tsort false
    );
    tsort.state
  
  let rec sorted_merge acc (ll : int list list) =
    match ll with
    | [] -> acc
    | l:: ll -> sorted_merge (join_sosgl l acc) ll
  
  let rec closure tsort = function
    | [] -> ()
    | h:: t -> let itemh = tsort.rel_a.(h) in
        let postl = List.sort compare itemh.rel_l in
        let postc = List.map (fun i -> tsort.rel_a.(i).rel_c) postl in
        itemh.rel_c <- sorted_merge [] (postl:: postc);
        closure tsort t
  
  let ext2existingint tsort ext =
    try
        Hashtbl.find tsort.ext2int ext
    with Not_found ->
        Err.intern ("Tsort.ext2existingint-notfound: "^i2s ext)
  
  let rec rev_ext2existingint tsort assembled = function
    | [] -> assembled
    | h:: t -> let h2i = ext2existingint tsort h in
        rev_ext2existingint tsort (h2i :: assembled) t
  
  let is_pre_post tsort x y =
    ( match tsort.state with
      | Undefined | Cycle _ -> Err.intern "Tsort.is_pre_post"
      | Sorted sl -> closure tsort (rev_ext2existingint tsort [] sl);
          tsort.state <- ClosureSorted sl;
          if debug_level DbgTsort then (
            statistics tsort false
          )
      | ClosureSorted _ -> ()
    );
    let x = ext2existingint tsort x
    and y = ext2existingint tsort y in
    mem_sosgl y tsort.rel_a.(x).rel_c
  
  let get_posts tsort x =
    ( match tsort.state with
      | Undefined | Cycle _ -> Err.intern "Tsort.is_pre_post"
      | Sorted sl -> closure tsort (rev_ext2existingint tsort [] sl);
          tsort.state <- ClosureSorted sl;
          if debug_level DbgTsort then (
            statistics tsort false
          )
      | ClosureSorted _ -> ()
    );
    let x = ext2existingint tsort x in
    List.map (fun i -> (tsort.rel_a.(i)).extern) tsort.rel_a.(x).rel_c
  
  let get_pres tsort x =
    ( match tsort.state with
      | Undefined | Cycle _ -> Err.intern "Tsort.is_pre_post"
      | Sorted sl -> closure tsort (rev_ext2existingint tsort [] sl);
          tsort.state <- ClosureSorted sl;
          if debug_level DbgTsort then (
            statistics tsort false
          )
      
      | ClosureSorted _ -> ()
    );
    let x = ext2existingint tsort x in
    let pl = ref [] in
    for i = 0 to tsort.used - 1 do
      if List.mem x tsort.rel_a.(i).rel_c
      then pl := i:: (!pl)
      else ()
    done;
    List.map (fun i -> (tsort.rel_a.(i)).extern) !pl
  
  let number_of_posts tsort x =
    try
        let x = Hashtbl.find tsort.ext2int x in
        List.length tsort.rel_a.(x).rel_l
    with Not_found ->
        0
  
  let number_in_posts tsort x =
    try
        let x = Hashtbl.find tsort.ext2int x in
        let n = ref 0 in
        for i = 0 to tsort.used - 1 do
          if List.mem x tsort.rel_a.(i).rel_c
          then n := !n + 1
          else ()
        done;
        !n
    with Not_found ->
        0
  
  let rec sorted_unique = function
    | [] -> []
    | [i] -> [i]
    | i:: (j:: l as jl) -> if i = j
        then sorted_unique jl
        else i :: (sorted_unique jl)
  
  let collapse tsort i il =
    (* convert external to internal representation *)
    let i = ext2int tsort i
    and il = List.map (ext2existingint tsort) il in
    (* error checking *)
    if List.mem i il
    then Err.intern "Tsort.collapse:cycle"
    else ();
    (* assemble all post nodes from ''i::il'', make the post list of ''i'' *)
    ( let getrel x =
        let rel = tsort.rel_a.(x).rel_l in
        tsort.rel_a.(x).rel_l <- [];
        rel
      in
      let pl = tsort.rel_a.(i).rel_l :: (List.map getrel il) in
      let pl = sorted_unique (List.sort compare (List.flatten pl)) in
      tsort.rel_a.(i).rel_l <- pl
    );
    (* in all post lists replace all ''il'' occurences by ''i'' *)
    ( let rec il2i = function
        | [] -> []
        | x:: xl -> let x' = if (List.mem x il) then i else x in
            x' :: (il2i xl)
      in
      for i = 0 to tsort.used - 1 do
        let item = tsort.rel_a.(i) in
        let il2i = il2i item.rel_l in
        item.rel_l <- sorted_unique (List.sort compare il2i)
      done
    );
    (* if present remove a self loop in ''i''s (sorted unique!) post list *)
    ( let item = tsort.rel_a.(i) in
      let rec rmi = function
        | [] -> []
        | h:: t as l -> if h > i
            then l
            else if h = i
            then t
            else h :: (rmi t)
      in
      item.rel_l <- (rmi item.rel_l)
    )
  
end

(* -------------------------------------------------------------------------
make system commands target independent
------------------------------------------------------------------------- *)
let file_copy f d =
  match Sys.os_type with
  | "Unix"
  | "Cygwin"
  -> let _ = Sys.command ("cp "^Filename.quote f^" "^Filename.quote d)
      in ()
  | "Win32"
  -> let _ = Sys.command ("copy "^Filename.quote f^" "^Filename.quote d)
      in ()
  | _ -> Err.intern "file_copy"

let file_remove f =
  match Sys.os_type with
  | "Unix"
  | "Cygwin" -> let _ = Sys.command ("rm "^Filename.quote f) in ()
  | "Win32" -> let _ = Sys.command ("del "^Filename.quote f) in ()
  | _ -> Err.intern "file_remove"


