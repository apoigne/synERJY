type teff_or_ifc =
       Eff of string
     | Ifc of string

let pn () = print_newline ()
let pT i  = if (i>0) then for x=1 to i do print_string "  " done else ()
let pnT i = pn(); if (i=0) then () else (pT i)
let ps s  = print_string s
let pi i  = print_int i
let pI () = ps "              "
let cid   = ref (Eff "")
let ext   = ref (Eff "")
let tpl   = ref ""
let dim   = ref ""

let list_of_builtin_classes = ref []

(* ----- copied from util_print.ml ----------------------------------------- *)
let print_comment s =
    let b = "\n(* "
    and e = " *)\n"
    and l = String.length s in
    let r = 61 - l          in
        if r < 0
           then ( ps b; ps s; ps e)
           else ( ps b; ps "---------- "; ps s; ps " ";
                  for i=1 to r do ps "-" done; ps e
                )

(* -------------------------------------------------------------------------
   generate the abstract syntax tree for the builtin classes
   ------------------------------------------------------------------------- *)
let builtin_typ _cid _tpl _ext =
    let plain_cid = match _cid with Eff c -> c | Ifc c -> c in
        list_of_builtin_classes := plain_cid :: !list_of_builtin_classes;
        cid := _cid;
        tpl := _tpl;
        ext := _ext;
        print_comment plain_cid;
        ps "let mk_builtin_class_"; ps plain_cid; ps " () =";
        pnT 0; ps "let id = id_"; ps plain_cid; ps " in";
        pnT 0; ps "let st = Hashtbl.create 23 in"
    
let builtin_std cid =
    builtin_typ (Eff cid) "[]" (Eff "Any")

let mthd name modif formal_l =
    let name = "id_op_"^name in
    pnT 0; ps "let t =";
    pnT 2; ps "{ name="; ps name; ps ";origin=id;entry=mk_mthd ";
           ps formal_l; ps ";";
    pnT 3; ps "final="; ps (match !cid with Eff _ -> "true" | Ifc _ -> "false");
           ps ";scope="; ps (if modif = "C" then "Class;" else "Instance;");
    pnT 3; ps "volatile=false;parameter=false;signature=mk_sig "; ps formal_l;
           ps ";access=pbl";
    pnT 2; ps "} in";
    pnT 5; ps "bs_st_entry st t;"

let cons name formal_l =
    let name = "id_"^name in
    pnT 0; ps "let t =";
    pnT 2; ps "{ name=Ly.c2mf "; ps name; ps ";origin=id;entry=mk_constr ";
           ps formal_l; ps ";";
    pnT 3; ps "final=true;scope=Instance;volatile=false;parameter=false;";
    pnT 3; ps "signature=mk_sig "; ps formal_l;
           ps ";access=pbl";
    pnT 2; ps "} in";
    pnT 5; ps "bs_st_entry st t;"

let builtin_end () =
    let (effective,cid) = match !cid with Eff c -> (true,c) | Ifc c -> (false,c)
    in
    pnT 0; ps "let bi =";
    pnT 2; ps "{ classkind=";
           ps (if effective then "EffectiveClass" else "Interface");
           ps ";classid=id"; ps ";typparams="; ps !tpl; ps ";";
    pnT 3; ps "src_file=\"\";src_pos=Ly.nolbl;";
    pnT 3; ( match !ext with
           | Eff super -> ps "extends="; ps super; ps ";implements=[];"
           | Ifc super -> ps "extends=t_object;implements=["; ps super; ps "];"
           );
    pnT 3; ps "specs=[];axioms=[];props=[];symtab=st;declseq=[];\
               lbl2sp=Ly.lbl2sp_empty;";
    pnT 3; ps "class_sta={class_status=NoUpdate;";ps "methods_called=[];";
    pnT 5; ps "classes_created=[];classes_dyn_created=[];";
    pnT 5; ps "class_methods_called=[];array_lits=[];";
    pnT 5; ps "class_behavior=None;typ_constrl=[]};";
    pnT 5; ps "status=NotTouched;";
    pnT 2; ps "} in";
    pnT 5; ps "Hashtbl.add se.classtab id_"; ps cid; ps " bi;";
    pnT 5; ps "Inherit_check.mk_forced id_"; ps cid; ps ";;\n"

let c2prim () =
    mthd "to_char"   "i"  "[t_char]";
    mthd "to_byte"   "i"  "[t_byte]";
    mthd "to_short"  "i"  "[t_short]";
    mthd "to_int16"  "i"  "[t_short]";
    mthd "to_uint16" "i"  "[t_uint16]";
    mthd "to_int"    "i"  "[t_int]";
    mthd "to_int32"  "i"  "[t_int]";
    mthd "to_uint32" "i"  "[t_uint32]";
    mthd "to_long"   "i"  "[t_long]";
    mthd "to_int64"  "i"  "[t_long]";
    mthd "to_uint64" "i"  "[t_uint64]"

let prim2prim () =
    mthd "to_bool"   "i"  "[t_bool]";
    c2prim ();
    mthd "to_double" "i"  "[t_double]";
    mthd "to_float"  "i"  "[t_float]";
    mthd "to_string" "i"  "[t_string]";
    mthd "to_time"   "i"  "[t_time]"

let comp_arith from =
    mthd "lt"           "i" ("[t_bool;t_"^from^"]");
    mthd "le"           "i" ("[t_bool;t_"^from^"]");
    mthd "gt"           "i" ("[t_bool;t_"^from^"]");
    mthd "ge"           "i" ("[t_bool;t_"^from^"]");

    mthd "plus"         "i" ("[t_"^from^"]");
    mthd "minus"        "i" ("[t_"^from^"]");

    mthd "add"          "i" ("[t_"^from^";t_"^from^"]");
    mthd "sub"          "i" ("[t_"^from^";t_"^from^"]");
    mthd "mult"         "i" ("[t_"^from^";t_"^from^"]");
    mthd "div"          "i" ("[t_"^from^";t_"^from^"]")

let integral_mthd from =
    comp_arith from;

    mthd "mod"          "i" ("[t_"^from^";t_"^from^"]");

    mthd "prefix_incr"  "i" ("[t_"^from^"]");
    mthd "prefix_decr"  "i" ("[t_"^from^"]");
    mthd "postfix_incr" "i" ("[t_"^from^"]");
    mthd "postfix_decr" "i" ("[t_"^from^"]");

    mthd "bit_and"      "i" ("[t_"^from^";t_"^from^"]");
    mthd "bit_or"       "i" ("[t_"^from^";t_"^from^"]");
    mthd "xor"          "i" ("[t_"^from^";t_"^from^"]");
    mthd "log_and"      "i" ("[t_"^from^";t_"^from^"]");
    mthd "log_or"       "i" ("[t_"^from^";t_"^from^"]");

    mthd "complement"   "i" ("[t_"^from^"]");

    mthd "leftshift"    "i" ("[t_"^from^";t_"^from^"]");
    mthd "rightshift"   "i" ("[t_"^from^";t_"^from^"]");
    mthd "rightshift0"  "i" ("[t_"^from^";t_"^from^"]")

(* -------------------------------------------------------------------------
   INITIALIZATION
   ------------------------------------------------------------------------- *)
let runit () = 
    ps "open Ly\nopen Ast\nopen P\nopen Util_parse\n\n";
    ps "let pbl = Public [id_object]\n\n";
    ps "let mk_fpl n t = {p_name=n;p_type=t;p_lbl=Ly.nolbl;p_clock=v_true}\n";
    ps "let mk_fpl fpl =\n";
    ps "    match fpl with\n";
    ps "    | [_]       -> []\n";
    ps "    | [_;t]     -> [mk_fpl id_other t]\n";
    ps "    | [_;t;u]   -> [mk_fpl id_p1 t;mk_fpl id_p2 u]\n";
    ps "    | [_;t;u;v] -> [mk_fpl id_p1 t;mk_fpl id_p2 u;mk_fpl id_p2 v]\n";
    ps "    | _ -> raise (Error \"Internal Error builtin\")\n\n";
    ps "let mk_sig = function\n";
    ps "    | []   -> raise (Error \"Internal Error of mk_sig\")\n";
    ps "    | r::p -> {rt=r;pt=Some p}\n\n";
    ps "let mk_mthd fpl =\n";
    ps "    Method { mpre=[];mpost=[];mformals=mk_fpl fpl;mbody=Nobody;\n";
    ps "             method_kind=Internal;msrcp=Ly.nolbl;\n";
    ps "             mread=[];mwrite=[];mcall=[] }\n\n";
    ps "let mk_constr fpl =\n";
    ps "    Constructor { cformals=mk_fpl fpl;cbody=[];csig=[];cro=[];\n";
    ps "                  cactive=None;csrcp=Ly.nolbl;\n";
    ps "                  cread=[];cwrite=[];ccall=[] } ;;\n";

(* -------------------------------------------------------------------------
   OBJECT
   ------------------------------------------------------------------------- *)
builtin_typ (Eff "object") "[]" (Eff "Any");
builtin_end ();

(* -------------------------------------------------------------------------
   BOOLEAN
   ------------------------------------------------------------------------- *)
builtin_typ (Eff "bool") "[]" (Eff "Any");
mthd "log_and"      "i" "[t_bool;t_bool]";
mthd "log_or"       "i" "[t_bool;t_bool]";
mthd "not"          "i" "[t_bool]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Char Uint8
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "char") "[]" (Ifc "");
integral_mthd "char";

mthd "is_digit"     "i" "[t_bool]";
mthd "is_lower"     "i" "[t_bool]";
mthd "is_upper"     "i" "[t_bool]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Byte Int8
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "byte") "[]" (Ifc "");
integral_mthd "byte";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Short Int16
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "short") "[]" (Ifc "");
integral_mthd "short";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Uint16
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "uint16") "[]" (Ifc "");
integral_mthd "uint16";

mthd "high"         "i" "[t_char]";
mthd "low"          "i" "[t_char]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Int Int32
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "int") "[]" (Ifc "");
integral_mthd "int";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Uint32
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "uint32") "[]" (Ifc "");
integral_mthd "uint32";

mthd "high"         "i" "[t_uint16]";
mthd "low"          "i" "[t_uint16]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Long Int64
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "long") "[]" (Ifc "");
integral_mthd "long";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Uint64
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "uint64") "[]" (Ifc "");
integral_mthd "uint64";

mthd "high"         "i" "[t_uint32]";
mthd "low"          "i" "[t_uint32]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Float
   ------------------------------------------------------------------------- *)
builtin_std "float";
comp_arith "float";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   Double
   ------------------------------------------------------------------------- *)
builtin_std "double";
comp_arith "double";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   String
   ------------------------------------------------------------------------- *)
builtin_std "string";
mthd "lt"           "i" "[t_bool;t_string]";
mthd "le"           "i" "[t_bool;t_string]";
mthd "gt"           "i" "[t_bool;t_string]";
mthd "ge"           "i" "[t_bool;t_string]";
mthd "length"       "i" "[t_int]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   ARRAY

   The Id's "ic_row" and "id_col" are used as parameters for dimensions.
   This is used in typecheck.ml to pass arguments. Do not change without
   changing the corresponding clauses in the function "tc_subst_typ_var"
   ------------------------------------------------------------------------- *)
builtin_typ (Eff "array1") "[(id__T,None)]" (Eff "t_object");
cons "array1"           "[Null;t_int]";
mthd "length"       "i" "[t_int]";
builtin_end ();

builtin_typ (Eff "vector") "[(id__T,None)]" 
                        (Eff "Array(TypeVar(id__T),DimLen 1,Arbitrary)"); 
cons "vector"           "[Null;t_int]";
mthd "transp"       "i" "[Array(TypeVar(id__T),DimVar id_col,DimLen 1)]";
builtin_end ();

builtin_typ (Eff "array2") "[(id__T,None)]" (Eff "t_object");
cons "array2"           "[Null;t_int;t_int]";
mthd "cols"         "i" "[t_int]";
mthd "rows"         "i" "[t_int]";
builtin_end ();

builtin_typ (Eff "matrix") "[(id__T,None)]" 
                        (Eff "Array(TypeVar(id__T),Arbitrary,Arbitrary)");
cons "matrix"           "[Null;t_int;t_int]";
mthd "transp"       "i" "[Array(TypeVar(id__T),DimVar id_col,DimVar id_row)]";
builtin_end ();

(* -------------------------------------------------------------------------
   SIGNAL RELATED STUFF
   ------------------------------------------------------------------------- *)
builtin_typ (Eff "flow") "[(id__T,None)]" (Eff "Any");
builtin_end ();

builtin_typ (Eff "sensor") "[(id__T,None)]"
            (Eff "Any");
cons "sensor"     "[Null;Typ(id_input,[])]";
builtin_end ();

builtin_typ (Eff "signal") "[(id__T,None)]"
            (Eff "Typ(id_sensor,[TypeVar id__T])");
cons "signal" "[Null]";
cons "signal" "[Null;Typ(id_output,[])]";
builtin_end ();

builtin_typ (Eff "delayed") "[(id__T,None)]"
            (Eff "Typ(id_sensor,[TypeVar id__T])");
cons "delayed" "[Null]";
cons "delayed" "[Null;Typ(id_output,[])]";
builtin_end ();

(* -------------------------------------------------------------------------
   INPUT OUTPUT RELATED STUFF (callback classes)
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "input") "[]" (Eff "t_object");
builtin_end ();

builtin_typ (Ifc "output") "[]" (Eff "t_object");
builtin_end ();

builtin_typ (Eff "sim_input") "[]" (Ifc "Typ(id_input,[])");
cons "sim_input"    "[Null]";
builtin_end ();

builtin_typ (Eff "sim_output") "[]" (Ifc "Typ(id_output,[])");
cons "sim_output"   "[Null]";
builtin_end ();

(* -------------------------------------------------------------------------
   TIME
   ------------------------------------------------------------------------- *)
builtin_std "time";
mthd "lt"           "i" "[t_bool;t_time]";
mthd "le"           "i" "[t_bool;t_time]";
mthd "gt"           "i" "[t_bool;t_time]";
mthd "ge"           "i" "[t_bool;t_time]";
mthd "add"          "i" "[t_time;t_time]";
mthd "sub"          "i" "[t_time;t_time]";
mthd "zero"         "C" "[t_time]";
mthd "usec"         "C" "[t_time;t_int]";
mthd "msec"         "C" "[t_time;t_int]";
mthd "sec"          "C" "[t_time;t_int]";
mthd "min"          "C" "[t_time;t_int]";
mthd "hour"         "C" "[t_time;t_int]";

prim2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   C*INT* types for manifest constants
   e.g. cuint7 0..127
        cuint8 0..255
        cint8  -128..127
   ------------------------------------------------------------------------- *)
builtin_typ (Ifc "cuint64")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cint64")   "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint63")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint32")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cint32")   "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint31")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint16")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cint16")   "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint15")  "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint8")   "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cint8")    "[]" (Ifc "");
c2prim ();
builtin_end ();

builtin_typ (Ifc "cuint7")   "[]" (Ifc "");
c2prim ();
builtin_end ();

(* -------------------------------------------------------------------------
   TERMINATION
   ------------------------------------------------------------------------- *)
    print_comment "init_builtin";
    ps "let init_builtin () =\n";
    ps "Hashtbl.clear se.classtab;\n";
    List.iter (fun c -> ps "mk_builtin_class_"; ps c; ps " ();\n")
              (List.rev !list_of_builtin_classes);
    ps "();;\n"; flush stdout

;; Printexc.catch runit ();;
