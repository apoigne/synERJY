open Ly
open Ast
open P
open Util_parse

let pbl = Public [id_object]

let mk_fpl n t = {p_name=n;p_type=t;p_lbl=Ly.nolbl;p_clock=v_true}
let mk_fpl fpl =
    match fpl with
    | [_]       -> []
    | [_;t]     -> [mk_fpl id_other t]
    | [_;t;u]   -> [mk_fpl id_p1 t;mk_fpl id_p2 u]
    | [_;t;u;v] -> [mk_fpl id_p1 t;mk_fpl id_p2 u;mk_fpl id_p2 v]
    | _ -> raise (Error "Internal Error builtin")

let mk_sig = function
    | []   -> raise (Error "Internal Error of mk_sig")
    | r::p -> {rt=r;pt=Some p}

let mk_mthd fpl =
    Method { mpre=[];mpost=[];mformals=mk_fpl fpl;mbody=Nobody;
             method_kind=Internal;msrcp=Ly.nolbl;
             mread=[];mwrite=[];mcall=[] }

let mk_constr fpl =
    Constructor { cformals=mk_fpl fpl;cbody=[];csig=[];cro=[];
                  cactive=None;csrcp=Ly.nolbl;
                  cread=[];cwrite=[];ccall=[] } ;;

(* ---------- object ------------------------------------------------------- *)
let mk_builtin_class_object () =
let id = id_object in
let st = Hashtbl.create 23 in
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_object bi;
          Inherit_check.mk_forced id_object;;

(* ---------- bool --------------------------------------------------------- *)
let mk_builtin_class_bool () =
let id = id_bool in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_bool;t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_bool;t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_not;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_bool bi;
          Inherit_check.mk_forced id_bool;;

(* ---------- char --------------------------------------------------------- *)
let mk_builtin_class_char () =
let id = id_char in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_char;t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char;t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_is_digit;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_is_lower;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_is_upper;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_char bi;
          Inherit_check.mk_forced id_char;;

(* ---------- byte --------------------------------------------------------- *)
let mk_builtin_class_byte () =
let id = id_byte in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_byte;t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte;t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_byte bi;
          Inherit_check.mk_forced id_byte;;

(* ---------- short -------------------------------------------------------- *)
let mk_builtin_class_short () =
let id = id_short in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_short;t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short;t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_short bi;
          Inherit_check.mk_forced id_short;;

(* ---------- uint16 ------------------------------------------------------- *)
let mk_builtin_class_uint16 () =
let id = id_uint16 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_uint16;t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16;t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_high;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_low;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_uint16 bi;
          Inherit_check.mk_forced id_uint16;;

(* ---------- int ---------------------------------------------------------- *)
let mk_builtin_class_int () =
let id = id_int in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_int;t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_int bi;
          Inherit_check.mk_forced id_int;;

(* ---------- uint32 ------------------------------------------------------- *)
let mk_builtin_class_uint32 () =
let id = id_uint32 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_uint32;t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32;t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_high;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_low;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_uint32 bi;
          Inherit_check.mk_forced id_uint32;;

(* ---------- long --------------------------------------------------------- *)
let mk_builtin_class_long () =
let id = id_long in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_long;t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long;t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_long bi;
          Inherit_check.mk_forced id_long;;

(* ---------- uint64 ------------------------------------------------------- *)
let mk_builtin_class_uint64 () =
let id = id_uint64 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mod;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_incr;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_prefix_decr;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_incr;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_postfix_decr;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_and;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_bit_or;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_xor;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_and;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_log_or;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_complement;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_leftshift;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rightshift0;origin=id;entry=mk_mthd [t_uint64;t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64;t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_high;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_low;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_uint64 bi;
          Inherit_check.mk_forced id_uint64;;

(* ---------- float -------------------------------------------------------- *)
let mk_builtin_class_float () =
let id = id_float in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_float;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_float;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_float;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_float;t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float;t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_float bi;
          Inherit_check.mk_forced id_float;;

(* ---------- double ------------------------------------------------------- *)
let mk_builtin_class_double () =
let id = id_double in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_plus;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_minus;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_double;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_double;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_mult;origin=id;entry=mk_mthd [t_double;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_div;origin=id;entry=mk_mthd [t_double;t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double;t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_double bi;
          Inherit_check.mk_forced id_double;;

(* ---------- string ------------------------------------------------------- *)
let mk_builtin_class_string () =
let id = id_string in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_length;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_string bi;
          Inherit_check.mk_forced id_string;;

(* ---------- array1 ------------------------------------------------------- *)
let mk_builtin_class_array1 () =
let id = id_array1 in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_array1;origin=id;entry=mk_constr [Null;t_int];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_length;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_array1 bi;
          Inherit_check.mk_forced id_array1;;

(* ---------- vector ------------------------------------------------------- *)
let mk_builtin_class_vector () =
let id = id_vector in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_vector;origin=id;entry=mk_constr [Null;t_int];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_transp;origin=id;entry=mk_mthd [Array(TypeVar(id__T),DimVar id_col,DimLen 1)];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [Array(TypeVar(id__T),DimVar id_col,DimLen 1)];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Array(TypeVar(id__T),DimLen 1,Arbitrary);implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_vector bi;
          Inherit_check.mk_forced id_vector;;

(* ---------- array2 ------------------------------------------------------- *)
let mk_builtin_class_array2 () =
let id = id_array2 in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_array2;origin=id;entry=mk_constr [Null;t_int;t_int];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_cols;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_rows;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_array2 bi;
          Inherit_check.mk_forced id_array2;;

(* ---------- matrix ------------------------------------------------------- *)
let mk_builtin_class_matrix () =
let id = id_matrix in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_matrix;origin=id;entry=mk_constr [Null;t_int;t_int];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;t_int;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_transp;origin=id;entry=mk_mthd [Array(TypeVar(id__T),DimVar id_col,DimVar id_row)];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [Array(TypeVar(id__T),DimVar id_col,DimVar id_row)];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Array(TypeVar(id__T),Arbitrary,Arbitrary);implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_matrix bi;
          Inherit_check.mk_forced id_matrix;;

(* ---------- flow --------------------------------------------------------- *)
let mk_builtin_class_flow () =
let id = id_flow in
let st = Hashtbl.create 23 in
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_flow bi;
          Inherit_check.mk_forced id_flow;;

(* ---------- sensor ------------------------------------------------------- *)
let mk_builtin_class_sensor () =
let id = id_sensor in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_sensor;origin=id;entry=mk_constr [Null;Typ(id_input,[])];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;Typ(id_input,[])];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_sensor bi;
          Inherit_check.mk_forced id_sensor;;

(* ---------- signal ------------------------------------------------------- *)
let mk_builtin_class_signal () =
let id = id_signal in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_signal;origin=id;entry=mk_constr [Null];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=Ly.c2mf id_signal;origin=id;entry=mk_constr [Null;Typ(id_output,[])];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;Typ(id_output,[])];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Typ(id_sensor,[TypeVar id__T]);implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_signal bi;
          Inherit_check.mk_forced id_signal;;

(* ---------- delayed ------------------------------------------------------ *)
let mk_builtin_class_delayed () =
let id = id_delayed in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_delayed;origin=id;entry=mk_constr [Null];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=Ly.c2mf id_delayed;origin=id;entry=mk_constr [Null;Typ(id_output,[])];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null;Typ(id_output,[])];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[(id__T,None)];
      src_file="";src_pos=Ly.nolbl;
      extends=Typ(id_sensor,[TypeVar id__T]);implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_delayed bi;
          Inherit_check.mk_forced id_delayed;;

(* ---------- input -------------------------------------------------------- *)
let mk_builtin_class_input () =
let id = id_input in
let st = Hashtbl.create 23 in
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_input bi;
          Inherit_check.mk_forced id_input;;

(* ---------- output ------------------------------------------------------- *)
let mk_builtin_class_output () =
let id = id_output in
let st = Hashtbl.create 23 in
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_output bi;
          Inherit_check.mk_forced id_output;;

(* ---------- sim_input ---------------------------------------------------- *)
let mk_builtin_class_sim_input () =
let id = id_sim_input in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_sim_input;origin=id;entry=mk_constr [Null];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[Typ(id_input,[])];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_sim_input bi;
          Inherit_check.mk_forced id_sim_input;;

(* ---------- sim_output --------------------------------------------------- *)
let mk_builtin_class_sim_output () =
let id = id_sim_output in
let st = Hashtbl.create 23 in
let t =
    { name=Ly.c2mf id_sim_output;origin=id;entry=mk_constr [Null];
      final=true;scope=Instance;volatile=false;parameter=false;
      signature=mk_sig [Null];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[Typ(id_output,[])];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_sim_output bi;
          Inherit_check.mk_forced id_sim_output;;

(* ---------- time --------------------------------------------------------- *)
let mk_builtin_class_time () =
let id = id_time in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_lt;origin=id;entry=mk_mthd [t_bool;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_le;origin=id;entry=mk_mthd [t_bool;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_gt;origin=id;entry=mk_mthd [t_bool;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_ge;origin=id;entry=mk_mthd [t_bool;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_add;origin=id;entry=mk_mthd [t_time;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sub;origin=id;entry=mk_mthd [t_time;t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_zero;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_usec;origin=id;entry=mk_mthd [t_time;t_int];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_msec;origin=id;entry=mk_mthd [t_time;t_int];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_sec;origin=id;entry=mk_mthd [t_time;t_int];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_min;origin=id;entry=mk_mthd [t_time;t_int];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_hour;origin=id;entry=mk_mthd [t_time;t_int];
      final=true;scope=Class;
      volatile=false;parameter=false;signature=mk_sig [t_time;t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_bool;origin=id;entry=mk_mthd [t_bool];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_bool];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_double;origin=id;entry=mk_mthd [t_double];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_double];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_float;origin=id;entry=mk_mthd [t_float];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_float];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_string;origin=id;entry=mk_mthd [t_string];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_string];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_time;origin=id;entry=mk_mthd [t_time];
      final=true;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_time];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=EffectiveClass;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=Any;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_time bi;
          Inherit_check.mk_forced id_time;;

(* ---------- cuint64 ------------------------------------------------------ *)
let mk_builtin_class_cuint64 () =
let id = id_cuint64 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint64 bi;
          Inherit_check.mk_forced id_cuint64;;

(* ---------- cint64 ------------------------------------------------------- *)
let mk_builtin_class_cint64 () =
let id = id_cint64 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cint64 bi;
          Inherit_check.mk_forced id_cint64;;

(* ---------- cuint63 ------------------------------------------------------ *)
let mk_builtin_class_cuint63 () =
let id = id_cuint63 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint63 bi;
          Inherit_check.mk_forced id_cuint63;;

(* ---------- cuint32 ------------------------------------------------------ *)
let mk_builtin_class_cuint32 () =
let id = id_cuint32 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint32 bi;
          Inherit_check.mk_forced id_cuint32;;

(* ---------- cint32 ------------------------------------------------------- *)
let mk_builtin_class_cint32 () =
let id = id_cint32 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cint32 bi;
          Inherit_check.mk_forced id_cint32;;

(* ---------- cuint31 ------------------------------------------------------ *)
let mk_builtin_class_cuint31 () =
let id = id_cuint31 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint31 bi;
          Inherit_check.mk_forced id_cuint31;;

(* ---------- cuint16 ------------------------------------------------------ *)
let mk_builtin_class_cuint16 () =
let id = id_cuint16 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint16 bi;
          Inherit_check.mk_forced id_cuint16;;

(* ---------- cint16 ------------------------------------------------------- *)
let mk_builtin_class_cint16 () =
let id = id_cint16 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cint16 bi;
          Inherit_check.mk_forced id_cint16;;

(* ---------- cuint15 ------------------------------------------------------ *)
let mk_builtin_class_cuint15 () =
let id = id_cuint15 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint15 bi;
          Inherit_check.mk_forced id_cuint15;;

(* ---------- cuint8 ------------------------------------------------------- *)
let mk_builtin_class_cuint8 () =
let id = id_cuint8 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint8 bi;
          Inherit_check.mk_forced id_cuint8;;

(* ---------- cint8 -------------------------------------------------------- *)
let mk_builtin_class_cint8 () =
let id = id_cint8 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cint8 bi;
          Inherit_check.mk_forced id_cint8;;

(* ---------- cuint7 ------------------------------------------------------- *)
let mk_builtin_class_cuint7 () =
let id = id_cuint7 in
let st = Hashtbl.create 23 in
let t =
    { name=id_op_to_char;origin=id;entry=mk_mthd [t_char];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_char];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_byte;origin=id;entry=mk_mthd [t_byte];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_byte];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_short;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int16;origin=id;entry=mk_mthd [t_short];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_short];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint16;origin=id;entry=mk_mthd [t_uint16];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint16];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int32;origin=id;entry=mk_mthd [t_int];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_int];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint32;origin=id;entry=mk_mthd [t_uint32];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint32];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_long;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_int64;origin=id;entry=mk_mthd [t_long];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_long];access=pbl
    } in
          bs_st_entry st t;
let t =
    { name=id_op_to_uint64;origin=id;entry=mk_mthd [t_uint64];
      final=false;scope=Instance;
      volatile=false;parameter=false;signature=mk_sig [t_uint64];access=pbl
    } in
          bs_st_entry st t;
let bi =
    { classkind=Interface;classid=id;typparams=[];
      src_file="";src_pos=Ly.nolbl;
      extends=t_object;implements=[];
      specs=[];axioms=[];props=[];symtab=st;declseq=[];lbl2sp=Ly.lbl2sp_empty;
      class_sta={class_status=NoUpdate;methods_called=[];
          classes_created=[];classes_dyn_created=[];
          class_methods_called=[];array_lits=[];
          class_behavior=None;typ_constrl=[]};
          status=NotTouched;
    } in
          Hashtbl.add se.classtab id_cuint7 bi;
          Inherit_check.mk_forced id_cuint7;;

(* ---------- init_builtin ------------------------------------------------- *)
let init_builtin () =
Hashtbl.clear se.classtab;
mk_builtin_class_object ();
mk_builtin_class_bool ();
mk_builtin_class_char ();
mk_builtin_class_byte ();
mk_builtin_class_short ();
mk_builtin_class_uint16 ();
mk_builtin_class_int ();
mk_builtin_class_uint32 ();
mk_builtin_class_long ();
mk_builtin_class_uint64 ();
mk_builtin_class_float ();
mk_builtin_class_double ();
mk_builtin_class_string ();
mk_builtin_class_array1 ();
mk_builtin_class_vector ();
mk_builtin_class_array2 ();
mk_builtin_class_matrix ();
mk_builtin_class_flow ();
mk_builtin_class_sensor ();
mk_builtin_class_signal ();
mk_builtin_class_delayed ();
mk_builtin_class_input ();
mk_builtin_class_output ();
mk_builtin_class_sim_input ();
mk_builtin_class_sim_output ();
mk_builtin_class_time ();
mk_builtin_class_cuint64 ();
mk_builtin_class_cint64 ();
mk_builtin_class_cuint63 ();
mk_builtin_class_cuint32 ();
mk_builtin_class_cint32 ();
mk_builtin_class_cuint31 ();
mk_builtin_class_cuint16 ();
mk_builtin_class_cint16 ();
mk_builtin_class_cuint15 ();
mk_builtin_class_cuint8 ();
mk_builtin_class_cint8 ();
mk_builtin_class_cuint7 ();
();;
