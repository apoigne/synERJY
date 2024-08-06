exception Error of string
exception Parse of string

type tclass
and  tmfid
and  tlbl
and  tsc_id   = int
and  tsrc_pos =
     | NoSrcPos
     | TextPos    of string * int * int * int * int * int
     | HaltPos    of string * int * int * int * int * int
     | EmitPos    of string * int * int * int * int * int
     | GraphicPos of string * int

type tlbl2sp

val c2id : string -> tclass
val mf2id : string -> tmfid
val c2mf : tclass -> tmfid
val mf2c : tmfid -> tclass
val c2str : tclass -> string
val mf2str : tmfid -> string
val c2int : tclass -> int
val int2c : int -> tclass
val mf2int : tmfid -> int
val int2mf : int -> tmfid

val lbl2mf  : tlbl -> tmfid
val mf2lbl  : tmfid -> tlbl
val is_int_lbl : tlbl -> bool
val ext_lbl : tlbl -> tsrc_pos -> unit
val int_lbl : tsrc_pos -> tlbl
val lbl2str : tlbl -> string
val is_specified_label : tlbl -> bool
val int_lbl2str : tlbl -> string
val nolbl : tlbl
val lbl2srcp : tlbl -> tlbl2sp -> tsrc_pos
val lbl2srcp_iter : tlbl2sp -> (tlbl -> tsrc_pos -> unit) -> unit
val clr_and_return_lbls : unit -> tlbl2sp
val lbl2srcp_text2emit : tlbl -> tlbl2sp -> tlbl2sp
val lbl2sp_empty : tlbl2sp
val pos_1line : Lexing.lexbuf -> unit
val pos2lc : int -> int * int
val clr_pos2lc : unit -> unit
val push_pos2lc : unit -> unit
val pop_pos2lc : unit -> unit
