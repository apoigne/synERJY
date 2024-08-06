type tsc_id = Ly.tsc_id

val reset_sgn_counters : unit -> unit
val tick   : tsc_id
val alpha  : tsc_id
val beta   : tsc_id
val tau    : tsc_id

val tick'  : tsc_id
val alpha' : tsc_id
val beta'  : tsc_id
val tau'   : tsc_id
val mu'    : tsc_id

val next_inp   : unit -> tsc_id
val next_ext   : unit -> tsc_id
val next_lcl   : unit -> tsc_id
val next_var   : unit -> tsc_id
val next_aux   : unit -> tsc_id
val next_mem   : unit -> tsc_id
val next_pre   : unit -> tsc_id
val next_jump  : unit -> tsc_id
val next_reg   : unit -> tsc_id
val next_gamma : unit -> tsc_id
val next_beta  : unit -> tsc_id
val next_tau   : unit -> tsc_id
val next_frq   : unit -> tsc_id
val next_eta   : unit -> tsc_id
val next_delta : unit -> tsc_id
val next_cond  : unit -> tsc_id
val next_act   : unit -> tsc_id
val next_dbg   : unit -> tsc_id

val is_inp   : tsc_id -> bool
val is_ext   : tsc_id -> bool
val is_lcl   : tsc_id -> bool
val is_var   : tsc_id -> bool
val is_aux   : tsc_id -> bool
val is_mem   : tsc_id -> bool
val is_pre   : tsc_id -> bool
val is_jump  : tsc_id -> bool
val is_reg   : tsc_id -> bool
val is_gamma : tsc_id -> bool
val is_beta  : tsc_id -> bool
val is_tau   : tsc_id -> bool
val is_frq   : tsc_id -> bool
val is_eta   : tsc_id -> bool
val is_delta : tsc_id -> bool
val is_cond : tsc_id -> bool
val is_act   : tsc_id -> bool
val is_dbg   : tsc_id -> bool

val next_sgn   : tsc_id -> tsc_id
val sgn2str    : tsc_id -> string
val sgn2valstr : tsc_id -> string

(* bijective coding of signals <-> values *)
val sgn2val      : tsc_id -> tsc_id
val is_val       : tsc_id -> bool
val is_inpval    : tsc_id -> bool
val is_extval    : tsc_id -> bool
val is_lclval    : tsc_id -> bool
val sgnorval2sgn : tsc_id -> tsc_id

val is_reg_or_mem_or_pre : tsc_id -> bool

