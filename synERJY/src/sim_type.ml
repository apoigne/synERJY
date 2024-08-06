(* sim_type.ml
*
* prefixes / suffixes:
* < name > does not contain underscores
* name of type < typename > : t < name >
* name of a list type < typename > : t < name > _list
* name of constructor < constructor > : S < name >
* convert to string fun : string_of_ < typename >
* printer functions : fmt_ < typename >
*)

open Ast
open Sim_error

let string_i = "invalid"
let int64_i = Int64.zero
let int32_i = Int32.zero
let int_i = 0
let bool_i = false

(* ------------------------------------------------------------------------ *)
type sim_exit_t =
	| SAbort of string
	| SEnd
let sim_exit_i = SEnd

type category_t =
	| SPrimitive
	| SClassType
let category_i = SPrimitive

type sigflow_t =
	| SInput
	| SOutput
	| SLocal
let sigflow_i = SLocal

type memrep_t =
	| SInPlace of int32
	| SByReference of int32
let memrep_i = SInPlace int32_i

type idspec_t = { idname : string;
	idtype : string;
	iddim : int list }
let idspec_i = { idname = string_i; idtype = string_i; iddim = [] }

type memberspec_t = { spec : idspec_t; rep : memrep_t }
let memberspec_i = { spec = idspec_i; rep = memrep_i }

type signalspec_t = { signame : string;
	sigflow : sigflow_t; (* flow *)
	sigvalt : idspec_t } (* args *)
let signalspec_i = { signame = string_i;
	sigflow = sigflow_i;
	sigvalt = idspec_i }

type fieldspec_t =
	| SSignal of signalspec_t
	| SMember of memberspec_t
let fieldspec_i = SSignal signalspec_i

(* trace *)
type traceinfo_t = { sim_version : string;
	nb_instants : int }
let traceinfo_i = { sim_version = string_i;
	nb_instants = int_i }

type traceheader_t = { traceinfo : traceinfo_t option;
	principal_signals : signalspec_t list }
let traceheader_i = { traceinfo = None;
	principal_signals = [] }

(* target *)
type textsel_t = { tselfilex: int; tselgo: int;
	originline: int; origincol: int;
	cornerline: int; cornercol: int }
let textsel_i = { tselfilex = int_i; tselgo = int_i;
	originline = int_i; origincol = int_i;
	cornerline = int_i; cornercol = int_i }

type grafsel_t = { gselfilex: int; gselgo: int }
let grafsel_i = { gselfilex = int_i; gselgo = int_i }

type tagspec_t =
	| SMTag of int * int
	(* taglabel,label of called method *)
	| SGTag of int * int
(* taglabel,label of object *)
type labelspec_t =
	| LblHaltSel of textsel_t
	| LblEmitSel of textsel_t
	| LblTextSel of textsel_t
	| LblGO of grafsel_t

type simclass_t =
	{ classnm : string;
		classtext : textsel_t;
		classgraficfiles : int list;
		signals : signalspec_t list;
		members : memberspec_t list;
		constr_label : int option;
		tags : tagspec_t list;
		labels : (int * labelspec_t) list }
let simclass_i =
	{ classnm = string_i;
		classtext = textsel_i;
		classgraficfiles = [];
		signals = [];
		members = [];
		constr_label = None;  (* class is not reactive *)
		tags = [];
		labels = [] }

type simroot_t = { root_obj : int32; root_class : string }
let simroot_i = { root_obj = int32_i; root_class = string_i }

type filespec_t = int * string
let filespec_i = (int_i, string_i)

type robjectspec_t = { robjectindex : int; robjectname : string }
let robjectspec_i = { robjectindex = int_i; robjectname = string_i }

type targetheader_t = { target_signals : signalspec_t list;
	target_classes : simclass_t list;
	target_timing : int64;
	target_files : filespec_t list;
	target_config : simroot_t }
let targetheader_i = { target_signals = [];
	target_classes = [];
	target_timing = int64_i;
	target_files = [];
	target_config = simroot_i }

(* ------------------------------------------------------------------------ *)
type simval_t =
	| SNull
	| SLiteral of tliteral
	| SObject of (string * tliteral) list
	| SVector of tliteral list
	| SMatrix of (tliteral list) list

let simval_i = SNull

(* ------------------------------------------------------------------------ *)
type emit_t = { emitname : string;
	emitarg : simval_t;
	mutable emitflow : sigflow_t option }

let emit_i = { emitname = string_i;
	emitarg = SNull;
	emitflow = None }

type dbg_t = { sim_obj : string ; sim_lbl : int list }
let dbg_i = { sim_obj = ""; sim_lbl = [] }

type simoutmsg_t =
	| Sim_Emit of emit_t
	| Sim_Exc of tliteral
	| Sim_Dbg of dbg_t

type instantplus_t = { ip_break : bool;
	ip_emit : emit_t list; ip_time : int64 }
let instantplus_i = { ip_break = bool_i;
	ip_emit = [] ; ip_time = int64_i }

type instantminus_t = { im_outmsg : simoutmsg_t list;
	im_time : int64 }
let instantminus_i = { im_outmsg = [];
	im_time = int64_i }

type instant_t = { tr_break : bool;
	tr_emit : emit_t list;
	tr_outmsg : emit_t list;
	tr_eof : bool }

let instant_i = { tr_break = bool_i;
	tr_emit = [];
	tr_outmsg = [];
	tr_eof = bool_i }

type parsebuf_t = { pb_lb: Lexing.lexbuf; mutable pb_line: int }
let parsebuf_i = { pb_lb = Lexing.from_string ""; pb_line = int_i }

(*--------------------------------------------------------------------------*)

let string_of_time f =
	Int64.to_string f

let time_of_string s = (float_of_string s)

(* -------------------------------------------------------------------------
common resources
global data of the simulator environment
------------------------------------------------------------------------- *)
type methods_t = Gui | Reset | Update | Update_display | Show

type stp_data_t = {
	mutable max_height : int;
	mutable max_width_var : int;
	mutable max_width_type : int;
	mutable max_width_val : int;
	mutable max_width_trace : int;
	mutable max_width_file : int;
	mutable max_width_inst : int;
	mutable max_width_files : int;
	mutable max_height_view : int;
	mutable max_width_view : int;
	mutable pres_instant : int;
	mutable mk_tkstepper : unit -> unit;
	mutable update_instant : unit -> unit;
	mutable update_display : unit -> unit;
	mutable update_replay : unit -> unit;
	mutable select_instant : unit -> unit;
	mutable update_log : exn -> unit;
	mutable close : bool -> unit -> unit;
	mutable open_trace : unit -> unit;
	mutable object_browser : methods_t -> unit -> unit;
	mutable log_console : methods_t -> string -> unit -> unit;
	mutable initial_load : string -> unit;
	mutable preferences : unit -> unit;
	mutable object_browser_ex : bool;
	mutable replay_panel_ex : bool;
	mutable control_panel_wp : string ref;
	mutable object_browser_wp : string ref;
	mutable replay_panel_wp : string ref;
	mutable log_console_wp : string ref;
	
	mutable transcript_update : string -> unit;
}

let stp =
	{
		max_height = 25;
		max_width_var = 25;
		max_width_type = 20;
		max_width_val = 12;
		max_width_trace = 80;
		max_width_file = 30;
		max_width_inst = 10;
		max_width_files = 25;
		max_height_view = 40;
		max_width_view = 80;
		pres_instant = 0;
		mk_tkstepper = (fun () -> failwith "stp.mk_tkstepper");
		update_instant = (fun () -> failwith "stp.update_instant");
		update_display = (fun () -> failwith "stp.update_display");
		update_replay = (fun () -> failwith "stp.update_replay");
		select_instant = (fun () -> failwith "stp.select_instant");
		update_log = (fun s -> failwith "stp.update_log");
		close = (fun s () -> failwith "stp.close");
		open_trace = (fun () -> failwith "stp.open_trace");
		object_browser = (fun m () -> failwith "stp.object_browser");
		log_console = (fun m x () -> failwith "stp.log_console");
		initial_load = (fun s -> failwith "stp.initial_load");
		preferences = (fun () -> failwith "stp.preferences");
		
		object_browser_ex = false;
		replay_panel_ex = false;
		
		control_panel_wp = ref "560x300+20+30";
		object_browser_wp = ref "340x300+600+30";
		replay_panel_wp = ref "500x200";
		log_console_wp = ref "+200+150";
		
		transcript_update = fun _ -> ();
	}

(* ----------------------------------------------------------------------- *)

type tsim_showsig = bool * bool option
(* show IO - signals: true = my only; false = my + subobj
show local - sigss: None = do not show;
Some bool = if true: my only; if false: my + subobj *)

type tsim_global_data =
	{
		mutable root_class_name : string;
		mutable root_object_address : int32;
		mutable target_cmd : string;
		mutable target_args : string list;
		mutable target_header : targetheader_t;
		mutable target_md5 : Digest.t option;
		
		mutable parsebuf : parsebuf_t;
		
		mutable conf_signals : tconfsig;
		mutable tracelog_font : string;
		tmp_tracefile : string;
		mutable tr_suffix : string;
		mutable tr_file : string option;
		mutable obj_brw : bool;
		mutable trace_brw : bool;
		mutable show_signals : tsim_showsig;
	}

let sim = {
	root_class_name = "a root class";
	root_object_address = Int32.zero;
	target_cmd = "";
	target_args = [];
	target_header = { target_signals = [];
		target_classes = [];
		target_timing = Int64.zero;
		target_files = [];
		target_config = { root_obj = Int32.zero;
			root_class = "not_yet_defined"
		}
	} ;
	target_md5 = None;
	
	parsebuf = { pb_lb = Lexing.from_string ""; pb_line = 1 };
	conf_signals = AllSig;
	tracelog_font = "Courier 8 normal";
	tmp_tracefile = "se.tmp.setr";
	tr_suffix = ".setr";
	tr_file = None;
	obj_brw = false;
	trace_brw = true;
	show_signals = (false, Some false);
}

type tsim_dynamics =
	{
		mutable delta_time: int64;
	}

let sim_dyn =
	{
		delta_time = Int64.zero
	}
