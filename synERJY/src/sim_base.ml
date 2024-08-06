(* sim_base.ml *)

open Ast        (* ast.ml   *)
open Err        (* err.ml   *)

open Sim_type   (* sim_type.ml  *)
open Sim_util   (* sim_util.ml  *)

(* -------------------------------------------------------------------------
* HISTORY
* stores the traced values of a signal or an data - member (attribute).
*
* unittest: $SE_HOME / testsuite / simul / sim.ml / history_test.ml
*)

module History

(* begin signature *)
: sig

	type history_t
	
	val create : unit -> history_t
	val clear : history_t -> unit
	val add : history_t -> int -> simval_t -> unit
	val is_present_at : history_t -> int -> bool
	val get_value_at : history_t -> int -> simval_t
end
(* end signature *)

=

struct
	
	type history_t = { mutable store : (int * simval_t) list }
	
	let create () = { store = [] }
	
	let clear (history: history_t) = history.store <- []
	
	let add (history: history_t) x lit = history.store <- (x, lit) :: history.store
	
	let rec get_value_at_rec l x =
		(match l with
			| (key, lit) :: t
			-> if (x >= key)
					then lit
					else get_value_at_rec t x
			| [] -> SNull
		)
	
	let get_value_at (history : history_t) (instant: int) =
		get_value_at_rec history.store instant
	
	let is_present_at (history : history_t) instant =
		try let _ = List.assoc instant history.store in true
		with Not_found -> false
	
end;;

(* --------------------------------------------------------------------------
the class any
may be inherited or used for type conversion in order to be able
to check self against the identity of an object or
to give a reference to self as parameter.
-------------------------------------------------------------------------- *)

class c_any () =
object (self)
end
;;

(* --------------------------------------------------------------------------
* auxiliary classes
*)

class c_path_position (my_path : c_any) (my_position : int) =
	(* the memento class for c_path *)
	object (self)
		val _path = my_path
		method path = _path
		
		val _position = my_position
		method position = _position
		
	end (* c_path_position *)

and c_path (the_path : string) (the_separator : char) =

object (self)
	inherit c_any () as super
	
	val _full_path = the_path
	method full_path = _full_path
	
	val _separator = the_separator
	method separator = _separator
	
	val mutable _name_index = - 1 (* see initializer *)
	
	val _top_index = 0
	val _bottom_index =
		try (String.rindex the_path the_separator) + 1
		with Not_found -> 0
	
	method is_top = (_name_index = _top_index)
	method is_bottom = (_name_index = _bottom_index)
	
	method top = _name_index <- _top_index; self
	method bottom = _name_index <- _bottom_index; self
	
	method up =
		assert (not self#is_top);
		_name_index <- (
			try
				(String.rindex_from _full_path (_name_index - 2) _separator) + 1
			with Not_found -> _top_index
		);
		self
	
	method down =
		assert (not self#is_bottom);
		_name_index <- (
			try (String.index_from _full_path (_name_index) _separator) + 1
			with Not_found -> assert false
		);
		self
	
	method name =
		String.sub _full_path _name_index
			( ( try
					String.index_from _full_path _name_index _separator
				with Not_found -> String.length _full_path
				) - _name_index
			)
	
	method as_any = (self : #c_any :> c_any)
	
	method get_position =
		new c_path_position (self#as_any) _name_index
	
	method set_position (p : c_path_position) =
		assert (p#path = self#as_any);
		_name_index <- p#position
	
	method pL =
		ps "c_path "; ps (self#full_path);
		ps " name_index=" ; pi _name_index;
		ps " top_index=" ; pi _top_index;
		ps " bottom_index="; pi _bottom_index;
		pn()
	
	initializer
	_name_index <- _bottom_index
	
end (* c_path *)
;;
(* test
012345678901234
let opn = "aa.bb.c..ddd.end";;
let op = new c_path opn '.';;
op#full_path = opn;;
op#top#name = "aa" ;;
op#is_top;;
op#top#down#name = "bb";;
op#top#down#down#name = "c";;
op#top#down#down#down#name = "";;
op#top#down#down#down#down#name = "ddd";;
op#top#down#down#down#down#down#name = "end";;
op#bottom#name = "end" ;;
op#is_bottom;;
op#bottom#up#name = "ddd";;
op#bottom#up#up#name = "";;
op#bottom#up#up#up#name = "c";;

op#top#up;;        (* Assert_failure(_) *)
op#bottom#down;;   (* Assert_failure(_) *)

op#pL;;
*)

let root_signals = ref []

(* ------------------------------------------------------------------------ *)
class virtual c_sim_top () =
	(* minimal meta object protocol and debug facilities *)
	object (self)
		method oid = "o"^(string_of_int(Obj.magic (Obj.field(Obj.repr self) 1) :> int));
		method classname = "c_sim_top"
		method xstub m =
			if !xstub_active then (
				ps "* xstub: ("; ps self#classname;
				ps ") oid#msg = ";
				ps self#oid; ps "#"; pL m;
			);
			()
		
		method nyi m = pL "* not yet implemented: "; self#xstub m
		
		method stub m = if !stub_active then self#xstub m;
		
		method s = (self#oid)^":"^(self#classname)
		method p = ps self#s; ()
		method pL = self#p ; pn () ; ()
		method pLL = self#pL; ()
		
	end (* c_sim_top *)
;;

(* ------------------------------------------------------------------------ *)
class virtual c_sim_usemarkable () =
object (self)
	inherit c_sim_top ()
	
	(* -------- usage for simulation *)
	
	val mutable _sim_used = false
	
	method is_sim_used = _sim_used
	method set_sim_used = _sim_used <- true
	method set_sim_unused = _sim_used <- false
end (* c_sim_usemarkable *)
;;

(* ------------------------------------------------------------------------ *)
class type ct_parent =
object
	method oid : string
	method get_full_name : string
	method get_name : string
	method name : string
	method access_path : memrep_t list
end (* ct_parent *)
;;

(* ------------------------------------------------------------------------ *)
class virtual ['target]
c_sE_trace_item (aName : string) (fspec : fieldspec_t) (aParent : ct_parent) (target : 'target) =

object (self)
	inherit c_sim_usemarkable () as super
	
	method oid = "f_"^(super#oid)
	
	method name = aName
	method fspec = fspec
	
	method valuetype =
		match fspec with
		| SSignal ssp -> ssp.sigvalt.idtype
		| SMember msp -> msp.spec.idtype
	
	method valuedim =
		match fspec with
		| SSignal ssp -> ssp.sigvalt.iddim
		| SMember msp -> msp.spec.iddim
	
	method initvalue =
		let t = self#valuetype in
		match self#valuedim with
		| [] ->
				if t = "void"
				then SNull
				else
				if is_primitive_typ t
				then SLiteral (simprimitivetyp2default t)
				else
					let cl = target#classes#select self#valuetype in
					SObject
					( List.map2
							( fun n t -> (n, simprimitivetyp2default t)
							) cl#field_names
							cl#field_types
					)
		| [n] | [n; 1]| [1; n] ->
				SVector(Array.to_list(Array.make n (simprimitivetyp2default t)))
		| [n1; n2] ->
				let l = Array.to_list (Array.make n2 (simprimitivetyp2default t)) in
				SMatrix(Array.to_list(Array.make n1 l))
		| _ ->
				Sim_error.intern "initvalue"
	
	method flow =
		match fspec with
		| SSignal ssp -> ssp.sigflow
		| SMember _ -> assert false
	
	method is_signal =
		match fspec with
		| SSignal _ -> true
		| _ -> false
	
	method is_attribute =
		match fspec with
		| SMember _ -> true
		| _ -> false
	
	method is_input =
		match fspec with
		| SSignal ssp -> ssp.sigflow = SInput
		| SMember _ -> false
	
	method is_output =
		match fspec with
		| SSignal ssp -> ssp.sigflow = SOutput
		| SMember _ -> false
	
	method is_local =
		match fspec with
		| SSignal ssp -> ssp.sigflow = SLocal
		| SMember _ -> false
	
	method classname = self#valuetype
	
	val mutable _parent = aParent
	method parent = _parent
	
	method set_parent par = _parent <- par
	
	method get_full_name =
		self#parent#get_full_name ^ "." ^ self#get_name
	
	method get_name = aName
	
	method get_type =
		match self#valuedim with
		| [] ->
				self#valuetype
		| [n] | [1; n] | [n; 1] ->
				self#valuetype^"["^string_of_int n^"]"
		| [n1; n2] ->
				self#valuetype^"["^string_of_int n1^","^string_of_int n2^"]"
		| _ ->
				Sim_error.intern "get_type:dimension"
	
	val mutable _value = SNull
	method value = _value
	
	method put_value v =
		_value <- v
	
	method private clear_value =
		_value <- SNull
	
	method virtual set_present : unit
	method virtual unset_present : unit
	
end (* c_sE_trace_item *)

(* ------------------------------------------------------------------------ *)
and ['target] c_sE_object
(aName : string)
(aClass : c_sE_target c_sE_class)
(aParent : ct_parent option)
(aLocation : memrep_t)
(target : 'target) =

object (self)
	inherit c_sim_usemarkable () as super
	
	method oid = "o_"^(super#oid)
	
	val _name = aName
	method name = _name
	val _parent_opt = aParent
	method parent_opt = _parent_opt
	
	val _myClass = aClass
	method get_class = _myClass
	
	method as_parent = (self : #ct_parent :> ct_parent)
	
	method access_path =
		( match aParent with
			| None -> (* this is a root object *)
					( match aLocation with
						| SInPlace(d) -> [SInPlace(d)]
						| _ -> assert false
					)
			| Some(p) ->
					aLocation:: p#access_path
		)
	
	val mutable geometry = "+10+400"    (* window position of trace browser *)
	method set_geometry g = geometry <- g
	method get_geometry = geometry
	
	(* a bit of a hack, Axel *)
	val mutable _dbgs = []
	val _dbgs_tbl = Hashtbl.create 123
	
	method dbgs = _dbgs
	
	method add_dbg (dbg: dbg_t) =
		_dbgs <- dbg:: _dbgs
	
	method get_dbgs_at instant =
		if instant = next_instant()
		then (
			_dbgs
		) else (
			try Hashtbl.find _dbgs_tbl instant
			with Not_found ->
					Sim_error.intern("debug information not found for instant "^string_of_int instant)
		)
	
	method reset_dbgs =
		Hashtbl.add _dbgs_tbl (next_instant()) _dbgs;
		_dbgs <- []
	
	method reset_instants =
		_dbgs <- [];
		Hashtbl.clear _dbgs_tbl;
		Hashtbl.add _dbgs_tbl (- 1) []
	
	method save =
		Hashtbl.add _dbgs_tbl (next_instant()) _dbgs
	(* end of the hack *)
	
	val mutable _fields = ([] : c_sE_target c_field list)
	val mutable _inputs = ([] : c_sE_target c_input list)
	
	val mutable _objects = (None : c_sE_target c_sE_object list option)
	
	method clear =
		_fields <- [];
		_inputs <- [];
		_objects <- None;
	
	method fields =
		_fields
	
	method inputs =
		_inputs
	
	method set_inputs l = (* XXX inputs should be built in add_field *)
		_inputs <- l
	
	method objects =
		( match _objects with
			| None ->
					let l = _myClass#make_subobjects_for (self#as_parent) in
					_objects <- Some(l);
					l
			| Some l -> l
		)
	
	method add_field (f : c_sE_target c_field) =
		f#set_parent (self#as_parent);
		_fields <- f:: self#fields;
		if f#is_signal then (
			f#set_selected
				( match sim.conf_signals with
					| AllSig -> true
					| InOutSig -> f#is_input || f#is_output
				);
			(* XXX inputs
			if f#is_input then (
			let i = target#make_input (self#as_parent) f#name f#fspec in
			i#set_selected true;
			_inputs <- i:: self#inputs
			)
			*)
		);
	
	method add_object (x : c_sE_target c_sE_object) =
		target#objects#add x;
		_objects <- Some(x:: self#objects)
	
	method has_field (nm: string) =
		List.exists (fun x -> x#name = nm) self#fields
	
	method get_field (nm: string) =
		list_select_first self#fields (fun x -> x#name = nm)
	
	method has_object (nm: string) =
		List.exists (fun x -> x#name = nm) self#objects
	
	method get_object (nm: string) =
		list_select_first self#objects (fun x -> x#name = nm)
	
	val mutable _open = false
	val mutable _present = false
	
	method get_oid_of (op : c_path) =
		Sim_error.xtrap ("get_oid_of "^(op#name));
		if op#name = _name then (
			if op#is_bottom then (
				self#oid
			) else (
				let n = (op#down)#name
				in
				(list_select_first self#objects
						(fun el -> n = el#name) ) #get_oid_of(op)
			)
		) else (
			raise Not_found
		)
	
	method get_full_name =
		match _parent_opt with
		| None -> self#name
		| Some (p) -> p#get_full_name ^ "." ^ self#name
	
	method get_name = _name
	
	method get_type =
		_myClass#name
	
	method get_actual_childs =
		List.fold_right
			( fun x l -> if x#is_open then x:: l else l
			) self#objects []
	
	method get_childs =
		List.fold_right
			( fun x l -> x:: l
			) self#objects []
	
	method set_open =
		_open <- true
	
	method set_closed =
		_open <- false
	
	method is_open =
		_open
	
	method is_present =
		_present
	
	val mutable _field_selection = []
	
	method reset_selections =
		List.iter (fun x -> x#reset_selected) self#fields;
		List.iter (fun x -> x#reset_selected) self#inputs;
	
	method set_selected_fields selected_fields =
		_field_selection <- selected_fields
	
	method get_selected_fields =
		_field_selection
	
	method check_and_select_fields sel_fields =
		List.iter (fun f -> f#set_selected false) self#fields;
		let fs = ref [] in
		List.iter
			(fun f_name ->
						List.iter (fun f -> if f#get_full_name = f_name then (
											f#set_selected true;
											fs := f::!fs
										)
							) self#fields
			) sel_fields;
		_field_selection <- !fs
	
	method get_selected_inputs =
		List.fold_right
			( fun x l -> if x#is_selected then x:: l else l
			) self#inputs []
	
	method check_and_select_inputs input_names =
		let set_input name =
			List.iter
				( fun x ->
							if x#get_full_name = name then (
								x#set_selected true;
							)
				) self#inputs
		in
		List.iter (fun n -> set_input n) input_names
	
	(* debug *)
	
	method s = (self#get_full_name)^":"^(self#classname)
	
	initializer (
		(* this is somewhat of a hack. The root objects get a
		name "the_root_class". This name is needed for the object browser
		so far. But the signals provided by the se.a.out do not know
		this name. Hence we have to strip it
		*)
		List.iter
			( fun (k, x) -> if path_is_entity_of
							(path_strip_root (self#get_full_name))
							k
						then self#add_field x
			) !root_signals;
		_field_selection <- self#fields;
		Hashtbl.add _dbgs_tbl (- 1) [];
		self#reset_instants
	)
	
end (* c_sE_object *)

(* ------------------------------------------------------------------------ *)
and ['target] c_input
(aName : string)
(fspec : fieldspec_t)
(aParent : ct_parent)
(target : 'target) =

object (self)
	inherit ['target] c_sE_trace_item aName fspec aParent target as super
	
	method check_format nval =
		try let v = Sim_parse.parse_val_string nval in
			( match v with
				| SNull ->
						if self#valuetype = "void" then (
							self#put_value v;
							true
						) else (
							false
						)
				| SLiteral (t, s, i) ->
						let t' = str2primitive_typ self#valuetype in
						if Util.le_type ~sub: t ~super: t' then (
							self#put_value v;
							true
						) else (
							false
						)
				
				| SObject l ->
						( try if List.fold_left2
								( fun accu (_, (t, _, _)) t' ->
											let t' = str2primitive_typ t' in
											accu && Util.le_type ~sub: t ~super: t'
								) true
								l
								(target#classes#select self#valuetype)#field_types
							then (
								self#put_value v;
								true
							) else (
								false
							)
						with _ -> Sim_error.intern "check_format:SObject"
						)
				
				| SVector l ->
						( match self#valuedim with
							| [n] | [n; 1]| [1; n] ->
									if List.length l = n &&
									List.fold_left
										( fun accu (t, _, _) ->
													let t' = str2primitive_typ self#valuetype in
													accu && Util.le_type ~sub: t ~super: t'
											
										) true l
									then (
										self#put_value v;
										true
									) else (
										false
									)
							| _ -> Sim_error.intern "check_format:SVector"
						)
				
				| SMatrix ll ->
						( match self#valuedim with
							| [n1; n2] ->
									let check l =
										List.length l = n2 &&
										List.fold_left
											( fun accu (t, _, _) ->
														let t' = str2primitive_typ self#valuetype in
														accu && Util.le_type ~sub: t ~super: t'
											) true l
									in
									if List.length ll = n1 &&
									List.fold_left
										( fun accu l -> accu && check l
										) true ll
									then (
										self#put_value v;
										true
									) else (
										false
									)
							| _ -> Sim_error.intern "check_format:SMatrix"
						)
			)
		with
		| Parsing.Parse_error -> false
		| e -> pL (Printexc.to_string e); P.pF(); false
	
	method check_format_at nval index =
		try
			let v = Sim_parse.parse_val_string nval in
			let tl = (target#classes#select self#valuetype)#field_types in
			let t' = (Array.of_list tl).(index) in
			match v with
			| SNull ->
					t' = "void"
			| SLiteral (t, s, i) ->
					let t' = str2primitive_typ t' in
					Util.le_type ~sub: t ~super: t'
			| x ->
					Sim_error.intern ("check_format_at "^simval2str x)
		with
		| Parsing.Parse_error -> false
		| e -> pL (Printexc.to_string e); P.pF(); false
	
	method value_format =
		self#valuetype^match self#valuedim with
		| [] -> ""
		| [n] -> "["^string_of_int n^"]"
		| [1; n] -> "["^string_of_int n^"]"
		| [n; 1] -> "["^string_of_int n^"]"
		| [n1; n2] -> "["^string_of_int n1^","^string_of_int n2^"]"
		| _ -> Sim_error.intern "value_format"
	
	val mutable _permanent = false
	val mutable _present = false
	
	method is_absent = not _present
	method is_permanent = _permanent
	method is_present = _present
	
	method set_permanent =
		_permanent <- true;
		_present <- true
	
	method set_present =
		_permanent <- false;
		_present <- true
	
	method unset_present =
		_permanent <- false;
		_present <- false;
	(* the value is kept *)
	
	val mutable _selected = true
	
	method is_selected = _selected
	method set_selected (b: bool) = _selected <- b
	
	method reset_selected =
		self#set_selected false;
	
	method signalspec =
		(match fspec with
			| SSignal ssp -> ssp
			| SMember _ -> assert false
		)
	
	method emit =
		(match fspec with
			| SSignal ssp ->
					{ emitname = ssp.signame;
						emitarg = _value;
						emitflow = Some ssp.sigflow
					}
			| SMember _ -> assert false
		)
	
	(* -------- debug -------- *)
	
	method s = (self#get_full_name)^":"^(self#classname)
	
	method pL =
		super#p;
		ps " oid: "; ps (self#oid);
		ps " name: "; ps (self#name);
		ps " val: "; ps (simval2str (self#value));
		ps " prs: "; ps (string_of_bool _present);
		ps " sel: "; pL (string_of_bool _selected);
	
	(* -------- initializer -------- *)
	
	initializer (
		_value <- self#initvalue;
		self#reset_selected;
		self#unset_present
	)
	
end  (* c_input *)

(* ------------------------------------------------------------------------ *)
and ['target] c_field
(aName : string)
(fspec : fieldspec_t)
(aParent : ct_parent)
(target : 'target) =

object (self)
	inherit ['target] c_sE_trace_item aName fspec aParent target as super
	
	(* if an entry exists then the signal is present at this instant.
	* an attribute is always present, if the entry does not exist, the
	* the value was not recorded
	*)
	val _history = History.create ()
	
	method get_value_at instant =
		if instant = (next_instant()) then (
			self#value
		) else (
			History.get_value_at _history instant
		)
	
	val mutable _present = false
	
	method is_absent = not _present
	method is_present = _present
	
	method is_present_at instant =
		if instant = next_instant() then (
			match fspec with
			| SSignal _ -> self#is_present
			| SMember _ -> self#is_selected
		) else (
			History.is_present_at _history instant
		)
	
	method get_traceinfo instant =
		let p = self#is_present_at instant
		and v = self#get_value_at instant in
		( match self#valuetype with
			| "void" ->
					if p then "*" else "-"
			| "bool" ->
					if simval2str v = "true"
					then
						if p then "T" else "-"
					else if p then "F" else "-"
			| _ ->
					if p then "$" else "-"
		)
	
	val mutable _start_instant = (- 1)
	
	method is_sampled_at instant = instant >= _start_instant
	
	method set_present =
		_present <- true;
	
	method unset_present =
		_present <- false;
		self#clear_value;
	
	val mutable _selected = true
	
	method is_selected = _selected
	method set_selected (b: bool) = _selected <- b
	
	method reset_selected =
		self#set_selected false;
	
	val mutable _access_path = ([] : memrep_t list)
	method access_path = _access_path
	
	method save =
		if ( match fspec with
			| SSignal _ -> _present
			| SMember _ -> _selected
		) then
			History.add _history (next_instant()) _value
	
	method signalspec =
		(match fspec with
			| SSignal ssp -> ssp
			| SMember _ -> assert false
		)
	
	method emit =
		(match fspec with
			| SSignal ssp -> { emitname = ssp.signame; emitarg = _value;
						emitflow = Some ssp.sigflow }
			| SMember _ -> assert false
		)
	
	(* -------- debug -------- *)
	
	method s = (self#get_full_name)^":"^(self#classname)
	
	method pL =
		super#p;
		ps " oid: "; ps (self#oid);
		ps " name: "; ps (self#name);
		ps " val: "; ps (simval2str (self#value));
		ps " prs: "; ps (string_of_bool _present);
		ps " sel: "; pL (string_of_bool _selected);
	
	method pLH =
		self#pL;
		let x = ref (- 1) in
		while !x <= next_instant() do
			ps (string_of_int !x);
			if self#is_present_at !x then (
				pL (": "^simval2str (self#get_value_at !x))
			) else (
				pL " -"
			);
			x := !x + 1;
		done
	
	(* -------- initialization -------- *)
	
	method reset_instants =
		(* Reset history, presence and anything else
		* but the selection
		*)
		_value <- self#initvalue;
		self#unset_present;
		self#set_sim_unused;
		History.clear _history;
		_access_path <- (
			match fspec with
			| SSignal _ -> []
			| SMember msp -> List.rev (msp.rep :: aParent#access_path);
		);
		()
	
	(* -------- initializer -------- *)
	
	initializer
	self#reset_instants;
	self#reset_selected;
	
end  (* c_field *)

(* ------------------------------------------------------------------------ *)
and ['target] c_sE_class
(clspec: simclass_t)
(target:'target) =

object (self)
	
	method name = clspec.classnm
	method source = clspec.classtext
	method grafics = clspec.classgraficfiles
	method constr_label = clspec.constr_label
	method get_tags = clspec.tags
	
	method classname = "c_sE_class"
	
	method cid = self#name     (* use name as oid *)
	
	method is_root_class =
		self#name = sim.root_class_name
	
	(*   class is reactive only if it has a constructor label *)
	method is_reactive = (clspec.constr_label <> None)
	
	method get_srcpos lbl =
		List.assoc lbl clspec.labels
	
	method get_text_pos lbl =
		match self#get_srcpos lbl with
		| LblTextSel t -> t
		| _ -> Sim_error.intern ("improper format for method or constructor position")
	
	method get_graphic_pos lbl =
		match self#get_srcpos lbl with
		| LblGO t -> t
		| _ -> Sim_error.intern ("improper format for graphic position")
	
	(** The sequence of field and sub-object definitions.  *)
	val mutable _object_defs = clspec.members
	
	(** Produce a list of field types of an object of this sE_class *)
	method field_types =
		List.map (fun el -> el.spec.idtype) _object_defs
	
	(** Produce a list of field names of an object of this sE_class *)
	method field_names =
		List.map (fun el -> el.spec.idname) _object_defs
	
	(** Produce a list of new sub_objects for a parent object *)
	method make_subobjects_for (parent : ct_parent) =
		let result = ref ([] : (c_sE_target c_sE_object) list) in
		List.iter
			( fun msp ->
						let aName = msp.spec.idname in
						let aTypeName = msp.spec.idtype in
						match msp.rep with
						| SByReference aDisplacement ->
								let n = target#make_object (Some parent) aName aTypeName
										(SByReference aDisplacement)
								in
								result := n :: !result
						| SInPlace aDisplacement ->
								if not(is_primitive_typ aTypeName || aTypeName = "void") then
									let n = target#make_object (Some parent) aName aTypeName
											(SInPlace aDisplacement)
									in
									result := n :: !result
			) _object_defs;
		!result
	
end  (* c_sE_class  *)

(* ------------------------------------------------------------------------ *)
and ['target] c_sE_classes (target:'target) =
object (self)
	inherit c_sim_top ()
	
	method classname = "c_sE_classes"
	
	val _container = (Hashtbl.create 101)
	
	method add (cl:'a c_sE_class) =
		try
			let _ = Hashtbl.find _container cl#name
			in ()
		with
		| Not_found -> Hashtbl.add _container cl#name cl
	
	method select name =
		try Hashtbl.find _container name
		with Not_found ->
				Err.intern ("c_sE_classes:select: "^name)
	
	method includes aClassname =
		try
			let _ = Hashtbl.find _container aClassname
			in true
		with
		| Not_found -> false
	
	method pL =
		let l = ref 0 in
		pL "sE_classes";
		Hashtbl.iter (fun k v -> l := !l + 1; ps k; pn()) _container;
		if !l = 0 then ps "**empty**"; pn()
	
end (* c_sE_classes *)

(* ------------------------------------------------------------------------ *)
and c_objects () =
object (self)
	inherit c_sim_top () as super
	
	method classname = "c_objects"
	
	val _table = (Hashtbl.create 101: (string, c_sE_target c_sE_object) Hashtbl.t)
	
	method add (el : c_sE_target c_sE_object) =
		( try let _ = Hashtbl.find _table el#oid
			in ()
		with Not_found ->
				Hashtbl.add _table el#oid el
		);
	
	method get oid = Hashtbl.find _table oid (* raise Not_found if not included *)
	
	method get_oid_of (object_path: c_path) =
		self#xstub "get_oid_of ";
		let oid = ref ""
		and fullname = object_path#full_path
		in
		Hashtbl.iter
			( fun x y -> if y#get_full_name = fullname
						then oid := y#oid
			) _table ;
		!oid
	
	method find fullname =
		let ol = ref [] in
		Hashtbl.iter
			( fun x y -> if y#get_full_name = fullname
						then ol := y::!ol
			) _table;
		match !ol with
		| [] -> Sim_error.intern ("object not found: "^fullname)
		| [o] -> o
		| _ -> Sim_error.intern ("object exists twice: "^fullname)
	
	method reset_dbgs =
		Hashtbl.iter (fun k v -> v#reset_dbgs) _table
	
	method reset_selections =
		Hashtbl.iter (fun k v -> v#reset_selections) _table
	
	method set_closed =
		Hashtbl.iter (fun k x -> x#set_closed) _table
	
	method clear =
		Hashtbl.iter (fun k x -> x#clear) _table;
		Hashtbl.clear _table
	
	method reset_instants =
		Hashtbl.iter (fun k v -> v#reset_instants) _table
	
	method iter f =
		let ff k e = f e in
		Hashtbl.iter ff _table
	
	(* -------- debug *)
	
	method pL =
		let l = ref 0 in
		pL "Hashtbl";
		Hashtbl.iter (fun k v -> l := !l + 1;
						ps k; ps " -> "; v#pL ) _table;
		if !l = 0 then pL "empty";
	
end (* c_objects *)

(* ------------------------------------------------------------------------ *)
and c_fields () =
object (self)
	inherit c_sim_top () as super
	
	method classname = "c_fields"
	
	val _table = (Hashtbl.create 101 : (string, c_sE_target c_field) Hashtbl.t)
	
	method add (el: c_sE_target c_field) =
		( try
			let _ = Hashtbl.find _table el#oid in
			()
		with Not_found ->
				Hashtbl.add _table el#oid el
		);
	
	method get oid = Hashtbl.find _table oid (* raise Not_found if not included *)
	
	method reset_instants =
		Hashtbl.iter (fun k v -> v#reset_instants) _table
	
	method clear =
		Hashtbl.clear _table;
		()
	
	method iter f =
		let ff k e = f e in
		Hashtbl.iter ff _table
	
	(* -------- debug *)
	
	method pL =
		let l = ref 0 in
		pL "Hashtbl";
		Hashtbl.iter
			( fun k v -> l := !l + 1;
						ps k; ps " -> "; v#pL
			) _table;
		if !l = 0 then pL "empty";
	
end (* c_fields *)

(* ------------------------------------------------------------------------ *)
and c_sE_target () =
	(* Manage the access path to the simulation target
	* through a tree of objects as nodes and fields as leafs.
	* This class also defines factory methods for objects and fields.
	*)
	object (self)
		inherit c_sim_top ()
		
		method classname = "c_sE_target"
		
		val mutable _fields = new c_fields ()
		val mutable _objects = new c_objects ()
		
		val mutable _sE_classes = (None : (c_sE_target c_sE_classes) option)
		
		method classes =
			match _sE_classes with
			| None -> Sim_error.intern "sE_classes not initialized"
			| Some sc -> sc
		
		method set_sE_classes (sc : c_sE_target c_sE_classes) =
			_sE_classes <- Some(sc)
		
		method fields = _fields
		method objects = _objects
		
		method make_field (aParent: ct_parent) (aName: string) (fspec: fieldspec_t) =
			let aName = path_strip_path aName in
			let x = new c_field aName fspec aParent (self:> c_sE_target) in
			self#fields#add x;
			x
		
		method make_input (aParent: ct_parent) (aName: string) (fspec: fieldspec_t) =
			let x = new c_input aName fspec aParent (self:> c_sE_target) in
			x
		
		method make_object (aParent_opt: ct_parent option) (aName: string) aStandardClassName aDisplacement =
			let aClass = (self#classes#select aStandardClassName) in
			let o = new c_sE_object aName aClass aParent_opt aDisplacement (self:> c_sE_target) in
			self#objects#add o;
			o
		
	end (* c_sE_target *)
;;

(* ------------------------------------------------------------------------ *)
let sE_target = new c_sE_target () ;;

sE_target#set_sE_classes (new c_sE_classes sE_target) ;;

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------
* root_inputs: the latches to handle inputs for input - signals
*)
let root_inputs = ref ([] : (string * c_sE_target c_input) list) ;;

let root_inputs_set_selected b =
	List.iter (fun (_, i) -> i#set_selected b) !root_inputs;;

let root_inputs_pL () =
	List.iter (fun (_, i) -> i#pL) !root_inputs;;

let root_outputs = ref ([] : (string * c_sE_target c_field) list);;
(* ------------------------------------------------------------------------
* root_signals: the signal interface of the target
* functions to select and deselect all root signals
*)

let root_signals_select () =
	List.iter
		( fun (n, s) ->
					s#set_selected
						( match sim.conf_signals with
							| AllSig -> true
							| InOutSig -> s#is_input || s#is_output
						)
		) !root_signals;;

let root_signals_pL () =
	List.iter (fun (n, s) -> s#pL) !root_signals;;

(* ----------------------------------------------------------------------
* Dummies for a root - class and a root - object.
* The dummy root object is used to collect the set of signals,
* which later is transferred to the "real" root object.
* The "real" root object is declared at the end of
* the meta information produced by the simulation target.
* The dummies are not included into the sets of sE_classes and sE_objects.
* The dummy - root - object is not part of the sim_target path.
*)
let root_class_dummy = new c_sE_class simclass_i sE_target ;;

let root_object_dummy = new c_sE_object "root_object_dummy"
		root_class_dummy
		None
		(SInPlace Int32.zero)
		sE_target
;;

