open Sim_type

let ps s = print_string s
let pn () = print_string "\n"
let pF () = flush stdout
let pi i = ps (string_of_int i)
let pS s = ps s ; pF()
let pN () = pn() ; pF()
let pL s = ps s ; pN()
let pI i = ps" "; pi i ; pF()
let pLL sl = List.iter (fun s -> pL s) sl

let psa args =
	Array.iter
		( fun x ->
			(* ps (" \""); *)
					ps " "; ps (String.escaped x);
			(* ps (" \""); *)
		) args

let psA args = psa args; pN()

(*--------------------------------------------------------------------------*)

let stub_active = ref false
let xstub_active = ref false
let update_log_active = ref false
let gc_info = ref false

let gc_statistics () = if !gc_info then Gc.print_stat Pervasives.stdout; pF()

(* --------------------------------------------------------------------------
common data for instant control
-------------------------------------------------------------------------- *)

let _instant_count = ref (- 1)  (* number of the last processed instant *)

let is_startup_instant () = !_instant_count = (- 1)
let reset_instant_count () = _instant_count := - 1
let incr_instant_count () = _instant_count := !_instant_count + 1

let prev_instant () = !_instant_count

let next_instant () = !_instant_count + 1

(* --------------------------------------------------------------------------
list_remove l el -- return a new list without el,
occurence of el is tested by structural equality.
list_removeq l el -- return a new list without el,
occurence of el is tested by physical equality.
all occurences of el, if any, in list l are removed.
test: list_remove [1; 2; 3; 2; 4] 2 ;;

list_select_first : 'a list -> ('a -> bool) -> 'a
raise Not_Found it the function is false for all elements
-------------------------------------------------------------------------- *)
let list_remove l xel =
	let (x, rl) =
		List.fold_left
			(fun (remel, nl) el -> (remel, (if remel = el then nl else el:: nl)))
			(xel,[]) l ;
	in
	List.rev rl

let list_removeq l xel =
	let (x, rl) =
		List.fold_left
			(fun (remel, nl) el -> (remel, (if remel == el then nl else el:: nl)))
			(xel,[]) l ;
	in
	List.rev rl

let rec list_select_first (l:'a list) (f : ('a -> bool)) =
	match l with
	| h :: t -> if f h then h else list_select_first t f
	| [] -> raise Not_found

(* -----------------------------------------------------------------------
String operations for anaylizing access path
----------------------------------------------------------------------- *)
let path_is_entity_of p e =
	let l = String.length p in
	if l > String.length e
	then false
	else
		let h = String.sub e 0 l in
		p = h && (let tl = String.sub e (l + 1) ((String.length e) - l - 1) in
			not(String.contains tl '.'))

let path_strip_root p =
	if String.contains p '.'
	then
		let n = (String.index p '.') + 1 in
		String.sub p n ((String.length p) - n)
	else ""

let rec path_strip_path p =
	if String.contains p '.'
	then
		let n = (String.index p '.') + 1 in
		path_strip_path (String.sub p n ((String.length p) - n))
	else p

(* -----------------------------------------------------------------------
Handling sim types
----------------------------------------------------------------------- *)
let is_primitive_typ = function
	| "bool"
	| "byte"
	| "short"
	| "int"
	| "long"
	| "char"
	| "uint16"
	| "uint32"
	| "uint64"
	| "float"
	| "double"
	| "time"
	| "String" -> true
	| _ -> false

let is_primitive_or_void_typ = function
	| "void" -> true
	| t -> is_primitive_typ t

let str2primitive_typ = function
	| "bool" -> Ast.t_bool
	
	| "byte" -> Ast.t_byte
	| "short" -> Ast.t_short
	| "int" -> Ast.t_int
	| "long" -> Ast.t_long
	
	| "char" -> Ast.t_char
	| "uint16" -> Ast.t_uint16
	| "uint32" -> Ast.t_uint32
	| "uint64" -> Ast.t_uint64
	
	| "float" -> Ast.t_float
	| "double" -> Ast.t_double
	
	| "time" -> Ast.t_time
	| "String" -> Ast.t_string
	| t -> Sim_error.intern ("str2primitive_typ: "^t)

let simprimitivetyp2default = function
	| "bool" -> (Ast.t_bool,"false", None)
	| "byte" -> (Ast.t_bool,"0", None)
	| "short" -> (Ast.t_short,"0", Some Int64.zero)
	| "int" -> (Ast.t_int,"0", Some Int64.zero)
	| "long" -> (Ast.t_long,"0", Some Int64.zero)
	
	| "char" -> (Ast.t_char,"' '", None)
	| "uint16" -> (Ast.t_uint16,"0", Some Int64.zero)
	| "uint32" -> (Ast.t_uint32,"0", Some Int64.zero)
	| "uint64" -> (Ast.t_uint64,"0", Some Int64.zero)
	
	| "float" -> (Ast.t_float,"0.0f", None)
	| "double" -> (Ast.t_double,"0.0", None)
	
	| "time" -> (Ast.t_time,"0sec", Some Int64.zero)
	| "string" -> (Ast.t_string,"", None)
	| t -> Sim_error.intern ("simtprimitiveyp2default: "^t)

(* -----------------------------------------------------------------------
handling sim values
----------------------------------------------------------------------- *)
let to_objectstr vl =
	String.concat "," (List.map (fun (s, v) -> s^"("^v^")") vl)

let to_vectorstr vl =
	"{"^String.concat "," vl^"}"

let to_matrixstr vll =
	let str vl = "{"^String.concat "," vl^"}" in
	"{"^String.concat "," (List.map str vll)^"}"

let simval2str = function
	| SNull -> ""
	| SLiteral (t, s, v) -> s
	| SObject l -> String.concat "," (List.map (fun (s, (_, v, _)) -> s^"("^v^")") l)
	| SVector l -> "{"^String.concat "," (List.map (fun (_, v, _) -> v) l)^"}"
	| SMatrix ll -> let str l ="{"^String.concat "," (List.map (fun(_, v, _) -> v) l)^"}" in
			"{"^String.concat "," (List.map str ll)^"}"

(* -----------------------------------------------------------------------
handling exceptions

Attention: The error messages correspond to that in se_exception.h
----------------------------------------------------------------------- *)
let exeption2string = function
	| (_, s, Some v) ->
		 let x = Int64.to_int v in
			"exception "^s^":\n"^
			if x = 1 then "throw value is zero" else
			if x = 2 then "null pointer" else
			if x = 3 then "array index out of bounds" else
			if x = 4 then "array hase negative size" else
			if x = 5 then "out of memory" else
			if x = 6 then "synchrony hypothesis violated:\n\
			computation time of the instant \
			exceeds the time bound" else
			if x = 7 then "maximal possible time for an \
			instant exceeded for the given\n\
			target architecture" else
			if x = 8 then "class cast" else
			if x = 9 then "dt used in the first instant" else
				"user defined "^string_of_int x
	| _ -> Err.intern "exeption2string"

