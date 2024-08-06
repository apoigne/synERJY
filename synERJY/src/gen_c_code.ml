open Ly
open Ast
open Util
open P
open Util_print
open Util_gen
open Util_gen_c
open Gen_application
open Gen_rct_code

(* -------------------------------------------------------------------------
generate code for methods
Gen_application.paraml_structure is called to store the max number of
builtin - and ref - types usedin the formal param list. This is used to prepare
deterministic left - to - right evaluation of parameter lists.
------------------------------------------------------------------------- *)
type tdcl_df =
	| Declaration
	| Definition

let cc_cstr_cback stmt =
	let err () = Err.intern "cc_cstr_cback" in
	let sgn =
		match stmt with
		| ExprStmt(_, a) ->
				( match a.expr with
					| Assign(s, NoArr, _, _) -> s
					| _ -> err()
				)
		| _ -> err() in
	let dcl = get_decl_after_tc (type2id cc.cc_typ) sgn (- 1) in
	callback_init ~called_from_decl: false dcl

let cc_method dcl_df anonymous method_kind formals body pre post cback_inits =
	let is_constructor = is_constructor cc.cc_decl in
	let result_typ = subst_map cc.cc_decl.signature.rt cc.cc_v2t in
	cc.cc_has_rtn_lbl <- post != [];
	
	match body with
	| Nobody ->
			( match method_kind with
				| NativeMethod c ->
						if dcl_df = Definition then (
						(* native methods do not have a definition == a body *)
						) else (
							ps "#define ";
							ps " f_"; ps (cc_type2str cc.cc_typ); ps "_";
							psmf cc.cc_decl.name; ps "_"; print_arity cc.cc_decl;
							ps "(";
							if cc.cc_decl.scope = Class
							then ()
							else
							if formals = []
							then ps "p__this"
							else ps "p__this,";
							ps (lst_sep2str (fun p -> mf2str p.p_name) ", " formals);
							ps ") ";
							ps c;
							ps "(";
							ps (lst_sep2str (fun p -> mf2str p.p_name) ", " formals);
							ps ")\n"
						)
				| _ -> () (* nothing to do if method is non-native and body-less *)
			)
	| TextStmtL sl when cc.cc_decl.name = id_main && ( se.target_sys = Simulink || se.target_sys = Scicos ) ->
			()
	| TextStmtL sl when method_kind = DataMethod -> (* related to the head of a method *)
			if sl = [] then ps "inline ";
			if is_constructor
			then ( ps "pat_"; ps (cc_type2str cc.cc_typ) )
			else ( ps (cc_typedef result_typ) );
			let native_name =
				match cc.cc_decl.entry with
				| Method m ->
						( match m.method_kind with
							| NativeMethod m -> Some m
							| _ -> None
						)
				| _ -> None
			in
			( match native_name with
				| None -> ps " f_"; ps (cc_type2str cc.cc_typ); ps "_";
						psmf cc.cc_decl.name; ps "_"; print_arity cc.cc_decl
				| Some n -> ps " "; ps n
			);
			if formals = [] && cc.cc_decl.scope = Class then (
				ps "( void )"
			) else (
				ps "( ";
				if cc.cc_decl.scope = Class || not(native_name = None) then (
				(* no this pointer for static & native methods *)
				) else (
					ps "pat_"; ps (cc_type2str cc.cc_typ);
					if is_constructor
					then ps " p__this"
					else (
						match anonymous with
						| None -> ps " p__this"
						| Some _ -> ps " a__this"
					);
					if formals = []
					then ()
					else ps ", "
				);
				ps (lst_sep2str cc_entity ", " formals); ps " )";
			);
			(* related to the body of a method *)
			( match dcl_df with
				| Declaration -> ps ";\n"
				| Definition ->
						ps "\n{\n";
						( match anonymous, cc.cc_decl.scope with
							| None, _ -> ();
							| _, Class -> ();
							| Some p, _ ->
									pT 1;
									if is_constructor then (
										ps " p__this->this__anon = p1;\n"
									) else (
										ps "pat_"; ps p;
										ps " p__this = (a__this->this__anon);\n"
									)
						);
						if result_typ = Null || cc.cc_has_rtn_lbl = false
						then ()
						else (
							pT 1; ps (cc_typedef result_typ); ps " __result;\n"
						);
						if is_constructor then (
						(* no preconditions, no invariant check *)
						) else (
							List.iter (c_assert 1) pre
						);
						List.iter (c_code_stmt 1) sl;
						if gen_for_targetsys () then (
							List.iter cc_cstr_cback cback_inits
						) else (
						(* callbacks only in target systems *)
						);
						if (cc.cc_has_rtn_lbl) then (
							pT 1; ps "return_label:\n";
							List.iter (c_assert 1) post
						) else (
						(* no postconditions, no invariants *)
						);
						if is_constructor then (pT 1; ps "return p__this;")
						else if (result_typ = Null) then (pT 1; ps "return;")
						else if (cc.cc_has_rtn_lbl) then (pT 1; ps "return __result;")
						else (); (* the required return stmt cares about rtn val *)
						ps "\n}\n\n"
			)
	| TextStmtL sl ->
			if sl = [] then ps "inline ";
			let intr_desc =
				match method_kind with
				| NativeIntr d -> d
				| _ -> Err.intern "cc_method" in
			match dcl_df with
			| Declaration -> () (* no declaration in header files *)
			| Definition ->
					ps intr_desc; ps " {\n";
					cc.cc_has_rtn_lbl <- true; (* suppress return statements *)
					List.iter (c_code_stmt 1) sl;
					pT 1; ps "return_label: ;\n";
					ps "}\n\n"

(* =========================================================================
generate code for a type / class
========================================================================= *)
let cc_mthd_decl dcl_df anonymous invl dcl =
	cc.cc_decl <- dcl;
	match dcl.entry with
	| Method mthd ->
			( match mthd.method_kind with
				| RctMethod _ | RctNode _ | GraphicCall _ ->
						() (* are compiled to the reactive engine *)
				| Internal when dcl_df = Definition ->
						() (* no internal definitions, of course *)
				| NativeMethod _ when (dcl.name = id_main) && (decl2arity dcl = 1) ->
						() (* main function is supplied from C, no declaration,
				no, definition, especially no prototype *)
				| _ ->
						let (pre, post) =
							if dcl.access = Protected ||
							dcl.access = Private
							then ([],[])
							else if dcl.scope = Class
							then (mthd.mpre, mthd.mpost)
							else (invl@mthd.mpre, mthd.mpost@invl)
						in
						cc_method dcl_df anonymous mthd.method_kind
							mthd.mformals mthd.mbody pre post []
			)
	| _ -> ()

let cc_constr_decl dcl_df anonymous invl dcl =
	cc.cc_decl <- dcl;
	match dcl.entry with
	| Constructor constr ->
			let sl = constr.cbody @ constr.cro in
			cc_method dcl_df anonymous DataMethod constr.cformals (TextStmtL sl) [] [] constr.csig;
	| _ -> ()

let mk_object_init typ ast =
	if ast.declseq = [] then ps "inline ";
	let prt = cc_type2str (subst_map typ cc.cc_v2t) in
	let ext = cc_type2str (subst_map ast.extends cc.cc_v2t) in
	ps "pat_"; ps prt; ps " i_"; ps prt;
	ps "(pat_"; ps prt; ps " p__this)\n";
	ps "{\n";
	if ast.extends = t_object
	then ()
	else ( pT 1; ps "i_"; ps ext; ps "((pat_"; ps ext; ps ")p__this);\n" );
	pT 1; ps "p__this->header.objhead.cid = c_"; ps prt; ps ";\n";
	List.iter (bs_init 1 Single) ast.declseq;
	List.iter (bs_init 1 Instance) ast.declseq;
	pT 1; ps "return p__this;\n";
	ps "}\n\n"

let cc_allocator (typ, _, _) =
	let prt = cc_type2str typ in
	ps "#define n_"; ps prt;
	ps "() (ct=gc_new(sizeof(struct at_"; ps prt;
	ps ")),\\\n        memset (ct,sizeof (struct at_"; ps prt;
	ps "),0),(pat_"; ps prt; ps ")ct)\n"

let mk_arraylit cid =
	let arrayval2ps el = lst_sepf2ps (fun e -> ps (cc_expr e)) (fun () -> ps ",") el in
	function
	| Dim1 l ->
			let il = List.length l.av1 in
			let t = some (List.hd l.av1).etyp "mk_arraylit" in
			ps "static struct { ARRAYLEN len;"; ps (cc_type2str t); ps " a[";
			pi il; ps "]; } "; psc cid; ps "_al"; pi l.an1;
			ps " = {"; pi il; ps ",{"; arrayval2ps l.av1; ps "}};\n"
	| Dim2 l ->
			let il1 = List.length l.av2
			and il2 = List.length (List.hd l.av2) in
			let t = some ((List.hd (List.hd l.av2)).etyp) "mk_arraylit" in
			ps "static struct { ARRAYLEN len1;ARRAYLEN len2;";
			ps (cc_type2str t);
			ps " a["; pi il1; ps "]["; pi il2; ps "]; } ";
			psc cid; ps "_al"; pi l.an2; ps " = {";
			pi il1; ps ","; pi il2;
			ps ",{"; arrayval2ps (List.concat l.av2); ps "}};\n"

let cc_arraylits comment_fn (typ, _, _) =
	let cid = type2id typ in
	let al = i_arraylits cid in
	if al = []
	then (
		comment_fn
	) else (
		comment_fn (); List.iter (mk_arraylit cid) al;
		(fun () -> ())
	)

let cc_class_def (typ, _, _) =
	let cid = type2id typ in
	let ast = i_ast cid in
	if is_array_typ typ || (cid = id_string) ||
	(ast.classkind = Interface) then (
	(* ok *)
	) else (
		let v2t = prepare_subst typ in
		let sf = function Assertion _ as x -> Some x | _ -> None in
		let al = list_condense sf ast.specs in
		let cs = (classkind2str ast)^" "^(Err.type2str typ) in
		let ano =
			match ast.classkind with
			| Anonymous cid' ->
					Some (cc_type2str (Typ(cid', type2tpl typ)))
			| _ ->
					None
		in
		print_detailed_comment "/* " cs " */\n";
		( match ast.status with
			| Singleton (cid, fm) ->
					set_cc_context typ ("sgl_"^Ly.c2str cid^"_"^Ly.mf2str fm) v2t true
			| _ ->
					set_cc_context typ "p__this" v2t false
		);
		mk_object_init typ ast;
		List.iter (cc_constr_decl Definition ano al) ast.declseq;
		List.iter (cc_mthd_decl Definition ano al) ast.declseq
	)

(* -------------------------------------------------------------------------
declaration for instance fields
------------------------------------------------------------------------- *)
let cc_instance_field (fid, dcl) =
	let rt = subst_map dcl.signature.rt cc.cc_v2t in
	let cid = type2id rt in
	pT 1;
	( try
		if is_static dcl then (
		(* declared as statics *)
		) else (
			ps (cc_builtintypedef cid); ps " "; psmf fid; ps ";\n"
		)
	with Not_found ->
			let can_expand = can_expand dcl in
			if debug_level DbgField
			then (
				let ps = P.primary_ps in
				if can_expand
				then ps "\n[[FieldExpanded-"
				else ps "\n[[FieldReference-";
				ps (Ly.mf2str fid); ps "]]"
			) else ();
			if can_expand then (
				if is_array_typ rt then (
					let (l1, l2, typ) = array2size fid dcl in
					let typedef =
						try cc_builtintypedef (type2id typ)
						with Not_found -> "se_Object"
					in
					ps "struct { ";
					if cid = id_array1 || cid = id_vector then (
						ps "ARRAYLEN len;"; ps typedef;
						ps " a["; pi l1; ps "]; }"
					) else (
						ps "ARRAYLEN len1;ARRAYLEN len2;"; ps typedef;
						ps " a["; pi l1; ps "]["; pi l2; ps "]; }"
					)
				) else (
					ps "tat_"; ps (cc_type2str (get_expandtyp dcl))
				)
			) else if dcl.scope = Single && is_primitive_typ rt then (
				ps "tat_"; ps (cc_type2str rt)
			) else (
				ps "pat_"; ps (cc_type2str rt)
			);
			ps " "; psmf fid; ps ";\n"
	)

(* -------------------------------------------------------------------------
static fields of class ''classtype'' are defined here
- cc_define is for primitive constants, which are #define'd
- cc_static is for all others
------------------------------------------------------------------------- *)
let cc_define (typ, _, _) =
	let enc_cid = type2id typ in
	let sel id n dcl =
		if (
			is_field dcl && dcl.origin = enc_cid && native2fromC dcl = None &&
			( match dcl.scope with
				| Class -> true
				| Single -> not (is_primitive_typ dcl.signature.rt)
				| Instance -> false
			)
		)
		then ( Some dcl )
		else ( None )
	in
	let sff = i_fromsymtab sel enc_cid in
	
	let cc_define dcl =
		let fid = dcl.name in
		let rtyp = dcl.signature.rt in
		if dcl.final && is_primitive_typ rtyp then (
			try
				let e = get_initexpr dcl in
				ps "#define ";
				( match native2toC dcl with
					| None -> ps "sta_"; psc dcl.origin; ps "_"; psmf fid
					| Some n -> ps n
				);
				cc.cc_typ <- typ;
				ps " "; ps (cc_expr e); pn();
				if debug_level DbgField
				then (
					let ps = P.primary_ps in
					ps "\n[[FieldStaticFinal-";
					ps (mf2str fid); ps "]]"
				) else ()
			with
			| Error e ->
					raise (Error e)
			| _ ->
					Err.intern "cc_define"
		) else (
		(* static native & static final of class type: see cc_static *)
		)
	in
	List.iter cc_define sff

let cc_static comment_fn (typ, _, _) =
	let enc_cid = type2id typ in
	let sel _ _ dcl =
		if ( is_field dcl && dcl.origin = enc_cid &&
			( match dcl.scope with
				| Class
				| Single -> true
				| Instance -> false
			)
		)
		then ( Some dcl )
		else ( None )
	in
	let sff = i_fromsymtab sel enc_cid in
	
	let cc_static comment_fn dcl =
		let fid = dcl.name in
		let typ = dcl.signature.rt in
		let cid = type2id typ in
		let native_fromC = native2fromC dcl in
		let native_toC = native2toC dcl in
		if dcl.final && is_primitive_typ typ then (
			(* own static final of primitive type are #define'd in cc_define
			static final fields of primitive type defined elsewhere in C
			need not to be declared
			*)
			comment_fn
		) else (
			comment_fn ();
			if dcl.volatile then (
				ps "volatile "
			) else (
			(* nothing to do *)
			);
			( match native_fromC, native_toC with
				| None , None -> ps "static "
				| Some _, None -> ps "extern "
				| None , Some _ -> ()
				| _ -> Err.intern "cc_static"
			);
			( try
				ps (cc_builtintypedef cid)
			with Not_found ->
					let can_expand = can_expand dcl in
					if debug_level DbgField
					then ( let ps = P.primary_ps in
						if can_expand
						then ps "\n[[StaticFieldExpanded-"
						else ps "\n[[StaticFieldReference-";
						ps (mf2str fid); ps "]]" )
					else ();
					if can_expand then (
						if is_array_typ typ || is_matrix_typ typ then (
							let (l1, l2, typ) = array2size fid dcl in
							let typedef = try cc_builtintypedef (type2id typ)
								with Not_found -> "OBJECT" in
							ps "struct { ";
							if cid = id_array1 || cid = id_vector then (
								ps "ARRAYLEN len;"; ps typedef;
								ps " a["; pi l1; ps "]; } "
							) else (
								ps "ARRAYLEN len1;ARRAYLEN len2;"; ps typedef; ps " a["; pi l1; ps "]["; pi l2 ; ps "]; } "
							)
						) else (
							ps "tat_"; ps (cc_type2str (get_expandtyp dcl)) )
					) else (
						ps "pat_"; psc cid
					);
			);
			( match native_fromC, native_toC with
				| None , None ->
						if dcl.scope = Class then ps " sta_" else ps " sgl_";
						psc dcl.origin; ps "_"; psmf fid
				| Some n, None ->
						ps " "; ps n
				| None , Some n ->
						ps " "; ps n
				| _ ->
						Err.intern "cc_static"
			);
			ps ";\n";
			(fun () -> ())
		)
	in
	List.fold_left cc_static comment_fn sff

(* -------------------------------------------------------------------------
header file information for a class
- typedef's, decl of new & init fun, #define of class - id
------------------------------------------------------------------------- *)
let cc_class_typedef (typ, c, fields) =
	let cs = cc_type2str typ in
	let co = "typedef, decl, class-id for class "^(Err.type2str typ) in
	print_comment co;
	ps "typedef struct at_"; ps cs; ps " tat_"; ps cs;
	ps ", *pat_"; ps cs; ps ";\n";
	ps "pat_"; ps cs; ps " i_"; ps cs; ps " (pat_"; ps cs; ps ");\n";
	ps "#define c_"; ps cs; ps " "; pi c; pn()

(* -------------------------------------------------------------------------
header file information for a class
1. group: struct containg instance field pointer or structs
2. group: method headers
------------------------------------------------------------------------- *)
let cc_class_header (typ, c, fields) =
	let v2t = prepare_subst typ in
	let cid = type2id typ in
	let ast = i_ast cid in
	let co = "field-struct, method header for class "^(Err.type2str typ) in
	print_comment co;
	( match ast.status with
		| _ ->
				let sel dcl = dcl.scope = Instance in  (* 1 *)
				let fieldl = extract_fields (type2id typ) sel fields in
				ps "struct at_"; ps (cc_type2str typ);
				ps "\n{\n  tat_object header;\n";
				set_cc_context typ "p__this" v2t false;
				( match ast.classkind with
					| Anonymous cid ->
							ps " pat_";
							ps (cc_type2str (Typ(cid, type2tpl typ)));
							ps " this__anon;\n"
					| _ -> ()
				);
				List.iter cc_instance_field fieldl;
				ps "};\n\n"
	);
	if is_array_typ typ || (cid = id_string) then (
	(* nothing to do *)
	) else (
		List.iter (cc_mthd_decl Declaration None []) ast.declseq;
		List.iter (cc_constr_decl Declaration None []) ast.declseq
	)

(* =========================================================================
mk_c: generate C - code for an application
========================================================================= *)
let mk_c () =
	if Gen_application.valid ()
	then ()
	else Err.msg (Err.NoCode);
	
	Hashtbl.clear cc.cc_dynbind;
	reset_tmpvar ();
	
	let conf_cid = type2id se.conf_class in
	let conf_str = c2str conf_cid in
	(* MAKE THE HEADER FILE    .h    --------------------------------------- *)
	( let h_file = se.file_prefix^conf_str^".h" in
		let oc = open_out h_file in
		push_print_fct (output_string oc);
		try
			ps "#ifndef _se_configuration_h\n";
			ps "#define _se_configuration_h\n\n";
			ps "#include <se_def.h>\n";
			ps "#include <se_rt.h>\n\n";
			if se.target_sys = Scicos then (
				let r = Filename.concat se.scilab_dir "routines" in
				ps "#include <";
				ps (filename_concat [r;"scicos";"scicos_block.h"]);
				ps ">\n";
				ps "#include <"; ps (Filename.concat r "machine.h");
				ps ">\n\n";
			);
			
			let (t, s, v) = field2literal conf_cid id_timing in
			let v = int64_2_int (some v "grpiC:tval") in
			if se.target_sys = Simulation then (
				ps "extern void *sim_reader (void *);\n\n"
			);
			ps "\nextern se_Time __deltat;\n\n";
			if v = 0 then (
				ps "#define TIMING __deltat\n\n"
			) else (
				ps "#define TIMING (se_Time)"; pi v; ps "\n\n"
			);
			ps (runtime_engine Header "init");
			ps (runtime_engine Header "instant");
			List.iter cc_class_typedef ac.ac_classes;
			List.iter cc_class_header (List.rev ac.ac_classes);
			print_comment "static fields of primitive type for all classes";
			List.iter cc_define ac.ac_classes;
			if ac.ac_gc = [] && ac.ac_dyn = [] then (
				print_comment "no dynamic creation of objects required"
			) else (
				print_comment "memory allocator for new objects";
				ps "void gc_init (void *,int,int[]);\n";
				ps "void *gc_new (int len);\n";
				ps "void gc_status (void);\n";
				List.iter cc_allocator ac.ac_classes;
				Err.warn (Err.DynamicObjectGeneration)
			);
			print_comment "defines related to arrays";
			let nll = i2s (get_sysprop se.conf_class id_exc_nullptr) in
			ps "#define check_pointer(ptr,lbl)\\";
			pnT(3); ps ("(ptr==Null && (mkExc("^nll^",lbl),0))\n");
			let bds = i2s (get_sysprop se.conf_class id_exc_bounds) in
			ps "#define check_index(ptr,i,lbl)\\";
			pnT(3); ps ("((i < 0 ||i >= ((pat_array1)ptr)->len) && (mkExc("
					^bds^",lbl),0))\n");
			ps "#define check_indices(ptr,i,j,lbl)\\";
			pnT(3); ps ("((i < 0 || j < 0 \
					|| i >= ((pat_array2)ptr) -> len1 \
					|| j >= ((pat_array2)ptr) -> len2) \
					&& (mkExc("^bds^", lbl), 0))\n");
			List.iter (fun f -> ps "\n#include \""; ps f; ps "\"") se.hfiles;
			ps "\n\n#endif /* _se_configuration_h */\n";
			pop_print_fct ();
			close_out oc
		with x ->
				pop_print_fct ();
				close_out oc;
				raise x
	);
	(* MAKE THE SOURCE FILE    .c    --------------------------------------- *)
	( let c_file = se.file_prefix^conf_str^".c" in
		let oc = open_out c_file in
		push_print_fct (output_string oc);
		try
			print_detailed_comment "/* " "tree of reactive objects" "\n";
			lst_sepf2ps print_ro_tree pn ac.ac_rctobj_tree;
			print_detailed_comment "   " "end of tree" " */\n";
			if se.target_sys = Simulink then (
				ps "#define S_FUNCTION_NAME "; ps conf_str; pn ();
				ps "#define S_FUNCTION_LEVEL 2\n";
				ps "#if !defined(MATLAB_MEX_FILE)\n";
				ps "# error This_file_can_be_used_only\
				_during_simulation_inside_Simulink\n";
				ps "#endif\n";
				ps "#include \"simstruc.h\"\n\n";
			);
			ps "#include \""; ps (se.file_prefix^conf_str^".h"); ps "\"\n";
			( let f () = print_comment "all static fields [class type]" in
				let _ = List.fold_left cc_static f ac.ac_classes in
				()
			);
			if gen_for_targetsys () then (
				List.iter cc_callback_signal ac.ac_classes
			) else (
			(* callbacks only in target systems ... *)
			);
			print_comment "global data for the run time system";
			ps "static tat_"; ps conf_str; ps " conf_obj;\n\n";
			ps "pat_object get_configuration (void) ";
			ps "{\n  return (pat_object) &conf_obj;\n}\n\n";
			ps "pat_object ct;\n";
			gen_tmpvar ();
			
			if se.target_sys = Simulation then (
				ps "seThread_t sim_rdr;\n\n";
			) else (
			);
			
			ps "SE_EXCEPTION_DEF\n";
			( let f () = print_comment "definition of array literals" in
				let _ = List.fold_left cc_arraylits f ac.ac_classes in
				()
			);
			pn();
			(* must generate a method ..._instant for Sfunctions
			the is composed from the ..._output and ..._update
			functions needed
			*)
			List.iter cc_class_def ac.ac_classes;
			( match se.target_sys with
				| Simulation
				| Host
				| Makefile
				| Platform _
				-> gen_reactive_part_for_c conf_cid ac.ac_sca
				| Simulink
				-> gen_reactive_part_for_simulink conf_cid ac.ac_sca
				| Scicos
				-> gen_reactive_part_for_scicos conf_cid ac.ac_sca
				| Verification
				| VerilogSimulation | Verilog _
				-> Err.intern "mk_c:rct"
			);
			
			if se.target_sys = Scicos then (
				ps "\nvoid se_"; ps conf_str;
				ps "(scicos_block *block,int flag)\n{\n";
				ps "  if (flag == 4)\n  { /* initialization */\n";
				ps "    "; ps (runtime_engine Appl "init");
				ps " } else if (flag == 1)\n { \
				/* output computation */\n";
				ps "    "; ps (runtime_engine Appl "instant");
				ps " } else if (flag == 5)\n { \
				/* termination */\n";
				ps "  };\n}\n";
				Err.warn Err.MainIgnored
			) else (
				print_comment "external interface";
				ps "inline void runtime_engine_init (void)\n";
				ps "{\n";
				pT 1; ps (runtime_engine Appl "init");
				pT 1; ps "return;\n}\n\n";
				ps "inline int runtime_engine_instant ()\n";
				ps "{\n";
				pT 1; ps "int exc = "; ps (runtime_engine Appl "instant");
				pT 1; ps "return exc;\n";
				ps "}\n\n";
				if se.target_sys = Simulation then (
					ps "int main (int argc,char *argv[])\n";
					ps "{\n";
					pT 1; ps "runtime_engine_init();\n";
					pT 1; ps "while (1)\n";
					pT 1; ps "{\n";
					pT 2; ps "runtime_engine_instant();\n";
					pT 1; ps "};\n";
					pT 1; ps "return 0;\n";
					ps "}\n"
				)
			);
			pop_print_fct ();
			close_out oc
		with
		| Error(x) ->
				pop_print_fct ();
				close_out oc;
				raise (Error x)
		| x ->
				pop_print_fct ();
				close_out oc;
				let s = Printexc.to_string x in
				let s = "mk_c: could not handle exception: "^s in
				Err.intern s
	);
	if se.target_sys = Simulink || se.target_sys = Scicos then (
		ps "\nparameter positions";
		List.iter
			( fun (count, dcl) ->
						ps "\n    "; pi count; ps " : "; ps (mf2str dcl.name)
			) !parameters;
		let sa = some ac.ac_sca "signal positons" in
		ps "\ninput signal positions";
		List.iter
			( fun xs ->
						if is_input_sig xs then (
							ps "\n    "; pi xs.simulinkno; ps " : ";
							ps (sig2str xs)
						)
			) sa.sa_sigs;
		ps "\noutput signal positions";
		List.iter
			( fun xs ->
						if is_output_sig xs then (
							ps "\n    "; pi xs.simulinkno; ps " : ";
							ps (sig2str xs)
						)
			) sa.sa_sigs;
	);
	pn()

(* MAKE THE SCICOS INTERFACE FUNCTION    .sci    -------------------------- *)
let rec lit2scicos_str = function
	| Literal(_, s, _) ->
			s
	| ArrayLiteral (Dim1 l) ->
			arr2scicos_str l.av1
	| ArrayLiteral (Dim2 l) ->
			"["^(lst_sep2str arr2scicos_str ";" l.av2)^"]"
	| _ ->
			Err.intern "lit2scicos_str"
and arr2scicos_str el =
	"["^(lst_sep2str (fun e -> lit2scicos_str e.expr) "," el)^"]"

let dcl2scicos_init (_, dcl) =
	match dcl.entry with
	| Field f ->
			( match f.init with
				| None -> Err.intern "dcl2scicos_init:1"
				(* parameters are always initialised *)
				| Some init -> lit2scicos_str init.expr
			)
	| _ ->
			Err.intern "dcl2scicos_init:2"

let mk_scicos_interface () =
	let conf_cid = type2id se.conf_class in
	let conf_str = c2str conf_cid in
	if Gen_application.valid ()
	then ()
	else Err.msg (Err.NoCode);
	let sci_file = conf_str^".sci" in
	let oc = open_out sci_file in
	push_print_fct (output_string oc);
	try
		let sa = some ac.ac_sca "mk_scicos_interface" in
		let inputs = List.filter is_input_sig sa.sa_sigs in
		let outputs = List.filter is_output_sig sa.sa_sigs in
		
		let dim2str xs =
			let typ = sig2valtyp xs in
			match typ with
			| Null ->
					"1"
			| Simple id when id = id_double ->
					"1"
			| Array(_, Arbitrary, _)
			| Array(_, _, Arbitrary) ->
					Err.msg (Err.ScicosIO(sig2str xs, Err.type2str typ))
			| Array(t, DimLen 1, d1)
			| Array(t, d1, DimLen 1) when t = t_double ->
					i2s (dim2value d1)
			| _ ->
					Err.msg (Err.ScicosIO(sig2str xs, Err.type2str typ)) in
		let sig_in = lst_sep2str dim2str ";" inputs in
		let sig_out = lst_sep2str dim2str ";" outputs in
		
		let (t, s, v) = field2literal conf_cid id_timing in
		let v = int64_2_int (some v "grpiC:tval") in
		if t != t_time then Err.intern "grpiC:time";
		let evt_in = if v = 0 then "" else "1" in
		let feedthroughs_in, _ =
			List.partition
				( fun xs -> List.mem (sig2sgn xs) sa.sa_obsv
				) inputs in
		let feedthr = if feedthroughs_in = [] then "%f" else "%t" in
		let time_dep = if inputs = [] && v = 0 then "%t" else "%f" in
		let check_par (_, dcl) =
			match dcl.signature.rt with
			| Simple t when t = id_double || t = id_int ->
					()
			| Array(t, DimLen 1, DimLen n1)
			| Array(t, DimLen n1, DimLen 1) when t = t_double || t = t_int ->
					()
			| Array(t, DimLen n1, DimLen n2) when t = t_double || t = t_int ->
					()
			| t ->
					Err.msg(Err.ScicosPar(mf2str dcl.name, Err.type2str t))
		in
		let dcl2name (_, dcl) = "\""^mf2str dcl.name^"\"" in
		let dcl2scicos_typ (_, dcl) =
			match dcl.signature.rt with
			| Simple _ ->
					"           \"vect\",1"
			| Array(_, DimLen 1, DimLen n1)
			| Array(_, DimLen n1, DimLen 1) ->
					"           \"vec\","^i2s n1
			| Array(_, DimLen n1, DimLen n2) ->
					"           \"mat\",["^i2s n1^","^i2s n2^"]"
			| t ->
					Err.msg (Err.ScicosPar(mf2str dcl.name, Err.type2str t))
		in
		let rpar indent (count, dcl) =
			match dcl.signature.rt with
			| Simple t when t = id_double ->
					ps indent; ps "rpar=[rpar p"; ps (i2s count); ps "]\n"
			| Array(t, DimLen 1, DimLen n1)
			| Array(t, DimLen n1, DimLen 1) when t = t_double ->
					ps indent; ps "rpar=[rpar p"; ps (i2s count); ps "]\n"
			| Array(t, DimLen n1, DimLen n2) when t = t_double ->
					ps indent; ps "for i=1:"; ps (i2s n1); ps "\n";
					ps indent; ps "  rpar=[rpar p";
					ps (i2s count); ps "(i,:)]\n";
					ps indent; ps "end\n"
			| t -> ()
		in
		let ipar indent (count, dcl) =
			match dcl.signature.rt with
			| Simple t when t = id_int ->
					ps indent; ps "ipar=[ipar p"; ps (i2s count); ps "]\n"
			| Array(t, DimLen 1, DimLen n1)
			| Array(t, DimLen n1, DimLen 1) when t = t_int ->
					ps indent; ps "ipar=[ipar p"; ps (i2s count); ps "]\n"
			| Array(t, DimLen n1, DimLen n2) when t = t_int ->
					ps indent; ps "for i=1:"; ps (i2s n1); ps "\n";
					ps indent; ps "  ipar=[ipar p";
					ps (i2s count); ps "(i,:)]\n";
					ps indent; ps "end\n"
			| t -> ()
		in
		let fpar_init (count, dcl) =
			ps ("  p"^i2s count^"="^dcl2scicos_init (count, dcl)^"\n") in
		let fpar2str (count, _) = "strcat(sci2exp(p"^i2s count^"))" in
		
		ps "function [x,y,typ]="; ps conf_str; ps "(job,arg1,arg2)\n";
		ps "x=[];y=[];typ=[];\n";
		ps "select job\n";
		ps "case 'plot' then\n";
		ps "  standard_draw(arg1)\n";
		ps "case 'getinputs' then\n";
		ps "  [x,y,typ]=standard_inputs(arg1)\n";
		ps "case 'getoutputs' then\n";
		ps "  [x,y,typ]=standard_outputs(arg1)\n";
		ps "case 'getorigin' then\n";
		ps "  [x,y]=standard_origin(arg1)\n";
		mk_parameters();
		if !parameters <> [] then (
			ps "case 'set' then\n";
			ps "  x=arg1\n";
			ps "  graphics=arg1.graphics;exprs=graphics.exprs\n";
			ps "  model=arg1.model;\n";
			ps "  while %t do\n";
			(* compute the parameters *)
			List.iter check_par !parameters;
			ps "    [ok,";
			let fpar (count, _) = "p"^i2s count in
			ps (lst_sep2str fpar "," !parameters);
			ps ",exprs]=..\n";
			ps "      getvalue(..\n";
			ps "        \"Set block parameters\",..\n";
			ps "        [";
			ps (lst_sep2str dcl2name ";..\n         " !parameters);
			ps "],..\n";
			ps "         list(..\n";
			ps (lst_sep2str dcl2scicos_typ ",..\n" !parameters);
			ps "),..\n";
			ps "         exprs..\n";
			ps "      )\n";
			ps "    if ~ok then break,end\n";
			List.iter (rpar "    ") !parameters;
			ps "    ipar=[]\n";
			List.iter (ipar "    ") !parameters;
			ps "    if ok then\n";
			ps "      model.rpar=rpar'\n";
			ps "      model.ipar=ipar'\n";
			ps "      x.model=model\n";
			ps "      graphics.exprs=exprs\n";
			ps "      x.graphics=graphics\n";
			ps "      break\n";
			ps "    end\n";
			ps "  end\n";
		);
		
		ps "case 'define' then\n";
		ps "  rpar=[]\n";
		ps "  ipar=[]\n";
		if !parameters <> [] then (
			List.iter fpar_init !parameters;
			List.iter (rpar "  ") !parameters;
			List.iter (ipar "  ") !parameters;
		);
		ps "  model=scicos_model()\n";
		ps "  model.sim=list('se_"; ps conf_str; ps "',4)\n";
		ps "  model.in=["; ps sig_in; ps "]\n";
		ps "  model.out=["; ps sig_out; ps "]\n";
		ps "  model.evtin=["; ps evt_in; ps "]\n";
		ps "  model.evtout=[]\n";
		ps "  model.rpar=rpar'\n";
		ps "  model.ipar=ipar'\n";
		ps "  model.blocktype='c'\n";
		ps "  model.firing=[]\n";
		ps "  model.dep_ut=["; ps feedthr; ps ","; ps time_dep; ps "]\n";
		ps "  model.nzcross=0\n";
		ps "  exprs=[";
		ps (lst_sep2str fpar2str ";\n         " !parameters);
		ps "\n        ]\n";
		ps "  gr_i=['xstringb(orig(1),orig(2),''"; ps conf_str;
		ps "'',sz(1),sz(2),''fill'');']\n";
		let width = String.length conf_str / 3 in
		let width = if width < 2 then 2 else
			if width > 8 then 8 else
				width in
		ps "  x=standard_define(["; pi width; ps " 2],model,exprs,gr_i)\n";
		ps "end\n";
		ps "endfunction\n";
		
		pop_print_fct ();
		close_out oc
	with
	| Error(x) ->
			pop_print_fct ();
			close_out oc;
			raise (Error x)
	| x ->
			pop_print_fct ();
			close_out oc;
			let s = Printexc.to_string x in
			let s = "mk_scicos_interface: "^s in
			Err.intern s

let scicos_generate_Makelib () =
	let conf_cid = type2id se.conf_class in
	let conf_str = c2str conf_cid in
	if Gen_application.valid ()
	then ()
	else Err.msg (Err.NoCode);
	let sci_file = "se.Makelib"^if Sys.os_type ="Win32" then ".mak" else "" in
	let tgt = if Sys.os_type = "Win32" then "win32" else "unix" in
	let incl = filename_concat ["$(SE_HOME)";"include"] in
	let tgt_incl = filename_concat ["$(SE_HOME)";"target"; tgt;"include"] in
	let se_rt = "libse_rt."^if Sys.os_type = "Win32" then "lib" else "so" in
	let oth_libs = filename_concat ["$(SE_HOME)";"target"; tgt;"lib"; se_rt] in
	let rout = filename_concat [se.scilab_dir;"routines"] in
	
	let oc = open_out sci_file in
	push_print_fct (output_string oc);
	
	try
		ps "# generated: Please do not edit this file\n";
		ps "# ---------------------------------------\n";
		ps "SCIDIR = "; ps se.scilab_dir; ps "\n";
		ps "SCIDIR1 = "; ps se.scilab_dir; ps "\n";
		if Sys.os_type = "Win32" then (
			ps "OBJS =  "; ps (se.file_prefix^conf_str); ps ".obj";
			List.iter
				( fun f ->
							let f = mk_path_absolute f in
							ps" "; ps(Filename.chop_extension f); ps ".obj"
				) se.cfiles;
			ps "\n";
			ps "OTHERLIBS =\""; ps oth_libs; ps "\" $(CCLIBS)\n";
			ps "LIBRARY = lib"; ps conf_str; ps "\n";
			ps "CFLAGS = -Dse_sim  -Dse__win32  -I \""; ps incl;
			ps "\" -I \""; ps tgt_incl;
			ps "\" $(CC_OPTIONS) -DFORDLL -I \""; ps rout;
			ps "\" -Dmexfunction_=mex$*_ -DmexFunction=mex_§*\n";
			ps "FFLAGS =\n";
			ps "!include ";
			ps (Filename.concat se.scilab_dir "Makefile.incl.mak");
			ps "\n";
			ps "!include ";
			ps (filename_concat [se.scilab_dir;"config";"Makedll.incl"])
			
		) else (
			ps "OBJS =  "; ps (se.file_prefix^conf_str); ps ".o";
			List.iter
				( fun f ->
							let f = mk_path_absolute f in
							ps" "; ps(Filename.chop_extension f); ps ".o"
				) se.cfiles;
			ps "\n";
			ps "OTHERLIBS =\""; ps oth_libs; ps"\" $(CCLIBS)\n";
			ps "LIBRARY = lib"; ps conf_str; ps "\n";
			ps "CFLAGS = -Dse_sim  -Dse__unix  -I \""; ps incl;
			ps "\" -I \""; ps tgt_incl; ps "\"\n";
			ps "FFLAGS =\n";
			ps "include "; ps (Filename.concat "$(SCIDIR)" "Makefile.incl");
			ps "\n";
			ps "include  "; ps (Filename.concat "$(SCIDIR)"
						(Filename.concat "config" "Makeso.incl"));
			ps "\n"
		);
		
		pop_print_fct ();
		close_out oc
	with
	| Error(x) ->
			pop_print_fct ();
			close_out oc;
			raise (Error x)
	| x ->
			pop_print_fct ();
			close_out oc;
			let s = Printexc.to_string x in
			let s = "scicos_generate_Makelib: "^s in
			Err.intern s

let scicos_generate_loader () =
	let conf_cid = type2id se.conf_class in
	let conf_str = c2str conf_cid in
	if Gen_application.valid ()
	then ()
	else Err.msg (Err.NoCode);
	let sci_file = "loader.sce" in
	let oc = open_out sci_file in
	push_print_fct (output_string oc);
	try
	
		ps "// generated : Please do not edit this file  \n";
		ps "// -------------------------------------------- \n";
		ps conf_str; ps "_path=get_absolute_file_path('loader.sce');\n";
		ps "link("; ps conf_str; ps "_path+'lib"; ps conf_str; ps ".";
		ps (if Sys.os_type = "Win32" then "dll" else "so");
		ps "',['se_"; ps conf_str; ps "'],'c');\n";
		ps "scicos ";
		if Sys.file_exists se.scicos_model then (
			ps (mk_path_absolute se.scicos_model)
		);
		ps ";\n";
		
		pop_print_fct ();
		close_out oc
	with
	| Error(x) ->
			pop_print_fct ();
			close_out oc;
			raise (Error x)
	| x ->
			pop_print_fct ();
			close_out oc;
			let s = Printexc.to_string x in
			let s = "mk_c: could not handle exception: "^s in
			Err.intern s

let scicos_generate_rcfile () =
	let conf_cid = type2id se.conf_class in
	let conf_str = c2str conf_cid in
	if Gen_application.valid ()
	then ()
	else Err.msg (Err.NoCode);
	let sci_file = ".scilab" in
	let oc = open_out sci_file in
	push_print_fct (output_string oc);
	try
		ps "genlib('lib"; ps conf_str;
		ps "',pwd());\nexec('loader.sce');\n";
		
		pop_print_fct ();
		close_out oc
	with
	| Error(x) ->
			pop_print_fct ();
			close_out oc;
			raise (Error x)
	| x ->
			pop_print_fct ();
			close_out oc;
			let s = Printexc.to_string x in
			let s = "mk_c: could not handle exception: "^s in
			Err.intern s
