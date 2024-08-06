open Ly
open P
open Ast
open Util
open Util_print
open Util_parse

let status = new observers

(* -------------------------------------------------------------------------
top - level function for inspection and debug
------------------------------------------------------------------------- *)
let pu_sta () =
  let stn cid =
    let prt k s = ps (Printf.sprintf "%15s %s%s\n" (c2str cid) k s) in
    let ast = i_ast cid in
    let knd = classkind2str ast^" from "^ast.src_file in
      match ast.class_sta.class_status with
      | Synchron -> prt knd ""
      | NoUpdate -> () (* ignore builtin classes *)
      | _ -> prt knd ", but not yet type-checked"
  in
  let cl = ref [] in
    Hashtbl.iter (fun c _ -> cl := c :: !cl) se.classtab;
    pn ();
    List.iter stn (Sort.list (fun a b -> (c2str a) < (c2str b)) !cl);
    pn();
    ps "selected configuration class: "; psc (type2id se.conf_class);
    pn()

(* -------------------------------------------------------------------------
promote classes from Parsed to TypeChecked, then produce the syA
------------------------------------------------------------------------- *)
let process_classes () =
  let class_status_eq cid x = class_status cid (=) x
  and iter f = Hashtbl.iter f se.classtab in
    try
        Inherit_check.circularity ();
        iter (fun cid _ -> if (class_status_eq cid Parsed) then Inherit_check.make cid);
        iter (fun cid _ -> if (class_status_eq cid InheritChecked) then Typecheck.make cid);
        iter (fun cid _ -> if (class_status_eq cid TypeChecked) then React2sc.mk_rctclass cid);
        ()
    with
    | Error(t) ->
        pn (); ps t; pF ()
    | e ->
        ps "[[InternalError]] process_classes: ";
        ps (Printexc.to_string e); pn (); pF ()

(* -------------------------------------------------------------------------
set the configuration class [explicit command]
------------------------------------------------------------------------- *)
let set_conf_class conf_option =
  ( match conf_option with
    | None ->
        process_classes ()
    | Some conf ->
        try
            let cid = Lex_wrapper.parse_string Yacc.prs_classid conf in
            let cst = c2str cid in
              se.conf_class <- Typ(cid,[]); status#has_changed ();
              ( try
                  let conf_ast = i_ast cid in
                    if not (conf_ast.classkind = ConfClass) then (
                      ps "\nclass "; ps cst;
                      ps " is no configuration \
                      class, declaring it for system configuration \
                      is illegal"
                    ) else (
                      ps "\nconfiguration class is "; ps cst; pn()
                    );
                    process_classes ()
              with Not_found ->
                  ps "\nunknown class "; ps cst;
                  ps " cannot be used for system configuration"
              )
        with _ ->
            ps "\nusing string \""; ps conf;
            ps "\" to declare a class to configure an application failed\n"
  );
  pF ()

(* -------------------------------------------------------------------------
if only one configuration class is known in the database base,
then set this class to THE configuration class
The typechecking of the application relative to the configuration class
is done here also,
- either by explicit call of process_classes
- or implicitly from set_conf_class
------------------------------------------------------------------------- *)
let force_process_classes () =
  let ccid = type2id se.conf_class in
  let cast = i_ast ccid in
    if cast.classkind = ConfClass && (class_status ccid (=) Synchron) then (
      ps "\nclass "; ps (c2str ccid);
      ps " is a valid configuration class\n"; pF ()
    ) else (
      Hashtbl.iter (fun c _ -> decrease_to_parsed c) se.classtab;
      match hashtbl_condense (fun c a -> if a.classkind = ConfClass
              then Some c else None
        ) se.classtab with
      | [cid] -> set_conf_class (Some (c2str cid))
      | _ -> process_classes ()
    )

(* -------------------------------------------------------------------------
check for static final ints that describe system characteristics
------------------------------------------------------------------------- *)
let check_sysprop name lit builtin =
  let (typ, _, v) = lit in
    try
        match Hashtbl.find s.p_symbol_table (name,- 1) with
        | dcl:: _ ->
            if builtin then (
              prs_err ("the exception '"^(mf2str name)^"' \
                  cannot be redefined")
            ) else if dcl.signature.rt = typ &
            dcl.signature.pt = None &
            dcl.scope = Class &
            dcl.final then (
            (* ok *)
            ) else prs_err ("configuration class expects a system \
                  property '"^(mf2str name)^"' as static \
                  final time field. Invalid use of the \
                  system property name detected")
        | _ -> Err.intern "check_sysprop"
    with Not_found ->
        let ini = { etyp = Some typ; expr = Literal lit; elbl = Ly.nolbl } in
        let fld = { init = Some ini; fldsrcp = Ly.nolbl; fldkind = DataField;
          assigns =[]; reads =[]} in
        let dcl = { name = name; origin = s.p_class_id; final = true;
          scope = Class; volatile = false; parameter = false;
          entry = Field fld; signature ={ rt = typ; pt = None }; access = Private } in
          mkp_st_entry dcl

(* -------------------------------------------------------------------------
generates the ast of a class
makes simple consistency checks
------------------------------------------------------------------------- *)
let mk_ast srcpos =
  let cid = s.p_class_id in
    if s.p_class_kind = Some RctClass then (
      try                          (* check for parameterless constructor *)
          match Hashtbl.find s.p_symbol_table (Ly.c2mf cid,- 2) with
          | [dcl] ->
              if dcl.signature.rt = Null &
              dcl.signature.pt = Some [] &
              dcl.scope = Instance &
              dcl.final = true &
              ( match dcl.access with
                | Public _ -> true
                | _ -> false )
              then enforce_confclass ("a configuration class has one \
                    and only one constructor, \
                    namely \"public "^(c2str cid)^" ()\" to \
                    initialize the configuration object")
              else ()
          | _ -> ()
      with Not_found ->
          Err.intern "mk_ast"
    ) else ();
    let clk =
      match s.p_class_kind with
      | None -> EffectiveClass
      | Some clk -> clk
    in
    
      if is_reactive_classkind clk then (
        List.iter Graphic_parse.mk_graphic_parse s.p_graphic_stms
      );
      if clk = Interface then (
        try
            let _ = Hashtbl.find s.p_symbol_table (Ly.c2mf cid,- 2) in
              Err.msg (Err.ConstructorError(cid,"no "))
        with Not_found ->
            () (* only NO constructor legal *)
      ) else (
        try
            let cl = Hashtbl.find s.p_symbol_table (Ly.c2mf cid,- 2) in
            let tsort = Tsort.init 20 in
              acyclic_constr tsort cid (not( s.p_extends = t_object )) cl;
              ( match Tsort.sort tsort with
                | Tsort.Cycle c -> Err.msg (Err.ConstructorCycle(cid, c))
                | Tsort.Sorted _ -> ()
                | _ -> Err.intern "mk_ast"
              )
        with Not_found ->
            Err.msg (Err.ConstructorError(cid,"at least one "))
      );
      if clk = RctClass then (
        (* no interface may be implemented,
        ACTUAL RESTRICTION: no reactive class may be extended *)
        if s.p_implements = []
        then ()
        else prs_err "reactive classes may not implement interfaces";
        if s.p_extends = t_object
        then ()
        else if is_reactive_class (type2id s.p_extends)
        then prs_err "reactive classes may not extend reactive \
        classes. If this is a annoying restriction, \
        please contact the language implementors"
        else ();
        (* only one constructor -> only one active *)
        try
            match Hashtbl.find s.p_symbol_table (Ly.c2mf cid,- 2) with
            | [dcl] ->
                if dcl.signature.rt = Null &
                dcl.scope = Instance &
                dcl.final = true &
                ( match dcl.access with
                  | Public _ -> true
                  | _ -> false )
                then ()
                else raise Not_found
            | _ -> raise Not_found
        with Not_found ->
            prs_err ("reactive classes may have only one constructor, \
                namely \"public "^(c2str cid)^" (...)\"")
      ) else ();
      if clk = ConfClass then (
        check_sysprop id_timing (t_time,"0sec", Some (Int64.zero)) false;
        check_sysprop id_exc_longjmp (t_int,"1", Some (Int64.of_int 1)) true;
        check_sysprop id_exc_nullptr (t_int,"2", Some (Int64.of_int 2)) false;
        check_sysprop id_exc_bounds (t_int,"3", Some (Int64.of_int 3)) false;
        check_sysprop id_exc_arraysize (t_int,"4", Some (Int64.of_int 4)) false;
        check_sysprop id_exc_memory (t_int,"5", Some (Int64.of_int 5)) false;
        check_sysprop id_exc_instant (t_int,"6", Some (Int64.of_int 6)) false;
        check_sysprop id_exc_timestamp (t_int,"7", Some (Int64.of_int 7)) false;
        check_sysprop id_exc_cast (t_int,"8", Some (Int64.of_int 8)) false;
        check_sysprop id_exc_deltat (t_int,"9", Some (Int64.of_int 9)) false;
      ) else ();
      if clk = Interface && s.p_specs != []
      then prs_err "interfaces may not have specifications"
      else if not (s.p_typparams = []) && (clk = ConfClass)
      then prs_err ("configuration class "^(c2str cid)^
            " may not have type parameter")
      else ();
      (* remove special entry with list of all constructors *)
      Hashtbl.remove s.p_symbol_table (Ly.c2mf cid,- 2);
      (* put all anonymous classes generated into the classtab *)
      let lbl2sp = Ly.clr_and_return_lbls () in
      let put fn = let ast = fn lbl2sp in
          if (class_status ast.classid (=) Unknown) then (
            Hashtbl.add se.classtab ast.classid ast
          ) else (
            Err.intern "mk_ast"
          )
      in
        List.iter put s.p_anonseq;
        (* return the ast of the class *)
        { classkind = clk;
          classid = cid;
          src_file = s.p_text_file;
          src_pos = srcpos;
          typparams = List.rev s.p_typparams;
          extends = s.p_extends; implements = s.p_implements;
          specs = s.p_specs;
          axioms = s.p_axioms;
          props = s.p_props;
          symtab = s.p_symbol_table;
          declseq = List.rev s.p_declseq;
          lbl2sp = lbl2sp;
          class_sta ={ class_status = Parsed;
            methods_called =[]; classes_created =[];
            classes_dyn_created =[];
            class_methods_called =[]; array_lits =[];
            class_behavior = None; typ_constrl =[];
          };
          status = NotTouched;
        }

(* -------------------------------------------------------------------------
add a file, typecheck loaded classes, and guess configuration class
------------------------------------------------------------------------- *)
let skip_to_next_class lexbuf =      (* class end found? or at least EOF? *)
  let get_token () =
    try Lex_wrapper.prs_tk lexbuf
    with Ly.Parse _ ->
        Yacc.GARBAGE
  in
  let end_found () =
    let tk = get_token () in
      ( tk = Yacc.EOF ) || ( tk = Yacc.CLASS ) || ( tk = Yacc.INTERFACE )
  in
  let _ = get_token () in
    while not (end_found ()) do () done

let bs_add_file file =
  let file = mk_path_absolute file in
    if file = "" || not(Sys.file_exists file)
    then Err.msg (Err.FileName(file))
    else ();
    let file = if (Filename.is_relative file)
      then ( Filename.concat (Sys.getcwd()) file )
      else ( file ) in
      ps "\nadding file "; ps file; ps " ..."; pF ();
      Gen_application.mk_invalid ();
      ( let cids = ref [] in
          Hashtbl.iter (fun cid ast -> if ast.src_file = file
                  then cids := cid :: !cids
                  else ()
            ) se.classtab;
          List.iter rm_from_classtab !cids
      );
      Hashtbl.iter (fun cid _ -> decrease_to_parsed cid) se.classtab;
      let a_instream = open_in file in
      let lexbuf = Lexing.from_channel a_instream in
      let err_text_and_reset () =   (* error message and look for next class *)
        if (s.p_parsing_graphic) then (
          ps ". File "; ps s.p_graphic_file; ps " graphic item "; pi s.p_goid
        ) else (
          let (l, c) = Ly.pos2lc (Lexing.lexeme_start lexbuf) in
            ps "\n   File "; ps file; ps "\n   line.col "; pi l; ps "."; pi c
        );
        ps ".\n";
        if s.p_parsing_graphic then (
          (* error occured during parsing of graphics, class was OK *)
          exit_parse_graphic ()
        ) else (
          (* error occured during parsing of a class *)
          s.p_blockparse_started <- false;
          skip_to_next_class lexbuf
        );
        ()
      in
        try (* ------------------------------------------------------------------
        this TRY has to close the stream in any case ...
        first parse IMPORTs, then parse a class after the other.
        Field from record s in Util_parse are used for state info's.
        ''s.p_blockparse_started'' remembers, whether parser is
        between or inside the IMPORTs or between or inside a CLASS.
        For imports ''s.p_blockparse_started'' is set here and reset in
        the YACC - rule ''prs_import'',
        for classes the YACC - rule ''prs_class'' sets and resets it
        ------------------------------------------------------------------ *)
            Lex_wrapper.already_read_tokens := [];
            Lex_wrapper.propagate_token := Yacc.GARBAGE;
            s.p_blockparse_started <- true;
            init_parse_of_file file;
            ( try
                let _ = Yacc.prs_import Lex_wrapper.package_token lexbuf in ();
            with
            | Parse(t) -> raise (Error ("\n[[ParseError]] import: "^t))
            | exc -> if s.p_blockparse_started
                then raise exc
                else ()
            );
            while (true) do
            (* compensate the reading of one token ahead when the previous class
            or an import was parsed *)
              Lex_wrapper.already_read_tokens := [ !Lex_wrapper.propagate_token ];
              init_parse_of_class ();
              ( try
              (* parse class to the s. -record and make the ast *)
                  let srcpos = Yacc.prs_class Lex_wrapper.src_token lexbuf in
                  let _ = Lex_wrapper.prs_tk lexbuf in (* read ahead to next class *)
                  let ast = mk_ast srcpos in
                    if (class_status ast.classid (=) Unknown) then (
                      add_to_classtab ast
                    ) else (
                      let file' = (i_ast ast.classid).src_file in
                        if file = file'
                        then Err.msg (Err.TwoClasses(ast.classid, file))
                        else Err.msg (Err.TwoFiles(ast.classid, file, file'))
                    )
              with
              | Parsing.Parse_error -> ps "\n[[ParseError]]";
                  err_text_and_reset ()
              | Failure("lexing: empty token")
              -> ps "\n[[ParseError]]";
                  err_text_and_reset ()
              | Failure(t) -> ps "\n[[ParseError]] "; ps t;
                  err_text_and_reset ()
              | Parse(t) -> ps "\n[[ParseError]] "; ps t;
                  err_text_and_reset ()
              )
            done
        with exc ->
            close_in a_instream;
            if exc = (Error "end_of_file")
            then if s.p_blockparse_started
              then raise (Error "[[ParseError]] invalid class end")
              else () (* regular end ... *)
            else raise exc

let add_file file =
  se.se_file <- file; status#has_changed ();
  try
      bs_add_file file; pF ();
      pF ()
  with
  | Error t ->
      pn (); ps t; pF ()
  | Failure t ->
      pn (); ps "[[Error]] "; ps t; pF ()
  | e ->
      ps "\n[[InternalError]] when loading a file. Was: ";
      ps (Printexc.to_string e); pF ()

let add_file_without_typecheck file =
  se.sefiles <- file:: se.sefiles;
  try
      bs_add_file file; pF ();
  with
  | Error t ->
      pn (); ps t; pF ()
  | Failure t ->
      pn (); ps "[[Error]] "; ps t; pF ()
  | e ->
      ps "\n[[InternalError]] when loading a file. Was: ";
      ps (Printexc.to_string e); pF ()

(* -------------------------------------------------------------------------
fork to simulator, graphic editor, and editor
------------------------------------------------------------------------- *)
let spawn_sim () =
  if Sys.file_exists se.simbinary then (
    let sim = filename_concat [se.se_home;"bin";"synERJYsim"] in
    let ec = match Sys.os_type with
      | "Unix" -> Sys.command (sim^" &")
      | _ -> Sys.command (sim^".exe") in
      ps "\nSimulator";
      ( match ec with
        | 0 -> ps " started\n"
        | _ -> ps " terminated with error ("; pi ec; ps ")\n"
      );
      pF ()
  ) else (
    ps "\nSimulator not started: no binary available"
  )

let spawn_se () =
  let se = (filename_concat [se.se_home;"bin";"synERJY"]) in
  let ec = match Sys.os_type with
    | "Unix" -> Sys.command (se^" &")
    | _ -> Sys.command (se^".exe") in
    ps "\ngraphic editor";
    ( match ec with
      | 0 -> ps " started\n"
      | _ -> ps " terminated with error ("; pi ec; ps ")\n"
    );
    pF ()

let spawn_ge () =
  let ge = (filename_concat [se.se_home;"bin";"synERJYcharts"]) in
  let ec = match Sys.os_type with
    | "Unix" -> Sys.command (ge^" &")
    | _ -> Sys.command (ge^".exe") in
    ps "\ngraphic editor";
    ( match ec with
      | 0 -> ps " started\n"
      | _ -> ps " terminated with error ("; pi ec; ps ")\n"
    );
    pF ()

let spawn_edit ?(file ="") () =
  let ec = Sys.command ("\""^se.editor^"\" \""^file^"\" &") in
    ps "\ntext editor";
    ( match ec with
      | 0 -> ps " started\n"
      | _ -> ps " terminated with error ("; pi ec; ps ")\n"
    );
    pF ()

(* =========================================================================
forking to compilation

compilation starts by calling a Makefile according to host or target type
Makefiles are supposed to have rules
sim - generation of a simulation executable
tgt - for code generation for an executable on the host
sfun - for code generation for a MATLAB executable on the host
(if supported)

========================================================================= *)

(* -------------------------------------------------------------------------
mk_environment - set up the proper environment variables
-- name of binary
-- synERJY generated host language file
-- additional files, e.g.
--- .c files
--- .h files
--- cclibs
-- trace file
------------------------------------------------------------------------- *)
let mk_environment () =
  let conf =
    c2str (type2id se.conf_class) in
  let src =
    se.file_prefix^conf^".c" in
  let cfiles =
    List.fold_left
      ( fun l x -> mk_path_absolute x^" "^l
      ) "" se.cfiles in
  let libs =
    List.fold_left
      ( fun l x -> mk_path_absolute x^" "^l
      ) "" se.cclibs in
  let se_envvars =
    (match se.target_sys with
      | Scicos ->
          if se.scilab_dir = "" then (
            Err.msg (Err.Environment "Scilab directory not specified")
          );
          [| "SE_MAKEINFO=makeinfo";
          "SE_WORKSPACE="^mk_path_absolute se.projectpath ;
          "SE_CPROGRAM="^src;
          "SE_CFILES="^cfiles;
          "SE_CLIBS="^libs;
          "SE_CONF_NAME="^conf;
          "SE_TRACEFILE=\""^se.trace_file^"\"";
          "SE_PREFIX="^se.file_prefix;
          "SCIDIR="^se.scilab_dir;
          "SCIDIR1="^se.scilab_dir;
          |]
      | Simulink ->
          if se.matlab_dir = "" then (
            Err.msg (Err.Environment "Matlab directory not specified")
          );
          [| "SE_MAKEINFO=makeinfo";
          "SE_WORKSPACE="^mk_path_absolute se.projectpath ;
          "SE_CPROGRAM="^src;
          "SE_CFILES="^cfiles;
          "SE_CLIBS="^libs;
          "SE_CONF_NAME="^conf;
          "SE_TRACEFILE=\""^mk_path_absolute se.trace_file^"\"";
          "SE_PREFIX="^se.file_prefix;
          "MATLABDIR="^se.matlab_dir;
          |]
      | Simulation | Host | Makefile | Verification ->
          [| "SE_MAKEINFO=makeinfo";
          "SE_WORKSPACE="^mk_path_absolute se.projectpath ;
          "SE_CPROGRAM="^src;
          "SE_CFILES="^cfiles;
          "SE_CLIBS="^libs;
          "SE_CONF_NAME="^conf;
          "SE_TRACEFILE=\""^mk_path_absolute se.trace_file^"\"";
          "SE_PREFIX="^se.file_prefix;
          "SE_SIMBINARY="^se.simbinary;
          |]
      | Platform platform ->
          [| "SE_MAKEINFO=makeinfo";
          "SE_WORKSPACE="^mk_path_absolute se.projectpath ;
          "SE_CPROGRAM="^src;
          "SE_CFILES="^cfiles;
          "SE_CLIBS="^libs;
          "SE_CONF_NAME="^conf;
          "SE_TRACEFILE=\""^mk_path_absolute se.trace_file^"\"";
          "SE_PREFIX="^se.file_prefix;
          "SE_SIMBINARY="^se.simbinary;
          "SE_PARPORT="^se.par_port;
          "SE_TARGET="^se.se_home^"/target/"^platform;
          |]
      | VerilogSimulation
      | Verilog _ ->
          let src = se.file_prefix^conf in
          let vfiles =
            List.fold_left
              ( fun l x -> mk_path_absolute x^" "^l
              ) "" se.vfiles in
            [|"SE_MAKEINFO=makeinfo";
            "SE_WORKSPACE="^mk_path_absolute se.projectpath;
            "SE_VPROGRAM="^src^".v";
            "SE_VSIMBINARY="^src^"_test.vcd";
            "SE_VFILES="^vfiles;
            "SE_TRACEFILE="^mk_path_absolute se.trace_file;
            "SE_VSIMPROGRAM="^src^"_test.v";
            "SE_PREFIX="^se.file_prefix;
            |]
    )
  in
    (
      let batfilename = se.file_prefix^conf^".bat" in
      let batfile = open_out batfilename in
        Array.iter (fun v ->
                output_string batfile "set " ;
                output_string batfile v ;
                output_string batfile "\n"
          ) se_envvars ;
        flush batfile;
        close_out batfile
    );
    Array.append (Unix.environment()) se_envvars

(* -------------------------------------------------------------------------
makefile - sets the path to the Makefile according to target
------------------------------------------------------------------------- *)
let makefile () =
  let tgt_dir = Filename.concat se.se_home "target" in
    match se.target_sys with
    | Simulation
    | Host
    | Simulink
    | Scicos ->
        filename_concat[tgt_dir; se.hostos;"Makefile"]
    | Makefile ->
        filename_concat[mk_path_absolute se.projectpath;"Makefile"]
    | Platform tgt ->
        filename_concat[tgt_dir; tgt;"Makefile"]
    | VerilogSimulation ->
        filename_concat[tgt_dir;"VerilogSim";"Makefile"]
    | Verilog tgt ->
        filename_concat[tgt_dir; tgt;"Makefile"]
    | Verification ->
        filename_concat[tgt_dir; se.hostos;"Makefile"]

(* -------------------------------------------------------------------------
execute command
------------------------------------------------------------------------- *)
let execute ?(output = true) cmd eval =
  let std_in, std_out, std_err = Unix.open_process_full cmd (mk_environment()) in
  let in_buf = ref [] and err_buf = ref [] in
  let flag_in = ref true and flag_err = ref true in
    if output then (
      while !flag_in || !flag_err do
        ( try in_buf := input_line std_in:: !in_buf
        with _ -> flag_in := false
        );
        ( try err_buf := input_line std_err:: !err_buf
        with _ -> flag_err := false
        );
      done
    );
    let _ = Unix.close_process_full (std_in, std_out, std_err) in
      List.iter eval (List.rev !in_buf);
      List.iter eval (List.rev !err_buf)

(* -------------------------------------------------------------------------
spawn_build - calls the Makefile
-- redirects shell output
------------------------------------------------------------------------- *)
let spawn_build () =
  let suffix = match se.target_sys with
    | Simulation -> " sim"
    | Host -> " tgt"
    | Simulink -> " sfun"
    | Scicos -> " scicos"
    | Makefile -> " tgt"
    | Platform _ -> " tgt"
    | VerilogSimulation -> " sim"
    | Verilog _ -> " tgt"
    | Verification -> Err.intern "spawn_build:3" in
  let cmd = match se.hostos with
    | "msys" -> "make -f "^makefile()^suffix
    | "win32" -> "nmake -f "^makefile()^suffix
    | _ -> "make -f "^makefile()^suffix in
    execute cmd (fun x -> pn(); ps x)

(* -------------------------------------------------------------------------
forking to aplication

compilation starts by calling a Makefile according to host or target type

- application on target, or
- upload for micro controllers or similar

spawn_run - starts the application
-- redirects shell output (so far Unix only)
------------------------------------------------------------------------- *)
let spawn_run () =
  let cmd = match se.hostos, se.target_sys with
    | "msys", Scicos -> "make -f  "^makefile()^" run_scicos"
    | "msys", _ -> "make -f  "^makefile()^" run"
    | "win32", Scicos -> "nmake -f "^makefile()^" run_scicos"
    | "win32", _ -> "nmake -f "^makefile()^" run"
    | "unix", Scicos -> "make -f  "^makefile()^" run_scicos"
    | "unix", _ -> "make -f  "^makefile()^" run"
    | _ -> Err.intern "spawn_run" in
    ps "\nrunning executable\n";
    execute ~output: true cmd (fun x -> pn(); ps x)

(* -------------------------------------------------------------------------
unit test
------------------------------------------------------------------------- *)
let call_unit_test_trace tr =
  let cmd = match se.hostos with
    | "msys" -> "make -f "^makefile()^" unit"
    | "win32" -> "nmake -f "^makefile()^" unit"
    | "unix" -> "make -f "^makefile()^" unit"
    | _ -> Err.intern "call_unit_test_trace" in
    se.trace_file <- tr;
    let success_flag = ref false in
      execute cmd ( fun l ->
              if l = "#success" then success_flag := true else
              if String.contains l '#' then ( ps l; pn() )
        );
      !success_flag

let call_unit_test () =
  let conf = c2str (type2id se.conf_class) in
  let exe =
    match se.target_sys with
    | Simulation ->
        filename_concat
          [mk_path_absolute se.projectpath; se.simbinary]
    | VerilogSimulation ->
        filename_concat
          [mk_path_absolute se.projectpath; se.file_prefix^conf^"_test.v"]
    | _ -> Err.intern "call_unit_test"
  in
    if Sys.file_exists exe then (
      List.fold_left
        ( fun b tr ->
              ps "\n\nrunning unit test for trace file "; ps tr; pn(); pn();
              let suc = call_unit_test_trace tr in
                if suc
                then ps ("\n# trace has passed unit test\n\n")
                else ps "\n# trace has NOT passed unit test\n\n";
                b && suc
          
        ) true se.trace_files
    ) else (
      Err.msg (Err.UnitTest "\nExecution of unit test aborted: \
          no binary available\n")
    )

let spawn_unit_test() =
  let _ = call_unit_test() in ()

(* -------------------------------------------------------------------------
generate different kinds of applications: for
- simulation
- target platform
- verification
------------------------------------------------------------------------- *)
let err m x =
  let s = Printexc.to_string x in
    ps "\n[[InternalError]] ";
    ps m;
    ps ": could not handle exception: ";
    ps s;
    pn();
    pF()

let ps_generated () =
  pn();
  ( match se.target_sys with
    | Platform s -> ps "C-source generated for target system \"";
        ps s; ps "\"\n"
    | Simulation -> ps "C-source generated for simulation\n"
    | Host -> ps ("C-source generated for Host\n")
    | Makefile -> ps "C-source generated for Makefile\n"
    | Simulink -> ps "C-source generated for Simulink\n"
    | Scicos -> ps "all sources generated for Scicos\n"
    | VerilogSimulation -> ps "Verilog source generated for simulation\n";
    | Verilog s -> ps "Verilog source generated for target system \""; ps s; ps "\"\n"
    | Verification -> Err.intern "ps_generated"
  );
  pF ()

let try_make_code () =
  force_process_classes (); (* see below *)
  Gen_application.make ();
  ( match se.target_sys with
    | Simulation
    | Host
    | Simulink
    | Makefile
    | Platform _ ->
        Gen_c_code.mk_c ()
    | Scicos ->
        Gen_c_code.scicos_generate_Makelib ();
        ps "\ngenerate Scicos interface function\n";
        Gen_c_code.mk_scicos_interface ();
        ps "generate Scicos loader function\n";
        Gen_c_code.scicos_generate_loader ();
        ps "generate resource file .scilab\n";
        Gen_c_code.scicos_generate_rcfile ();
        ps "generate Scicos computational function";
        Gen_c_code.mk_c ()
    | VerilogSimulation
    | Verilog _ ->
        Gen_verilog.Verilog.make_verilog ()
    | Verification ->
        Err.intern "make_code:1"
  );
  ps_generated ()

let make_code () =
  try try_make_code ()
  with
  | Error(t) -> pn (); ps t; pF ()
  | Sys_error(t) -> ps "\n(file-)system error occured:\n"; ps t; pF ()
  | x -> err "make_code" x

let make_build () =
  try try_make_code ();
      match se.target_sys with
      | Verilog _ ->
          Err.intern "NYI"
      | _ ->
          ps "\nbuilding target system\n";
          spawn_build()
  with
  | Error(t) -> pn (); ps t; pF ()
  | Sys_error(t) -> ps "\n(file-)system error occured:\n"; ps t; pF ()
  | x -> err "make_build" x

let make_code_and_build () =
  try
      force_process_classes (); (* see above *)
      Gen_application.make ();
      Gen_c_code.mk_c ();
      ps_generated ();
      spawn_build ();
  with
  | Error(t) -> pn (); ps t; pF ()
  | Sys_error(t) -> ps "\n(file-)system error occured:\n"; ps t; pF ()
  | x -> err "make_C_and_build" x

(* -------------------------------------------------------------------------
start the programming environment
------------------------------------------------------------------------- *)
let reset () =
  P.pC();
  se.trace_file <-"";
  se.scicos_model <-"";
  se.conf_class <- t_object;
  Builtin_data.init_builtin (); (* clears the classtab & enters builtins   *)
  status#has_changed ()

let remove_and_reset () =
  let cmd = "make -f "^makefile()^" clean" in
    execute cmd (fun x -> pn(); ps x);
    reset ()

let reset_all () =
  se.sefiles <-[];
  se.secfiles <-[];
  se.trace_file <-"";
  se.trace_files <-[];
  se.scicos_model <-"";
  se.scicos_models <-[];
  se.cfiles <-[];
  se.hfiles <-[];
  se.cclibs <-[];
  remove_and_reset ()

(* -------------------------------------------------------------------------
process a command file
IN GREDIT_TK.ML THERE IS A SIMILAR VERSION OF THIS FUNCTION
------------------------------------------------------------------------- *)
let set_code_generator s =
  se.code_sty <- s;
  Util_gen_c.boolgen :=
  if s ="expr-word" then Util_gen_c.ExprWordSty else
  if s ="expr-bit" then Util_gen_c.ExprBitSty else
  if s ="jmp-bit" then Util_gen_c.JmpBitStyle else
    exit(2)

let process_cmd =
  function
  | CmdClearwdw ->
      pC ()
  | CmdEof ->
      raise End_of_file
  | CmdExecTool Sim ->
      spawn_sim ()
  | CmdExecTool Ge ->
      spawn_ge ()
  | CmdExecTool Edit ->
      spawn_edit ()
  
  | CmdMkBuild ->
      make_code ();
      spawn_build ()
  | CmdMkCode ->
      make_code ();
  | CmdMkTestBinary ->
      make_code_and_build ();
      ps "no build called\n"
  | CmdMkTest -> spawn_unit_test ()
  (* | CmdFPGA -> make_format FPGA
  | CmdBlifAppl -> make_format Blif
  | CmdFormAppl -> make_vis_formula "Application" se.mc_name ()
  | CmdFormClass -> make_vis_formula se.mc_class se.mc_name ()
  | CmdVisAppl -> make_vis_model "Application" ()
  | CmdVisClass -> make_vis_model se.mc_class ()
  *)
  | CmdLoadFile f ->
      let f = mk_path_absolute f in
        if Sys.file_exists f then (
          add_file_without_typecheck f
        ) else (
          pn(); ps"file "; ps f; ps" does not exist\n";
        )
  | CmdLoadTraceFile f ->
      let f = mk_path_absolute f in
        if Filename.check_suffix f ".setr" then (
          if Sys.file_exists f then (
            pn(); ps "trace file "; ps f; ps " loaded\n";
            se.trace_files <- f:: se.trace_files;
            status#has_changed()
          ) else (
            pn(); ps"file "; ps f; ps" does not exist\n";
          )
        ) else (
          pn(); ps"file "; ps f; ps" is not a trace file\n";
        )
  | CmdPrint s ->
      pn (); ps s; pn (); pF ()
  | CmdQuit ->
      exit 0
  | CmdReset ->
      reset ()
  | CmdSetConfClass s ->
      set_conf_class s
  
  | CmdUnit -> () (* already processed in yacc.mly *)
  | CmdSetParPort s -> se.par_port <- s
  | CmdSetCodeStyle s -> set_code_generator s
  
  | CmdRedisplay
  | CmdConfSignal _
  | CmdSaveSimConf _
  | CmdLoadSimConf _
  | CmdTraceBrowser
  | CmdObjectBrowser -> () (* commands ignored by se, relevant for ge,sim *)

let process_pure_cmd_file cmdfile =
  Lex_wrapper.prc_cmdfile process_cmd
    (fun sl -> pn(); List.iter ps sl; pF())
    cmdfile

let process_cmd_file cmdfile =
  process_pure_cmd_file cmdfile;
  force_process_classes ()
