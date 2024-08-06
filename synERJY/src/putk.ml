open Ly
open P
open Ast
open Util
open Tk

(* ========================================================================== *)
let set_command_file keyword file =
	if se.cmd_file = "" then (
		se.cmd_file <- file;
		se.batchmode <- (keyword = "-b")
	) else (
		P.ps "[[Error]] illegal to use both the - b and the - f \
		command line parameter\n";
		exit(1)
	)

let parse_cmdline_args () =
	Arg.parse
		[ ("-f" , Arg.String(set_command_file "-f"),
			" < file > load command - file after $SE_HOME /.serc in an \
			interactive session");
		("-I" , Arg.String(fun s -> ()),
			"ignore -I command (3.07 error");
		("-b" , Arg.String(set_command_file "-b"),
			" < file > process commands from file, then quit. This batch mode \
			is used from Makefile's");
		("-cg", Arg.String(Puctrl.set_code_generator),
			"<sty>: select code generator - INTERNAL USE FOR TESTS")
		]
		( function f -> se.cmdline_files <- f:: se.cmdline_files )
		"se -f <file> -b <file> -cg <sty> <se-file-to-load> ..."

let conf2cid () =
	match se.conf_class with
	| Typ(cid, _) -> cid
	| _ -> Err.intern "conf2str"

let conf2str () =
	Ly.c2str (conf2cid ())

type ide_context_t =
	{ mutable button_sim_config : Option.options_t list;
		mutable button_sim_state : bool;
		mutable button_unit_state : bool;
	}

let ide =
	{ button_sim_config = [Option.Text("Simulator")];
		button_sim_state = false;
		button_unit_state = false;
	}

(* ********************  spawn functions *************************** *)
let call_sim () = Error.guard Puctrl.spawn_sim ()
let call_run () = Error.guard Puctrl.spawn_run ()
let call_ge () = Error.guard Puctrl.spawn_ge ()

(* ==========================================================================
context information
========================================================================== *)
module Target =

struct
	(* ********************  set host and target platform ************** *)
	let set_host () =
		match Sys.os_type with
		| "Win32" -> se.hostos <- "win32"; se.timescale <- 1
		| "Cygwin" -> se.hostos <- "cygwin"; se.timescale <- 1
		| "Unix" -> se.hostos <- "unix"; se.timescale <- 1
		| _ -> Err.intern ""
	
	let load tgt =
		let setgt = filename_concat [se.se_home;"target"; tgt;".setgt"] in
		if Sys.file_exists setgt then (
			Error.guard Puctrl.process_pure_cmd_file setgt
		)
	
	let verify () =
		let filter_smv_msg str = is_substring_of " __" str || is_substring_of "***" str
		and conf = c2str (type2id se.conf_class) in
		let src = se.file_prefix^conf^".smv" in
		if se.nu_smv_dir = "" then (
			Err.msg (Err.Environment "NuSMV directory not specified")
		) else (
			let nu_smv = filename_concat [se.nu_smv_dir;"bin";"NuSMV"] in
			try
				Puctrl.execute
					(nu_smv^" "^src)
					( fun x ->
								if not(x = "" || filter_smv_msg x) then (
									pn(); ps x
								)
					)
			with _ -> ()
		)
	
	(* *********  update the gui for the target system  ********* *)
	let set tgt () =
		( match tgt with
			| Simulation ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Simulator"); Option.Cmd(call_sim)];
					set_host ()
			| Simulink ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Run"); Option.Cmd(call_run)];
					set_host ();
			| Scicos ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Run"); Option.Cmd(call_run)];
					set_host ()
			| Makefile ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Run"); Option.Cmd(call_run)];
					set_host ()
			| Host ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Run"); Option.Cmd(call_run)];
					set_host ()
			| Platform tgt ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Upload"); Option.Cmd(call_run)];
					se.uploadbutton <- "disabled";
					load tgt
			| VerilogSimulation ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Simulator"); Option.Cmd(call_run)];
					set_host ()
			| Verilog _ ->
					ide.button_sim_state <- false;
					ide.button_sim_config <- [Option.Text("Upload"); Option.Cmd(call_run)];
					set_host ()
			| Verification ->
					ide.button_sim_state <- true;
					ide.button_sim_config <- [Option.Text("Verify"); Option.Cmd(verify)];
					set_host ()
		);
		se.target_sys <- tgt;
		Puctrl.status#has_changed()
	
end

(* =========================================================================
Project
========================================================================= *)

module Project =

struct
	
	(* ********************   save project    ************************ *)
	let save () =
		match se.project with
		| None -> ()
		| Some _ ->
				Unix.chdir (mk_path_absolute se.projectpath);
				let out = open_out "_.seprj" in
				let os str = output_string out str in
				os "// settings for the compiler and the main environment\n";
				( match se.target_sys with
					| Simulation -> os "target Simulation;\n"
					| Simulink -> os "target Simulink;\n"
					| Scicos -> os "target Scicos;\n"
					| Host -> os "target Host;\n"
					| Makefile -> os "target Makefile"; os ";\n"
					| Platform tgt -> os "C-code target = "; os tgt; os ";\n"
					| VerilogSimulation -> os "target VerilogSimulation;\n"
					| Verilog tgt -> os "Verilog target = "; os tgt; (); os ";\n"
					| Verification -> os "target Verification;\n";
				);
				( match se.project_kind with
					| CPrj -> os "set project kind = C-code"; os ";\n"
					| VerilogPrj -> os "set project kind = Verilog"; os ";\n"
				);
				List.iter
					( fun f -> os "load file = "; os f; os ";\n"
					) se.sefiles;
				os "set configuration class = ";
				os (c2str (type2id se.conf_class)); os ";\n";
				( match se.secfiles with
					| [] -> ()
					| f:: fl -> os "set secfile = "; os f; os ";\n";
							List.iter (fun f -> os "set secfile += "; os f; os ";\n") fl
				);
				( match se.cfiles with
					| [] -> ()
					| f:: fl -> os "set cfile  = "; os f; os ";\n";
							List.iter (fun f -> os "set cfile  += "; os f; os ";\n") fl
				);
				( match se.hfiles with
					| [] -> ()
					| f:: fl -> os "set hfile  = "; os f; os ";\n";
							List.iter (fun f -> os "set hfile  += "; os f; os ";\n") fl
				);
				( match se.cclibs with
					| [] -> ()
					| f:: fl -> os "set cclib  = "; os f; os ";\n";
							List.iter (fun f -> os "set cclib  += "; os f; os ";\n") fl
				);
				List.iter
					( fun f -> os "load trace file  = "; os f; os ";\n"
					) se.trace_files;
				List.iter
					( fun f -> os "load Scicos model  = "; os f; os ";\n"
					) se.scicos_models;
				( match se.vfiles with
					| [] -> ()
					| f:: fl -> os "set Verilog file  = "; os f; os ";\n";
							List.iter
								(fun f -> os "set Verilog file  += "; os f; os ";\n") fl
				);
				close_out out
	
	(* ********************   load project    ************************ *)
	let load_h ?(dir = None) () =
		if Sys.file_exists se.workspace then (
			let dir =
				match dir with
				| None -> Dialog.chooseDirectory
							~parent:"." ~initdir: se.workspace
							"Choose project"
				| Some d -> d
			in
			if Sys.file_exists dir then (
				match filename_minus_path dir se.workspace with
				| None ->
						ps "\nProject is not within the current workspace\n"
				| Some d ->
						let file = filename_concat [dir;"_.seprj"] in
						if Sys.file_exists file then (
							Unix.chdir dir;
							Error.guard Puctrl.reset_all();
							se.project <- Some (Filename.basename dir);
							se.projectpath <- filename_concat ("<workspace>":: d);
							Error.guard Puctrl.process_pure_cmd_file file;
							ide.button_sim_state <- false;
							Puctrl.force_process_classes();
							let conf c a = if a.classkind = ConfClass
								then Some c
								else None in
							se.conf_classes <- hashtbl_condense conf se.classtab;
							if se.conf_classes = [] then Err.msg (Err.NoConfClass);
							Puctrl.status#has_changed()
						) else (
							if dir = se.workspace || dir = ""
							then ps "no project chosen"
							else
							if Constant.windowingsystem() = Constant.aqua
							then ps ("\nDirectory '"^dir
										^"' is NOT a synERJY project\n\n")
							else ps ("\nFolder '"^dir
										^"' is NOT a synERJY project\n\n")
						)
			) else if dir = "" then (
			) else (
				ps ("\nproject '"^dir^"' does not exist\n\n")
			)
		) else (
			ps ("\nworkspace '"^se.workspace^"' does not exist\n\n")
			
		)
	
	(* ********************  load and reload    ************************ *)
	let load ?(dir = None) () =
		match se.project with
		| None ->
				load_h ~dir: dir ()
		| Some prj ->
				ps ("\nProject "^prj^" already loaded\n")
	
	let reload () =
		load_h ~dir: (Some (mk_path_absolute se.projectpath)) ();
		pF()
	
	(* ********************   close project    ************************* *)
	let close () =
		se.project <- None;
		Error.guard Puctrl.reset_all();
		se.projectpath <- "";
		se.conf_classes <- [];
		Puctrl.status#has_changed()
end

(* =========================================================================
Preferences
========================================================================= *)
class preference_panel =
	
	let mat_txt = "  Matlab directory "
	and sci_txt = "  Scilab directory "
	and smv_txt = "  NuSMV directory  " in
	
	object(self)
		
		val top = new Toplevel.toplevel "Preferences" ".se.prefs"
		
		val choices = new Ttk.notebook ".se.prefs.nb"
		
		val frame1 = new Ttk.frame ".se.prefs.nb.f1"
		
		val wrksp_lbl = new Ttk.label ~text:"  Workspace  "".se.prefs.nb.f1.wrksp"
		val wrksp_box = new Ttk.entry 50 ".se.prefs.nb.f1.wrksp_e"
		val wspbutton = new Ttk.button ".se.prefs.nb.f1.wspbutton"
		
		val size_lbl = new Ttk.label ~text:"  Text size  " ".se.prefs.nb.f1.size"
		val size_box = new Ttk.combobox ".se.prefs.nb.f1.size_e"
		
		val weight_lbl = new Ttk.label ~text:"  Text weight  " ".se.prefs.nb.f1.weight"
		val weight_box = new Ttk.combobox ".se.prefs.nb.f1.weight_e"
		
		val code_lbl = new Ttk.label ~text:"  Code style  " ".se.prefs.nb.f1.code_sty"
		val code_box = new Ttk.combobox ".se.prefs.nb.f1.code_sty_e"
		val pport_lbl = new Ttk.label ~text:"  Parallel port  " ".se.prefs.nb.f1.par_port"
		val pport_box = new Ttk.combobox ".se.prefs.nb.f1.par_port_e"
		
		val frame2 = new Ttk.frame ".se.prefs.nb.f2"
		
		val matlab_lbl = new Ttk.label ~text: mat_txt ".se.prefs.nb.f2.matlab"
		val matlab_box = new Ttk.entry 50 ".se.prefs.nb.f2.matlab_e"
		val matlab_but = new Ttk.button ".se.prefs.nb.f2.matlab_but"
		
		val scilab_lbl = new Ttk.label ~text: sci_txt ".se.prefs.nb.f2.scilab"
		val scilab_box = new Ttk.entry 50 ".se.prefs.nb.f2.scilab_e"
		val scilab_but = new Ttk.button ".se.prefs.nb.f2.scilab_but"
		
		val nu_smv_lbl = new Ttk.label ~text: smv_txt ".se.prefs.nb.f2.nu_smv"
		val nu_smv_box = new Ttk.entry 50 ".se.prefs.nb.f2.nu_smv_e"
		val nu_smv_but = new Ttk.button ".se.prefs.nb.f2.nu_smv_but"
		
		val frame3 = new Ttk.frame ".se.prefs.nb.f3"
		
		val editor_lbl = new Ttk.label ~text:"  Editor  " ".se.prefs.nb.f3.editor"
		val editor_box = new Ttk.entry 25 ".se.prefs.nb.f3.editor_e"
		val editbutton = new Ttk.button ".se.prefs.nb.f3.editbutton"
		
		val frame4 = new Util_tk.bordered_frame ~top: 10 ~bottom: 10 ~left: 20 ~right: 20 ".se.prefs.f4"
		val acceptbutton = new Ttk.button ~text:"Save" ".se.prefs.f4.ok"
		val abortbutton = new Ttk.button ~text:"Dismiss" ".se.prefs.f4.no"
		
		method choose_workspace () =
			let dir = Dialog.chooseDirectory ~parent:".se.prefs" "Choose workspace" in
			if Sys.file_exists dir then (
				wrksp_box#set dir;
				Puctrl.status#has_changed();
				top#raise
			) else (
				ps "\nChosen directory  does not exist\n\n"
			)
		
		method choose_editor () =
			let ft =
				match Sys.os_type with
				| "Unix" -> "{{All Files} {*}}"
				| _ -> "{{executable} {.exe}} {{All Files} {*}}"
			in
			let file = Dialog.getOpenFile ~parent:"." ~filetypes: ft "Choose editor" in
			if Sys.file_exists file then (
				editor_box#set file;
				Puctrl.status#has_changed();
				top#raise
			) else (
				ps "\nChosen editor  does not exist\n\n"
			)
		
		method choose_dir (entry: Ttk.entry) txt () =
			let file = Dialog.chooseDirectory ~parent:"." txt in
			if Sys.file_exists file then (
				entry#set file;
				Puctrl.status#has_changed();
				top#raise;
			) else (
				ps "\nChosen directory does not exist"
			)
		
		method set () =
			wrksp_box#set se.workspace;
			editor_box#set se.editor;
			let (f, size, weight) = se.se_font in
			size_box#set size;
			weight_box#set weight;
			matlab_box#set se.matlab_dir;
			scilab_box#set se.scilab_dir;
			nu_smv_box#set se.nu_smv_dir;
			Puctrl.status#has_changed();
		
		method apply _ =
			let wsp = wrksp_box#get in
			if se.workspace = wsp then (
			(* do nothing *)
			) else (
				Error.guard Project.close();
				se.workspace <- wsp
			);
			se.editor <- editor_box#get;
			let se_size = size_box#get
			and se_weight = weight_box#get
			and (se_font, _, _) = se.se_font in
			se.se_font <- (se_font, se_size, se_weight);
			self#set();
			Font.set Font.se_font se_size se_weight;
			se.matlab_dir <- matlab_box#get;
			se.scilab_dir <- scilab_box#get;
			se.nu_smv_dir <- nu_smv_box#get;
		
		(* let tm = timescale#entry#get in
		( try let tm = s2i tm in
		if tm > 0
		then se.timescale <- tm
		else raise (Failure "")
		with Failure _ ->
		ignore (se_messageBox ~default:"ok" "ok" "error"
		("Time granularity not well specified."))
		
		) *)
		
		method save () =
			self#apply [||];
			Error.guard Util_tk.save_preferences();
			Puctrl.status#has_changed();
			top#destroy
		
		initializer (
			Wm.not_resizable top#path;
			
			size_box#state Constant.ReadOnly;
			size_box#load_items se.font_sizes;
			size_box#set_cmd self#apply;
			
			weight_box#state Constant.ReadOnly;
			weight_box#load_items se.font_weights;
			weight_box#set_cmd self#apply;
			
			choices#pack [Pack.Top];
			
			choices#add frame1#path [Option.Text("General")];
			
			wrksp_box#insert se.workspace;
			wrksp_box#state Constant.ReadOnly;
			wspbutton#configure [Option.Cmd(self#choose_workspace); Option.Text("Browse")];
			
			code_box#set se.code_sty;
			code_box#configure [Option.Width(10)];
			code_box#load_items ["expr-word";"expr-bit";"jmp-bit"];
			code_box#set_cmd
				( fun _ ->
							se.code_sty <- code_box#get;
							Puctrl.set_code_generator se.code_sty;
							Puctrl.status#has_changed();
				);
			
			pport_box#set se.par_port;
			pport_box#configure [Option.Width(8)];
			pport_box#load_items ["0x278";"0x378";"0x3BC"];
			pport_box#set_cmd
				( fun _ ->
							se.par_port <- pport_box#get;
							Puctrl.status#has_changed();
				);
			
			Grid.grid [ wrksp_lbl#path; wrksp_box#path;
				"-"; "-"; wspbutton#path] [Grid.Sticky("nsew")];
			(* Grid.grid [ size_lbl#path; size_box#path;
			weight_lbl#path; weight_box#path] [Grid.Sticky("nsew")];*)
			Grid.grid [ code_lbl#path; code_box#path;
				pport_lbl#path; pport_box#path] [Grid.Sticky("nsew")];
			
			choices#add frame2#path [Option.Text("Environment")];
			
			matlab_lbl#configure [Option.Width(16)];
			matlab_box#insert se.matlab_dir;
			matlab_box#state Constant.ReadOnly;
			let choose_matlab_dir = self#choose_dir matlab_box mat_txt in
			matlab_but#configure [Option.Cmd(choose_matlab_dir); Option.Text("Browse")];
			
			scilab_lbl#configure [Option.Width(16)];
			scilab_box#insert se.scilab_dir;
			scilab_box#state Constant.ReadOnly;
			let choose_scilab_dir = self#choose_dir scilab_box sci_txt in
			scilab_but#configure [Option.Cmd(choose_scilab_dir); Option.Text("Browse")];
			
			nu_smv_lbl#configure [Option.Width(16)];
			nu_smv_box#insert se.nu_smv_dir;
			nu_smv_box#state Constant.ReadOnly;
			let choose_nu_smv_dir = self#choose_dir nu_smv_box smv_txt in
			nu_smv_but#configure [Option.Cmd(choose_nu_smv_dir); Option.Text("Browse")];
			
			Grid.grid [ matlab_lbl#path; matlab_box#path; matlab_but#path] [Grid.Sticky("nsew")];
			Grid.grid [ scilab_lbl#path; scilab_box#path; scilab_but#path] [Grid.Sticky("nsew")];
			Grid.grid [ nu_smv_lbl#path; nu_smv_box#path; nu_smv_but#path] [Grid.Sticky("nsew")];
			
			choices#add frame3#path [Option.Text("Miscellaneous")];
			
			editor_lbl#configure [Option.Width(10)];
			editor_box#configure [Option.Width(55)];
			editor_box#insert se.editor;
			editor_box#state Constant.ReadOnly;
			editbutton#configure [Option.Cmd(self#choose_editor); Option.Text("Browse")];
			
			Grid.grid [ editor_lbl#path; editor_box#path;
				editbutton#path] [Grid.Sticky("nsew")];
			
			frame4#pack [Pack.Bottom; Pack.Fillx];
			frame4#configure [Option.Relief(Option.Flat)];
			acceptbutton#configure [Option.Cmd(self#save)];
			acceptbutton#pack [Pack.Right];
			abortbutton#configure [Option.Cmd(fun () -> top#destroy)];
			abortbutton#pack [Pack.Right];
			
			self#set();
			top#raise;
			top#grab
		)
	end

(* ********************   project panel    ************************ *)
class project_panel =

object(self)
	val top = new Toplevel.toplevel "Project panel" ".se.project"
	
	val filebox = new Ttk.notebook ".se.project.nb"
	val sefiles = new Util_tk.filebox ".se.project.nb.sefiles"
	val secfiles = new Util_tk.filebox ".se.project.nb.secfiles"
	val setrfiles = new Util_tk.filebox ".se.project.nb.setrfiles"
	val cfiles = new Util_tk.filebox ".se.project.nb.cfiles"
	val hfiles = new Util_tk.filebox ".se.project.nb.hfiles"
	val cclibfiles = new Util_tk.filebox ".se.project.nb.cclibfiles"
	val scicosfiles = new Util_tk.filebox ".se.project.nb.scicosfiles"
	val verilogfiles = new Util_tk.filebox ".se.project.nb.verilogfiles"
	
	val frame2 = new Util_tk.bordered_frame ~bottom: 10 ~left: 20 ~right: 20 ".se.project.f2"
	val closebutton = new Ttk.button ~text:"Close" ".se.project.f2.ok"
	
	method top = top
	
	method close () = Error.guard Project.save(); top#destroy
	
	initializer (
		top#set_cmd self#close;
		Wm.not_resizable top#path;
		
		(* se files *)
		filebox#add sefiles#path [Option.Text("synERJY files")];
		sefiles#set_suffix "{{synERJY file} {.se}}";
		sefiles#set_add
			( fun file ->
						let file = mk_path_relative file in
						if List.mem file se.sefiles then (
							None
						) else (
							(try Puctrl.add_file file with _ -> ());
							se.sefiles <- se.sefiles@[file];
							Puctrl.force_process_classes ();
							Error.guard Project.save();
							Puctrl.status#has_changed();
							Some file
						)
			);
		sefiles#set_del
			( fun file ->
						se.sefiles <- List.filter (fun x -> x <> file) se.sefiles;
						Error.guard Project.save ();
						Puctrl.status#has_changed()
			);
		sefiles#insert_list se.sefiles;
		
		(* sec files *)
		filebox#add secfiles#path [Option.Text("synERJYcharts")];
		secfiles#set_suffix "{{synERJYcharts file} {.sec}}";
		secfiles#set_add
			( fun file ->
						let file = mk_path_relative file in
						if List.mem file se.secfiles then (
							None
						) else (
							se.secfiles <- se.secfiles@[file];
							Error.guard Project.save();
							Puctrl.status#has_changed();
							Some file
						)
			);
		secfiles#set_del
			( fun file ->
						se.secfiles <- List.filter (fun x -> x <> file) se.secfiles;
						Error.guard Project.save();
						Puctrl.status#has_changed()
			);
		secfiles#insert_list se.secfiles;
		
		(* trace files *)
		filebox#add setrfiles#path [Option.Text("Trace files")];
		setrfiles#set_suffix "{{trace file} {.setr}}";
		setrfiles#set_add
			( fun file ->
						let file = mk_path_relative file in
						if List.mem file se.trace_files then (
							None
						) else (
							se.trace_files <- se.trace_files@[file];
							Error.guard Project.save();
							Puctrl.status#has_changed();
							Some file
						)
			);
		setrfiles#set_del
			( fun file ->
						se.trace_files <- List.filter (fun x -> x <> file) se.trace_files;
						Error.guard Project.save();
						Puctrl.status#has_changed()
			);
		setrfiles#insert_list se.trace_files;
		
		( match se.project_kind with
			| CPrj ->
			
			(* c files *)
					filebox#add cfiles#path [Option.Text("C files")];
					cfiles#set_suffix "{{C file} {.c}}";
					cfiles#set_add
						(fun file ->
									let file = mk_path_relative file in
									if List.mem file se.cfiles then (
										None
									) else (
										se.cfiles <- se.cfiles@[file];
										Error.guard Project.save();
										Puctrl.status#has_changed();
										Some file
									)
						);
					cfiles#set_del
						(fun file ->
									se.cfiles <- List.filter (fun x -> x <> file) se.cfiles;
									Error.guard Project.save();
									Puctrl.status#has_changed()
						);
					cfiles#insert_list se.cfiles;
					
					(* h files *)
					filebox#add hfiles#path [Option.Text("Header files")];
					hfiles#set_suffix "{{Header file} {.h}}";
					hfiles#set_add
						( fun file ->
									let file = mk_path_relative file in
									if List.mem file se.hfiles then (
										None
									) else (
										se.hfiles <- se.hfiles@[file];
										Error.guard Project.save();
										Puctrl.status#has_changed();
										Some file
									)
						);
					hfiles#set_del
						( fun file ->
									se.hfiles <- List.filter (fun x -> x <> file) se.hfiles;
									Error.guard Project.save();
									Puctrl.status#has_changed()
						);
					hfiles#insert_list se.hfiles;
					
					(* cclibs *)
					filebox#add cclibfiles#path [Option.Text("cclibs")];
					cclibfiles#set_suffix "{{cclib} {.a .so}}";
					cclibfiles#set_add
						( fun file ->
									let file = mk_path_relative file in
									if List.mem file se.cclibs then (
										None
									) else (
										se.cclibs <- se.cclibs@[file];
										Error.guard Project.save();
										Puctrl.status#has_changed();
										Some file
									)
						);
					cclibfiles#set_del
						( fun file ->
									se.cclibs <- List.filter (fun x -> x <> file) se.cclibs;
									Error.guard Project.save();
									Puctrl.status#has_changed()
						);
					cclibfiles#insert_list se.cclibs;
					
					(* scicos models *)
					if se.target_sys = Scicos then (
						filebox#add scicosfiles#path [Option.Text("Scicos model")];
						scicosfiles#set_suffix " {{ scicos textual model } {.cosf }}
						{{ scicos binary model } {.cos }} ";
						scicosfiles#set_add
							( fun file ->
										let file = mk_path_relative file in
										if List.mem file se.scicos_models then (
											None
										) else (
											se.scicos_models <- se.scicos_models@[file];
											Error.guard Project.save();
											Puctrl.status#has_changed();
											Some file
										)
							);
						scicosfiles#set_del
							( fun file ->
										se.scicos_models <-
										List.filter (fun x -> x <> file) se.scicos_models;
										Error.guard Project.save();
										Puctrl.status#has_changed()
							);
						scicosfiles#insert_list se.scicos_models
					)
			
			| VerilogPrj ->
			
			(* v files *)
					filebox#add verilogfiles#path [Option.Text("Verilog file")];
					verilogfiles#set_suffix "{{Verilog file} {.v}}";
					verilogfiles#set_add
						( fun file ->
									let file = mk_path_relative file in
									if List.mem file se.vfiles then (
										None
									) else (
										(try Puctrl.add_file file with _ -> ());
										se.vfiles <- se.vfiles@[file];
										Error.guard Project.save();
										Puctrl.status#has_changed();
										Some file
									)
						);
					verilogfiles#set_del
						( fun file ->
									se.vfiles <- List.filter (fun x -> x <> file) se.vfiles;
									Error.guard Project.save ();
									Puctrl.status#has_changed()
						);
					verilogfiles#insert_list se.vfiles;
		);
		
		filebox #pack [Pack.Top; Pack.Fillx; Pack.Expand];
		frame2#pack [Pack.Bottom; Pack.Fillx];
		frame2#configure [Option.Relief(Option.Groove)];
		closebutton#pack [Pack.Right];
		closebutton#configure [Option.Cmd(self#close)];
		top#raise
	)
	
end

(* ********************   new project    ************************ *)
class new_project_panel =

object(self)
	val top = new Toplevel.toplevel "New Project" ".se.newprct"
	
	val frame1 = new Util_tk.bordered_frame ~top: 20 ~bottom: 20
			~left: 20 ~right: 20 ".se.newprct.f1"
	val kind_lbl = new Ttk.label ~text:" Kind " ".se.newprct.f1.kind"
	val kind_box = new Ttk.combobox ".se.newprct.f1.kind"
	val frame11 = new Ttk.frame ".se.newprct.f1.f11"
	val project = new Util_tk.dir_entry "Name " 40 ~base: true ".se.newprct.f1.f11.dir"
	val prj_path = new Util_tk.dir_entry "Path " 40 ".se.newprct.f1.prj_path"
	
	val frame2 = new Util_tk.bordered_frame ~top: 10 ~bottom: 10 ~left: 20 ~right: 20 ".se.newprct.f2"
	val savebutton = new Ttk.button ~text:"Save" ".se.newprct.f2.yes"
	val discardbutton = new Ttk.button ~text:"Dismiss" ".se.newprct.f2.no"
	
	method save () =
		let prj = project#entry#get and path = prj_path#entry#get in
		if prj = "" || path = "" then (
			ps "\nProject name || path not specified\n"
		) else (
			let prj_path = Filename.concat path prj in
			if Sys.file_exists prj_path then (
				let m = "Turn existing directory "^prj_path^" into a project" in
				let x = Dialog.messageBox ~default:"ok" "okcancel" "question" m in
				if x = "ok" then (
					se.project <- Some prj;
					se.projectpath <- prj_path;
					Unix.chdir prj_path;
					Error.guard Project.save();
					Puctrl.status#has_changed();
					top#destroy
				)
			) else (
				Unix.chdir path;
				Unix.mkdir prj 0o777;
				se.project <- Some prj;
				se.projectpath <- prj_path;
				Error.guard Puctrl.status#has_changed();
				Unix.chdir prj_path;
				top#destroy
			)
		)
	
	initializer (
		Wm.resize_width_only top#path;
		frame1#configure [Option.Relief(Option.Groove)];
		frame1#pack [Pack.Top; Pack.Fillx; Pack.Expand];
		kind_box#set "C project";
		kind_box#load_items ["C prjct";"Verilog project"];
		se.project_kind <- CPrj;
		kind_lbl#configure [Option.Width(8)];
		kind_box#state Constant.ReadOnly;
		kind_box#set_cmd
			( fun _ ->
						se.project_kind <-
						( match kind_box#get with
							| "C project" -> CPrj
							| "Verilog project" -> VerilogPrj
							| _ -> Err.intern "new_prj"
						);
						Puctrl.status#has_changed();
			);
		
		Grid.grid [ kind_lbl#path; kind_box#path] [Grid.Sticky("nsew")];
		
		project#label#configure [Option.Width(8)];
		prj_path#entry#set se.workspace;
		prj_path#label#configure [Option.Width(8)];
		Grid.grid [ prj_path#path; "-"] [Grid.Sticky("nsew")];
		
		frame2#pack [Pack.Bottom; Pack.Fillx; Pack.Expand];
		frame2#configure [Option.Relief(Option.Groove)];
		savebutton#pack [Pack.Right];
		savebutton#configure [Option.Cmd(self#save)];
		discardbutton#pack [Pack.Right];
		discardbutton#configure [Option.Cmd(fun () -> top#destroy)];
		top#raise;
		top#grab;
		top#focus
	)
	
end

(* =========================================================================
synERJY console
========================================================================= *)
class synerjy_console pwd =

object(self)
	
	val top =
		new Toplevel.toplevel "synERJY Programming Environment"
			~about: (Some("About synERJY", Util_tk.Dialog.about ("synERJY "^se.version))) ".se"
	
	val pu_frame = new Ttk.frame ".se.pu"
	
	(* buttons *)
	val buttons =
		let left = if Constant.windowingsystem() = Constant.aqua then 5 else 0
		and right = if Constant.windowingsystem() = Constant.aqua then 5 else 0 in
		new Util_tk.bordered_frame ~left: left ~right: right ".se.pu.buttons"
	
	val button_clear = new Ttk.button ~text:"Clear" ".se.pu.buttons.clear"
	val button_reload = new Ttk.button ~text:"Reload" ".se.pu.buttons.reload"
	val button_sim = new Ttk.button ~text:"Simulator" ".se.pu.buttons.sim"
	val button_build = new Ttk.button ~text:"Build" ".se.pu.buttons.build"
	val button_quit = new Ttk.button ~text:"Quit" ".se.pu.buttons.quit"
	
	val sE_f = new Util_tk.bordered_frame ~left: 8 ~right: 8 ~bottom: 8 ".se.pu.buttons.f"
	val sE = new Ttk.label ".se.pu.buttons.f.sE"
	val top_right = new Util_tk.bordered_frame ~top: 10 ~bottom: 10 ~left: 5 ~right: 5 ".se.pu.top"
	
	val line = new Ttk.frame ".se.pu.line"
	
	(* base information *)
	val top_right_i = new Ttk.frame ".se.pu.top.frame"
	val project_lbl = new Ttk.label ~text:"  Project" ".se.pu.top.frame.project"
	val project_box = new Ttk.entry 20 ".se.pu.top.frame.project_e"
	val wrksp_lbl = new Ttk.label ~text:"  Workspace" ".se.pu.top.frame.workspace"
	val wrksp_box = new Ttk.entry 30 ".se.pu.top.frame.workspace_e"
	
	val tgt_lbl = new Ttk.label ~text:"  Target" ".se.pu.top.frame.tgt"
	val tgt_box = new Ttk.combobox ".se.pu.top.frame.tgt_e"
	val conf_lbl = new Ttk.label ~text:"  Conf class" ".se.pu.top.frame.class"
	val conf_box = new Ttk.combobox ".se.pu.top.frame.class_e"
	
	val file_lbl = new Ttk.label ~text:"  File" ".se.pu.top.frame.file"
	val file_box = new Ttk.combobox ".se.pu.top.frame.file_e"
	
	(* trace or scicos model *)
	val trace_lbl = new Ttk.label ~text:"  Trace " ".se.pu.top.frame.trace"
	val trace_box = new Ttk.combobox ".se.pu.top.frame.trace_e"
	val button_unit = new Ttk.button ".se.pu.top.frame.trace_b"
	
	(* Verification toolbar *)
	val tool_lbl = new Ttk.label ~text:"  Tool" ".se.pu.top.frame.tool"
	val tool_box = new Ttk.combobox ".se.pu.top.frame.tool_e"
	val model_lbl = new Ttk.label ~text:"  Model" ".se.pu.top.frame.model"
	val model_box = new Ttk.combobox ".se.pu.top.frame.model_e"
	val logic_lbl = new Ttk.label ~text:"  Logic" ".se.pu.top.frame.logic"
	val logic_box = new Ttk.combobox ".se.pu.top.frame.logic_e"
	val spec_lbl = new Ttk.label ~text:"  Proposition" ".se.pu.top.frame.spec"
	val spec_box = new Ttk.combobox ".se.pu.top.frame.spec_e"
	
	(* log text *)
	val log = new Tk.text ".se.pu.log_txt"
	
	(* methods *)
	method define_new_project () =
		match se.project with
		| None ->
				ignore(new new_project_panel)
		| Some prj ->
				ps ("\n\nProject "^prj^" already loaded\n")
	
	method load_project () =
		Error.guard Project.load();
		Target.set se.target_sys ()
	
	method reload_project () =
		Error.guard Project.reload();
		Target.set se.target_sys ()
	
	method reload_file () =
		let file = file_box#get in
		( try Puctrl.add_file file with _ -> () )
	
	method show_project () =
		if se.project = None
		then ps "\n\nno project chosen"
		else ignore(new project_panel)
	
	method call_edit f () =
		if se.editor = ""
		then ps "\n\n!!!!!!!!!!!!! No editor chosen !!!!!!!!!!!!!
		\nUse the preference panel to choose an editor."
		else Error.guard (Puctrl.spawn_edit ~file: (mk_path_absolute f)) ()
	
	method make_build () =
		if se.project = None then
			ps "\n\nno project chosen"
		else (
			match se.target_sys with
			| Simulink ->
					Error.guard Puctrl.make_build ();
					Puctrl.status#has_changed();
					ide.button_sim_state <- false
			| Verification ->
					self#mk_model;
					Puctrl.status#has_changed();
					ide.button_sim_state <- true
			| _ ->
					Error.guard Puctrl.make_build ();
					Puctrl.status#has_changed();
					ide.button_sim_state <- true
		);
		button_build#configure [Option.Text("Build"); Option.Cmd(self#make_build)];
		ide.button_sim_state <- true;
		Puctrl.status#has_changed()
	
	method make_code () =
		if se.project = None
		then ps "\n\nno project chosen"
		else Error.guard Puctrl.make_code ();
		button_build#configure [Option.Text("Compile"); Option.Cmd(self#make_code)];
		ide.button_sim_state <- false;
		Puctrl.status#has_changed();
		ps "\n\nno build called\n"
	
	method unit_test () =
		try
			if Puctrl.call_unit_test ()
			then ps "\nAll files have passed the unit test."
			else ps "\n!!!!!! Some file did not pass the unit test.  !!!!!!"
		with Error e ->
				pn(); ps e; pn()
	
	method quit () =
		Constant.destroy "."
	
	(* verification *)
	method mk_model =
		match tool_box#get with
		| "NuSMV" ->
				Gen_NuSMV.SMV.make_smv_model
					(logic_box#get) (model_box#get) (spec_box#get)
		| err ->
				Err.msg (Err.NoVerTool(err))
	
	method select_logics tool =
		try snd (List.find (fun (t, _) -> t = tool) se.logics)
		with Not_found -> []
	
	method models () =
		let cl = ref [] in
		Hashtbl.iter
			( fun cid c ->
						if c.classkind = RctClass
						then cl:= (c2str cid)::!cl
			) se.classtab;
		("Application"::!cl)
	
	method all_propositons sa =
		let props = Gen_NuSMV.SMV.get_props sa logic_box#get in
		List.fold_left
			( fun l (sp, ro) ->
						match sp with
						| Ctl(lbl, _, _) -> (Util_print.ro_lbl2str ro lbl):: l
						| Ltl(lbl, _) -> (Util_print.ro_lbl2str ro lbl):: l
						| Ptl(lbl, _) -> (Util_print.ro_lbl2str ro lbl):: l
						| _ -> l
			) [] props
	
	method filter_props props =
		match tool_box#get, logic_box#get with
		| "NuSMV","CTL" ->
				List.fold_right
					( fun spec l ->
								match spec with
								| Ctl (id, _, _) -> (lbl2str id):: l
								| _ -> l
					) props []
		| "NuSMV","LTL" ->
				List.fold_right
					( fun spec l ->
								match spec with
								| Ltl (id, _) -> (lbl2str id):: l
								| _ -> l
					) props []
		| _ -> Err.intern "select_spec"
	
	method select_specs =
		let model = model_box#get in
		if model = "Application" then (
			match Gen_application.ac.Gen_application.ac_sca with
			| None -> self#filter_props (i_ast (conf2cid())).props
			| Some sa -> self#all_propositons sa
		) else (
			try let cid = Lex_wrapper.parse_string Yacc.prs_classid model in
				self#filter_props (i_ast cid).props
			with _ -> Err.msg (Err.NoClass(model))
		)
	
	method ins_specs =
		spec_box#delete();
		let specs = self#select_specs in
		let specs =
			match specs with
			| [] -> []
			| [f] -> specs
			| _ -> "All":: specs
		in
		spec_box#load_items specs;
		if specs <> [] then spec_box#current 0
	
	method ins_logics =
		logic_box#delete();
		let logics = self#select_logics tool_box#get in
		logic_box#load_items logics;
		if logics <> [] then logic_box#current 0;
		self#ins_specs
	
	initializer (
		top#focusmodel "active";
		top#set_cmd (fun () -> top#destroy);
		Wm.withdraw top#path;
		(* if Constant.windowingsystem() = Constant.aqua
		then Wm.attributes top#path "notify" "1";*)
		
		Font.txt_font := Font.se_font;
		
		(* toplevel *)
		if Constant.windowingsystem() = Constant.aqua then (
			Constant.show_preferences (fun () -> ignore(new preference_panel))
		);
		
		(* Menus *)
		let m = top#menu in
		let file = new Tk.menu (m#path^".file") in
		m#add_cascade "File" file;
		file#add_command ~cllbck: self#define_new_project
			~acc:"N" "New project";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-n>" self#define_new_project;
			top#bind "<Command-KeyPress-N>" self#define_new_project
		) else (
			top#bind "<Control-KeyPress-n>" self#define_new_project;
			top#bind "<Control-KeyPress-N>" self#define_new_project
		);
		file#add_command ~cllbck: self#load_project
			~acc:"O" "Load project";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-o>" self#load_project;
			top#bind "<Command-KeyPress-O>" self#load_project
		) else (
			top#bind "<Control-KeyPress-o>" self#load_project;
			top#bind "<Control-KeyPress-O>" self#load_project
		);
		file#add_command ~cllbck: self#reload_project
			~acc:"R" "Reload project";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-r>" self#reload_project;
			top#bind "<Command-KeyPress-R>" self#reload_project
		) else (
			top#bind "<Control-KeyPress-r>" self#reload_project;
			top#bind "<Control-KeyPress-R>" self#reload_project
		);
		file#add_command ~cllbck: Project.close ~acc:"W" "Close project";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-w>" Project.close;
			top#bind "<Command-KeyPress-W>" Project.close
		) else (
			top#bind "<Control-KeyPress-w>" Project.close;
			top#bind "<Control-KeyPress-W>" Project.close
		);
		(*se_menu_add_command f "Save project"            save_project 0 "S";*)
		if not(Constant.windowingsystem() = Constant.aqua) then (
			file#add_separator;
			file#add_command ~cllbck: self#quit ~acc:"Q" "Close project";
			top#bind "<<Quit>>" self#quit;
		);
		
		let edit = new Tk.menu (m#path^".edit") in
		m#add_cascade "Edit" edit;
		
		if Constant.version() >= "8.4" && not(Constant.windowingsystem() = Constant.aqua)
		then (
			edit#add_command ~cllbck: log#undo ~acc:"Z" "Undo";
			edit#add_command ~cllbck: log#redo ~acc:"Y" "Redo";
			edit#add_separator
		);
		edit#add_command ~cllbck: log#cut ~acc:"X" "Cut";
		edit#add_command ~cllbck: log#copy ~acc:"C" "Copy";
		edit#add_command ~cllbck: log#paste ~acc:"V" "Paste";
		edit#add_separator;
		edit#add_command ~cllbck: P.pC "Clear";
		if Constant.windowingsystem() = Constant.aqua then (
			edit#add_separator;
			edit#add_command
				~cllbck: (fun () -> ignore(new preference_panel)) "Preferences";
		);
		
		let view = new Tk.menu (m#path^".view") in
		m#add_cascade "View" view;
		view#add_command ~cllbck: self#show_project ~acc:"P" "Project";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-p>" self#show_project;
			top#bind "<Command-KeyPress-P>" self#show_project
		) else (
			top#bind "<Control-KeyPress-p>" self#show_project;
			top#bind "<Control-KeyPress-P>" self#show_project
		);
		view#add_command ~cllbck: call_ge ~acc:"G" "synERJYcharts";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-g>" call_ge;
			top#bind "<Command-KeyPress-G>" call_ge
		) else (
			top#bind "<Control-KeyPress-g>" call_ge;
			top#bind "<Control-KeyPress-G>" call_ge
		);
		view#add_command ~cllbck: call_sim ~acc:"Y" "Simulator";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-y>" call_sim;
			top#bind "<Command-KeyPress-Y>" call_sim
		) else (
			top#bind "<Control-KeyPress-y>" call_sim;
			top#bind "<Control-KeyPress-Y>" call_sim
		);
		
		(* Targets *)
		let target = new Tk.menu (m#path^".targets") in
		m#add_cascade "Target" target;
		let add_cmd tgt =
			let tgtstr = Err.tgtsys2str tgt in
			se.targets <- (tgtstr, tgt):: se.targets;
			target#add_command ~cllbck: (Target.set tgt) ~underl: (- 1) tgtstr
		in
		add_cmd Simulation;
		add_cmd Host;
		let makefile =
			Filename.concat (mk_path_absolute se.projectpath) "Makefile" in
		if Sys.file_exists makefile then (
			add_cmd Makefile
		);
		Puctrl.status#add_observer
			( fun () ->
						if target#entrycget "end" "label" = "Makefile" then (
							target#delete ()
						);
						
						if Sys.file_exists makefile then (
							target#add_command ~cllbck: (Target.set Makefile)
								~underl: (- 1) (Err.tgtsys2str Makefile)
						)
			);
		add_cmd Simulink;
		add_cmd Scicos;
		add_cmd Verification;
		Array.iter
			( fun f ->
						let fd = filename_concat [se.se_home;"target"; f] in
						match (Unix.stat fd).Unix.st_kind with
						| Unix.S_DIR
						-> ( match f with
									| "unix" | "win32" | "mingw"
									-> ()
									| "VerilogSim"
									-> add_cmd (VerilogSimulation)
									| _ -> if Sys.file_exists (Filename.concat fd ".setgt")
											then add_cmd (Platform f)
								)
						| _ -> ()
			) (Sys.readdir (Filename.concat se.se_home "target"));
		
		let g = new Tk.menu (m#path^".gen") in
		m#add_cascade "Make" g;
		g#add_command ~cllbck: self#make_build ~acc:"P" "Build";
		if Constant.windowingsystem() = Constant.aqua then (
			top#bind "<Command-KeyPress-b>" self#make_build;
			top#bind "<Command-KeyPress-B>" self#make_build
		) else (
			top#bind "<Control-KeyPress-b>" self#make_build;
			top#bind "<Control-KeyPress-B>" self#make_build
		);
		g#add_command ~cllbck: self#make_code "Compile only";
		
		let s = new Tk.menu (m#path^".sources") in
		m#add_cascade "Sources" ~underl: 0 s;
		Puctrl.status#add_observer (
				fun () ->
						s#delete();
						List.iter
							( fun f ->
										s#add_command ~cllbck: (self#call_edit f) ~underl: (- 1) f;
							) ( match se.project_kind with
								| CPrj -> se.sefiles@se.trace_files@se.cfiles@se.hfiles
								| VerilogPrj -> se.vfiles
							)
			);
		
		let window = new Tk.menu (m#path^".window") in
		m#add_cascade "Window" window;
		
		let h = new Tk.menu (m#path^".helpdocs") in
		m#add_cascade "Help" h;
		if not(Constant.windowingsystem() = Constant.aqua)
		then h#add_command
				~cllbck: (Error.guard (Util_tk.Dialog.about se.ge_version)) "About";
		h#add_command ~cllbck: (Error.guard Util_tk.Dialog.help) ~acc:"H" "synERJY Help";
		top#bind "<<Help>>" (Error.guard Util_tk.Dialog.help)
		;
		
		(* Buttons *)
		buttons#configure (if Constant.windowingsystem() = Constant.aqua then [] else [Option.Relief(Option.Ridge)]);
		
		button_clear#configure [Option.Cmd(P.pC)];
		button_reload#configure [Option.Cmd(self#reload_project)];
		button_build#configure [Option.Cmd(Error.guard self#make_build)];
		ide.button_sim_config <- [Option.Cmd(Error.guard call_sim)];
		button_quit #configure [Option.Cmd(self#quit)];
		
		Puctrl.status#add_observer
			( fun () ->
						if ide.button_sim_state
						then button_sim#state Constant.NotDisabled
						else button_sim#state Constant.Disabled;
						button_sim#configure ide.button_sim_config;
			);
		
		let cf = if Constant.windowingsystem() = Constant.aqua then [Option.Width(8)] else [] in
		button_clear#configure cf;
		button_reload#configure cf;
		button_build#configure cf;
		button_sim#configure cf;
		button_quit#configure cf;
		
		let f = Constant.file_join [| se.se_home;"images";"synERJY.gif" |] in
		let i = Image.create Image.Photo f in
		sE#configure [Option.Image(i)];
		
		line#configure [Option.Relief(Option.Ridge); Option.Width(2)];
		
		let pos = if Constant.windowingsystem() = Constant.aqua then [Pack.Top] else [Pack.Top; Pack.Fillx] in
		if Constant.windowingsystem() = Constant.aqua then (
			let space = new Ttk.label ".se.pu.buttons.lblspace0" in
			space#pack pos;
		);
		button_clear#pack pos;
		button_reload#pack pos;
		let space = new Ttk.label ".se.pu.buttons.lblspace1" in
		space#pack pos;
		button_build#pack pos;
		button_sim#pack pos;
		
		let space = new Ttk.label ".se.pu.buttons.lblspace2" in
		space#pack pos;
		let space = new Ttk.label ".se.pu.buttons.lblspace4" in
		space#pack pos;
		if Constant.windowingsystem() = Constant.aqua then (
			button_quit#pack pos;
		);
		sE_f#pack [Pack.Bottom; Pack.Fillx];
		sE#pack [Pack.Left];
		
		buttons#pack [Pack.Left; Pack.Filly];
		
		line#pack [Pack.Left; Pack.Filly];
		
		top_right#pack [Pack.Top; Pack.Fillx];
		top_right_i#pack [Pack.Top; Pack.Fillx];
		
		project_lbl#configure [Option.Width(7)];
		project_box#state Constant.ReadOnly;
		project_box#insert (
				match se.project with
				| None -> "no project chosen"
				| Some prj -> prj
			);
		project_box#icursor();
		Puctrl.status#add_observer
			( fun () ->
						project_box#set (
								match se.project with
								| None -> "no project chosen"
								| Some prj -> prj
							)
			);
		
		wrksp_lbl#configure [Option.Width(11)];
		wrksp_box#configure [Option.Width(30)];
		wrksp_box#state Constant.ReadOnly;
		Puctrl.status#add_observer
			( fun () ->
						wrksp_box#set se.workspace;
			);
		
		tgt_box#state Constant.ReadOnly;
		tgt_box#configure [Option.Width(20)];
		let tgts = List.rev (List.map fst se.targets) in
		tgt_box#load_items tgts;
		tgt_box#set_cmd
			( fun _ ->
						let sys = List.assoc tgt_box#get se.targets in
						Target.set sys ();
						Project.save();
						Puctrl.status#has_changed()
			);
		Puctrl.status#add_observer
			( fun () ->
						tgt_box#set (Err.tgtsys2str se.target_sys);
			);
		
		conf_box#state Constant.ReadOnly;
		conf_box#set_cmd
			( fun _ ->
						let conf_class = conf_box#get in
						Puctrl.set_conf_class (Some conf_class);
						Project.save();
						Puctrl.status#has_changed()
			);
		Puctrl.status#add_observer
			( fun () ->
						let cls = List.map c2str se.conf_classes in
						conf_box#load_items cls;
						let cc = type2id se.conf_class in
						if cls = [] then (
							conf_box#set "!! no configuration class specified !!"
						) else if List.mem cc se.conf_classes then (
							conf_box#set (c2str cc)
						) else (
							se.conf_class <- Typ(List.hd se.conf_classes,[]);
							conf_box#set (c2str cc)
						)
			);
		
		if se.sefiles <> [] then file_box#set (List.hd se.sefiles);
		file_box#state Constant.ReadOnly;
		file_box#set_cmd
			( fun _ ->
						se.se_file <- file_box#get;
						Project.save();
						Puctrl.status#has_changed()
			);
		Puctrl.status#add_observer
			( fun () ->
						file_box#load_items se.sefiles
			);
		
		button_unit#configure [Option.Text("Unit test"); Option.Cmd(Error.guard self#unit_test)];
		trace_box#configure [Option.Width(60)];
		trace_box#set_cmd
			( fun _ ->
						let file = trace_box#get in
						match se.target_sys with
						| Simulation
						| VerilogSimulation -> se.trace_file <- file
						| Scicos -> se.scicos_model <- file
						| _ -> ()
			);
		Puctrl.status#add_observer
			( fun () ->
						if (se.target_sys = Simulation ||
							se.target_sys = VerilogSimulation)
						&& se.trace_files <> [] then (
							trace_lbl#configure [Option.Text("  Trace")];
							trace_box#delete();
							if se.trace_file = "" && se.trace_files <> []
							then se.trace_file <- List.hd se.trace_files
							else ();
							trace_box#load_items se.trace_files;
							if se.trace_files <> [] then trace_box#current 0;
							Grid.forget [trace_lbl#path; trace_box#path; button_unit#path];
							Grid.forget [tool_lbl#path; tool_box#path; model_lbl#path; model_box#path; ];
							Grid.forget [logic_lbl#path; logic_box#path; spec_lbl#path; spec_box#path];
							Grid.grid [trace_lbl#path; trace_box#path; "-";"-"; button_unit#path] [Grid.Sticky "nsew" ]
						) else if se.target_sys = Scicos then (
							trace_lbl#configure [Option.Text("  Model")];
							trace_box#delete();
							if se.scicos_model = "" && se.scicos_models <> []
							then se.scicos_model <- List.hd se.scicos_models
							else ();
							trace_box#load_items se.scicos_models;
							if se.scicos_models <> [] then trace_box#current 0;
							Grid.forget [tool_lbl#path; tool_box#path; model_lbl#path; model_box#path; ];
							Grid.forget [logic_lbl#path; logic_box#path; spec_lbl#path; spec_box#path];
							Grid.forget [trace_lbl#path; trace_box#path; button_unit#path];
							Grid.grid [trace_lbl#path; trace_box#path; "-";"-";"-"] [Grid.Sticky "nsew" ]
						) else (
							Grid.forget [trace_lbl#path; trace_box#path; button_unit#path]
						)
			);
		
		(* verification *)
		let tools = List.rev_map fst se.logics in
		tool_box#load_items tools;
		tool_box#set (if tools <> [] then List.hd tools else "");
		
		let models = self#models() in
		tool_box#load_items models;
		model_box#set (if models <>[] then List.hd models else "");
		
		logic_box#load_items [];
		
		self#ins_logics;
		tool_box#set_cmd (fun _ -> self#ins_logics);
		model_box#set_cmd (fun _ -> self#ins_specs);
		logic_box#set_cmd (fun _ -> self#ins_specs);
		
		Puctrl.status#add_observer
			( fun () ->
						if se.target_sys = Verification then (
							Grid.forget [tool_lbl#path; tool_box#path;
								model_lbl#path; model_box#path; ];
							Grid.forget [trace_lbl#path; trace_box#path; button_unit#path];
							Grid.grid [tool_lbl#path; tool_box#path;
								model_lbl#path; model_box#path;"-"]
								[Grid.Sticky "nsew" ];
							Grid.grid [logic_lbl#path; logic_box#path;
								spec_lbl#path; spec_box#path;"-"]
								[Grid.Sticky "nsew" ]
						)
			);
		
		Grid.grid [project_lbl#path; project_box#path;
			wrksp_lbl#path; wrksp_box#path;"-"] [Grid.Sticky "nsew" ];
		Grid.grid [tgt_lbl#path; tgt_box#path;
			conf_lbl#path; conf_box#path;"-"] [Grid.Sticky "nsew" ];
		Grid.grid [file_lbl#path; file_box#path;"-";"-";"-"]
			[Grid.Sticky "nsew" ];
		Grid.columnconfigure top_right_i#path wrksp_box#path [Option.Weight(1)];
		
		log#bind "<<Cut>>" log#cut;
		log#bind "<<Copy>>" log#copy;
		log#bind "<<Paste>>" log#paste;
		log#configure [ Option.Height(20); Option.Wrap("word"); Option.Bd(1) ];
		bs_ps := (fun s -> log#insert s; log#see "end");
		bs_clr := log#delete;
		bs_flush := (fun () -> log#see "end");
		let log_with_scroll = new Util_tk.widget_with_scrolls
				(log:> Tk.scrollable_widget) ".se.pu.log" in
		log_with_scroll#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
		
		pu_frame#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
		
		Wm.deiconify top#path;
		
		top#raise;
		top#focus;
    Wm.zoom top#path;
		Wm.attributes ".se" "topmost" "1";
		
		( try
			let prj_file = Filename.concat pwd "_.seprj" in
			if Sys.file_exists prj_file then (
				Project.load ~dir: (Some pwd) ();
				Target.set se.target_sys ()
			);
		with _ -> ()
		);
		
		Puctrl.status#has_changed();
		se.open_directory <- se.projectpath;
		se.save_directory <- se.projectpath;
		
	)
	
end
;;

(* =========================================================================
synERJY console startup
========================================================================= *)
let init_se () =
	let pwd = Sys.getcwd() in
	Puctrl.reset ();
	( try
		se.cmdline_files <- [];
		parse_cmdline_args ();
		if se.batchmode then (
			Puctrl.process_cmd_file se.cmd_file
		) else (
		);
	with _ ->
			if se.batchmode then (
				exit 1
			) else (
			(* start the gui to recover from error interactively *)
			)
	);
	if se.batchmode then (
		exit 0
	) else (
		Util_tk.tk_start ~window_name:"" ~class_name:"" "";
		
		if Constant.version() < "8.5" then (
			P.ps "Sorry, synERJY requires Tcl/Tk version 8.5 at least";
			Constant.destroy "."
		) else (
			Wm.withdraw ".";
			let serc = Filename.concat se.home ".serc" in
			if Sys.file_exists serc then (
				Puctrl.process_cmd_file serc
			) else (
				let serc = Filename.concat se.se_home ".serc" in
				if Sys.file_exists serc
				then Puctrl.process_cmd_file serc
				else ()
			);
			if not (Sys.file_exists se.workspace) then (
				se.workspace <- se.home
			);
			Unix.chdir se.workspace;
			se.open_directory <- se.workspace;
			se.save_directory <- se.workspace;
			se.projectpath <- se.workspace;
			
			if se.cmd_file = "" then (
			(* no further resource file specified *)
			) else (
				Puctrl.process_pure_cmd_file se.cmd_file
			);
			
			Util_tk.mk_environment ();
			let _, se_size, se_weight = se.se_font in
			Font.set Font.se_font se_size se_weight;
			
			P.start_output2wdw (!bs_ps) (!bs_clr) (!bs_flush);
			List.iter Puctrl.add_file_without_typecheck se.cmdline_files;
			ignore (new synerjy_console pwd);
			Util_tk.tk_main_loop ();
			Puctrl.status#clear_observers ();
			P.stop_output2wdw ()
		)
	)

let _ = Callback.register "init_se" init_se