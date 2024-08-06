open Ast
open Tk

(* ==========================================================================
composite widget classes
========================================================================== *)
class bordered_frame ?(left = 0) ?(right = 0) ?(top = 0) ?(bottom = 0) path =

object(self)
  inherit Ttk.composite_widget path
  
  val _top = new Ttk.frame (path^"._top")
  val _bottom = new Ttk.frame (path^"._bottom")
  val _left = new Ttk.frame (path^"._left")
  val _right = new Ttk.frame (path^"._right")
  
  initializer
  _top#configure [Option.Height(top)];
  _top#pack [Pack.Top; Pack.Fillx];
  _bottom#configure [Option.Height(bottom)];
  _bottom#pack [Pack.Bottom; Pack.Fillx];
  _left#configure [Option.Width(left)];
  _left#pack [Pack.Left; Pack.Filly];
  _right#configure [Option.Width(right)];
  _right#pack [Pack.Right; Pack.Filly];
end

(* ------------------------------------------------------------------------- *)
class widget_with_scrolls (widget: Tk.scrollable_widget) path =
  
  let frame = path^"._frame"
  and xscroll = path^"._xscroll"
  and yscroll = path^"._yscroll" in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val frame = new Ttk.frame frame
      val xscroll = new Ttk.scrollbar ~orientation:"horizontal" widget xscroll
      val yscroll = new Ttk.scrollbar ~orientation:"vertical" widget yscroll
      
      method xscroll = xscroll
      method yscroll = yscroll
      
      initializer
      widget#configure [Option.Width(40); Option.Height(20);
        Option.Xscroll(xscroll#path); Option.Yscroll(yscroll#path)];
      Grid.grid [frame#path]
        [Grid.Row(0); Grid.Column(0); Grid.Sticky "nsew" ];
      Grid.grid [yscroll#path]
        [Grid.Row(0); Grid.Column(1); Grid.Sticky "nsew" ];
      Grid.grid [xscroll#path]
        [Grid.Row(1); Grid.Column(0); Grid.Sticky "nsew" ];
      Grid.columnconfigure path "0" [ Option.Weight(1) ];
      Grid.rowconfigure path "0" [ Option.Weight(1) ];
      widget#pack [Pack.In(frame#path); Pack.Fillxy; Pack.Expand];
      widget#raise
    end

class widget_with_xscroll (widget: Tk.scrollable_widget) path =
  
  let frame = path^"._frame"
  and xscroll = path^"._xscroll" in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val frame = new Ttk.frame frame
      val xscroll = new Ttk.scrollbar ~orientation:"horizontal" widget xscroll
      
      method xscroll = xscroll
      
      initializer (
        widget#configure [Option.Xscroll(xscroll#path)];
        widget#pack [Pack.In(frame#path)];
        Grid.grid [frame#path] [Grid.Row(0); Grid.Column(0); Grid.Sticky "nsew" ];
        Grid.grid [xscroll#path] [Grid.Row(1); Grid.Column(0); Grid.Sticky "nsew" ];
        Grid.rowconfigure path "0" [ Option.Weight(1) ];
        Grid.columnconfigure path "0" [ Option.Weight(1) ];
        widget#raise
      )
      
    end

class widget_with_yscroll (widget: Tk.scrollable_widget) path =
  
  let frame = path^"._frame"
  and yscroll = path^"._yscroll" in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val frame = new Ttk.frame frame
      val yscroll = new Ttk.scrollbar ~orientation:"vertical" widget yscroll
      
      method yscroll = yscroll
      
      initializer (
        widget#configure [Option.Yscroll(yscroll#path)];
        widget#pack [Pack.In(frame#path); Pack.Fillxy; Pack.Expand];
        Grid.grid [frame#path] [Grid.Row(0); Grid.Column(0); Grid.Sticky "nsew" ];
        Grid.grid [yscroll#path] [Grid.Row(0); Grid.Column(1); Grid.Sticky "nsew" ];
        Grid.rowconfigure path "0" [ Option.Weight(1) ];
        Grid.columnconfigure path "0" [ Option.Weight(1) ];
        widget#raise
      )
      
    end

(*
(* ------------------------------------------------------------------------- *)
class tk_label_frame text path =

let frame = path^"._frame"
and label = path^"._frame._label"
and contain = path^"._contain" in

object(self)
inherit Ttk.composite_widget path

val _frame = new Ttk.frame frame
val _label = new Ttk.label ~text: text label
val _contain = new Ttk.frame contain
method frame = _frame
method contain = _contain

initializer
_label#pack [Left];
_frame#pack [Top; Fillx];

_contain#pack [Top];
_contain#configure [Relief(Sunken)]
end
*)

(* ------------------------------------------------------------------------- *)
class labelled_entry text width path =
  
  let label = path^"._label"
  and entry = path^"._entry" in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val _label = new Ttk.label ~text: text label
      val _entry = new Ttk.entry width entry
      
      method label = _label
      method entry = _entry
      
      initializer (
        _label#configure [Option.Anchor("w")];
        _label#pack [Pack.Left];
        _entry#pack [Pack.Left; Pack.Fillx; Pack.Expand]
      )
    end

(* ------------------------------------------------------------------------- *)
class labelled_widget (widget: Tk.widget) text width path =
  
  let label = path^"._label"
  and frame = path^"._entry" in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val label = new Ttk.label ~text: text label
      val frame = new Ttk.frame frame
      
      initializer
      label#configure [Option.Anchor("nw"); Option.Width(width)];
      label#pack [Pack.Left];
      frame#pack [Pack.Left; Pack.Fillx; Pack.Expand];
      widget#pack [Pack.In(frame#path); Pack.Fillx; Pack.Expand]
    end

(* ------------------------------------------------------------------------- *)
class dir_entry text width ?(cmd = fun() -> ()) ?(base = false) path =

object(self)
  inherit labelled_entry text width path
  
  val frame = new Ttk.frame (path^"._space3")
  val button = new Ttk.button ~text:"Browse" (path^"._button")
  
  method button = button
  
  method browse () =
    let file = Dialog.chooseDirectory ~parent: path "Set directory" in
    let file = if base then Filename.basename file else file in
      if file <> "" then (
        self#entry#delete();
        self#entry#insert file;
        cmd()
      )
  
  initializer (
    button#configure [Option.Cmd(self#browse)];
    frame#configure [Option.Width(10)];
    frame#pack [Pack.Left];
    button#pack [Pack.Left]
  )
  
end
(*
(* ------------------------------------------------------------------------- *)
class tk_labelled_combobox text path =

let label = path^"._label"
and combo = path^"._combo" in

object(self)
inherit Ttk.composite_widget path

val _label = new Ttk.label ~text: text label
val _combo = new Ttk.combobox combo

method label = _label
method combobox = _combo

initializer
_label#configure [Anchor("w")];
_label#pack [Left];
_combo#pack [Left; Pack.Fillxy; Expand];
end
*)
(* ------------------------------------------------------------------------- *)
class filebox ?(height = 0) ?(width = 0) path =

object(self)
  inherit Ttk.composite_widget path
  
  val listbox = new Tk.listbox (path^".listbox")
  val buttons = new Ttk.frame (path^".buttons")
  val button_plus = new Ttk.label ~text:" + " (path^".buttons.plus")
  val button_minus = new Ttk.label ~text:" - " (path^".buttons.minus")
  
  val mutable suffix = ""
  val mutable add_act = fun x -> None
  val mutable del_act = fun x -> ()
  
  method set_suffix s = suffix <- s
  method set_add cmd = add_act <- cmd
  method set_del cmd = del_act <- cmd
  
  method insert_list l =
    listbox#delete ();
    listbox#insert_list l
  
  method listbox = listbox
  
  method add_file () =
    button_plus#configure [Option.Relief(Option.Raised)];
    let file =
      Dialog.getOpenFile
        ~parent: path
        ~filetypes: (suffix^" {{All Files} {*}}")
        "Set file" in
      if file <> "" then (
        let fn = add_act file in
          match fn with
          | None -> ()
          | Some fn ->
              listbox#insert fn;
              listbox#sort
      );
      button_plus#configure [Option.Relief(Option.Groove)]
  
  method delete () =
    button_plus#configure [Option.Relief(Option.Raised)];
    let f = listbox#select () in
      del_act f;
      listbox#delete_selection ();
      button_plus#configure [Option.Relief(Option.Groove)]
  
  initializer
  let listbox_with_scroll =
    new widget_with_yscroll
      (listbox:> Tk.scrollable_widget) (path^".filetbox") in
    self#configure[Option.Takefocus(1)];
    listbox#configure [Option.Selectmode("extended"); Option.Exportselection(1)];
    button_plus#bind Constant.b1rel self#add_file;
    button_plus#configure [Option.Relief(Option.Groove)];
    button_minus#bind Constant.b1rel self#delete;
    button_minus#configure [Option.Relief(Option.Groove)];
    if width > 0 then listbox#configure [Option.Width(width)];
    if height > 0 then listbox#configure [Option.Height(height)];
    
    button_plus#pack [Pack.Left];
    button_minus#pack [Pack.Left];
    buttons#pack [Pack.Bottom; Pack.Fillx; Pack.Expand];
    listbox_with_scroll#pack [Pack.Top; Pack.Fillx; Pack.Expand];
  
end
(* -------------------------------------------------------------------------
BROWSER WIDGET
------------------------------------------------------------------------- *)
(* class virtual ['item] c_browser_widget path =

let browser_with_scrolls = path^"._browser_with_scrolls"
and browser = path^"._browser" in

object(self)
inherit Ttk.composite_widget path

(* interface functions *)

method virtual get_childs :'item -> 'item list
method virtual get_id :'item -> string
method virtual get_name :'item -> string
method virtual get_full_name :'item -> string
method virtual get_type :'item -> string
method virtual set_open :'item -> unit
method virtual set_closed :'item -> unit
method virtual is_open :'item -> bool
method virtual popup :'item -> string array -> unit

val browser = new Tk.text browser

method highlight o yes =
if yes
then browser#tag_configure (self#get_id o) [Fg("red")]
else browser#tag_configure (self#get_id o) [Fg("blue")]

method trigger_expand o pos =
let id = self#get_id o in
let index = browser#index pos.(1) pos.(2) in
let line = Constant.line_of_index index in
let column = Constant.column_of_index index in
browser#delete_one index;
let tag = "shr"^id in
browser#insert ~index: index ~tag: tag "-";
tag_bind browser#path tag "<Button-1>" tk_Wxy (self#trigger_shrink o);
self#set_open o;
List.fold_right
( fun o () -> self#expand o line (column + 3)
) (self#get_childs o) ()

method expand o line column =
let id = self#get_id o in
let l = string_of_int (line + 1) and c = string_of_int column in
browser#insert ~index: ((string_of_int line)^".end")
("\n"^(String.make column ' '));
if self#get_childs o = [] then (
browser#insert ~index: (l^".end") ""
) else (
(* check whether the list of calls is expanded or not *)
if not (self#is_open o) then (
let tag = "exp"^id in
browser#insert ~index: (l^".end") ~tag: tag "+";
tag_bind browser#path tag "<Button-1>" tk_Wxy (self#trigger_expand o);
) else (
let tag = "shr"^id in
browser#insert ~index: (l^"._"^c) ~tag: (tag^" prop") "-";
tag_bind browser#path tag "<Button-1>" tk_Wxy (self#trigger_shrink o)
)
);
browser#insert ~index: (l^".end") "  ";
browser#insert ~index: (l^".end") ~tag: id (self#get_name o);
browser#insert ~index: (l^".end") (self#get_type o);
browser#tag_configure id [Fg("blue")];
tag_bind browser#path id "<Button-2>" tk_Wxy (self#popup o);
tag_bind browser#path id "<Control-Button-1>" tk_Wxy (self#popup o);
if self#is_open o
then
List.fold_right
( fun o () -> self#expand o (line + 1) (column + 3)
) (self#get_childs o) ()

method trigger_shrink o pos =
let id = self#get_id o in
let index = browser#index pos.(1) pos.(2) in
let line = Constant.line_of_index index in
browser#delete_one index;
let tag = "exp"^id in
browser#insert ~index: index ~tag: tag "+";
tag_bind browser#path tag "<Button-1>" tk_Wxy (self#trigger_expand o);
self#set_closed o;
List.iter (fun o -> self#shrink o line) (self#get_childs o)

method shrink o line =
let l = string_of_int line in
browser#delete ~first: (l^".end") ~last: ((string_of_int (line + 1))^".end") ();
List.iter (fun o -> self#shrink o line) (self#get_childs o)

method clear =
browser#delete ()

initializer
let browser_with_scrolls = new Tk.widget_with_scrolls
(browser:> Tk.scrollable_widget)
browser_with_scrolls in
browser#configure [Bg("white"); Height(20); Width(20);
TkState(Constant.Disabled)];
browser_with_scrolls#pack [Pack.Fillxy; Expand];

end
*)

class virtual ['item] c_browser_widget path =

object(self)
  inherit Tk.widget path
  
  (* interface functions *)
  method virtual set_obj_id : 'item -> string -> unit
  method virtual get_obj_id : 'item -> string
  method virtual is_root : 'item -> bool
  method virtual get_children : 'item -> 'item list
  method virtual get_state_id : 'item -> string
  method virtual get_name : 'item -> string
  method virtual get_full_name : 'item -> string
  method virtual get_type : 'item -> string
  method virtual set_open : 'item -> unit
  method virtual set_closed : 'item -> unit
  method virtual is_open : 'item -> bool
  method virtual popup : 'item -> string array -> unit
  
  val browser = new Ttk.treeview path
  
  val mutable roots = []
  
  method column = browser#column
  
  method highlight o yes =
    if yes
    then browser#tag_configure (self#get_state_id o) [Option.Fg("red")]
    else browser#tag_configure (self#get_state_id o) [Option.Fg("blue")]
  
  method populate root o =
    List.iter
      ( fun o ->
            let id = browser#insert root "end" [Option.Text(self#get_name o)] in
              self#set_obj_id o id;
              self#populate id o
      ) (self#get_children o)
  
  method populate_root o =
    let root = browser#insert "" "end" [Option.Text(self#get_name o)] in
      self#set_obj_id o root;
      roots <- root:: roots;
      self#populate root o
  
  method delete o =
    browser#delete [self#get_obj_id o]
  
  method delete_all () =
    browser#delete roots;
    roots <- []
  
  initializer
  browser#heading "#0" [Option.Text("States")];
  
end

module Dialog =

struct
  include Dialog
  
  let getOpenFile ?(ext ="") ?(initdir ="") ?(initfile ="") ?(parent ="") ?(filetypes ="") title =
    let f = getOpenFile ~ext: ext ~initdir: initdir ~initfile: initfile ~parent: parent ~filetypes: filetypes title in
      if Sys.file_exists f then se.open_directory <- Filename.dirname f;
      f
  
  let chooseDirectory ?(parent ="") ?(initdir ="") title =
    let d = chooseDirectory ~parent: parent ~initdir: initdir title in
      if Sys.file_exists d then se.open_directory <- d;
      d
  
  let getSaveFile ?(ext ="") ?(initdir ="") ?(initfile ="") ?(parent ="") title =
    let f = getSaveFile ~ext: ext ~initdir: initdir ~initfile: initfile ~parent: parent title in
      if Sys.file_exists f then se.save_directory <- Filename.dirname f;
      f
  
  let getSaveFile2 ?(ext ="") ?(initdir ="") ?(initfile ="") ?(parent ="") ?(filetypes ="") title =
    let f = getSaveFile2 ~ext: ext ~initdir: initdir ~initfile: initfile ~parent: parent ~filetypes: filetypes title in
      if Sys.file_exists f then se.save_directory <- Filename.dirname f;
      f
  
  let about tool () =
    ignore (messageBox ~default:"ok" "ok" "info"
          (tool^"\n\nCopyright:\n \
            Fraunhofer IAIS\n 2000 - 2008\n \
            Reinhard Budde\n \
            \ \ http:// www.iais.fraunhofer.de / 504.html\n \
            Axel Poigne\n \
            \ \ http:// www.iais.fraunhofer.de / 1109.html\n \
            Karl - Heinz Sylla\n \
            \ \ http:// www.iais.fraunhofer.de / 1119.html\n\n")
      )
  
  let help () =
    ignore (messageBox ~default:"ok" "ok" "info"
          ("No online help provided yet.\n \
            Please have a look at our manuals.\n")
      )
end

(* ==========================================================================
saving preferences
========================================================================== *)
let save_preferences () =
  let out = open_out (Filename.concat se.home ".serc") in
  let os str = output_string out str in
  
    os "\n// font used for the console window\n";
    let f, s, w = se.se_font in
      os "set font size              = "; os s; os ";\n";
      os "set font weight            = "; os w; os ";\n";
      
      os "\n// font used for the simulator windows\n";
      let f, s, w = se.sim_font in
        os "set simulator font size    = "; os s; os ";\n";
        os "set simulator font weight  = "; os w; os ";\n";
        
        os "\n// font used for the graphic editor windows\n";
        let f, s, w = se.ge_font in
          os "set graphic font size      = "; os s; os ";\n";
          os "set graphic font weight    = "; os w; os ";\n";
          
          os "\n// settings for the compiler and the main environment\n";
          os "set file prefix            = "; os se.file_prefix; os ";\n";
          
          os "\n// settings for the graphic editor\n";
          os "set graphic width          = "; os (string_of_int se.ge_width); os ";\n";
          os "set graphic height         = "; os (string_of_int se.ge_height); os ";\n";
          os "set print size             = "; os se.print_format; os ";\n";
          os "set graphic file           = "; os se.ge_file; os ";\n\n";
          os "\n";
          os "set code style             = "; os se.code_sty; os ";\n";
          os "set parallel port          = "; os se.par_port; os ";\n";
          
          if se.editor <> "" then (
            os "\nset editor = "; os se.editor; os ";\n"
          );
          
          os "\nset timescale = "; os (string_of_int se.timescale); os ";\n";
          
          if Sim_type.sim.Sim_type.obj_brw then os "\nshow object browser;\n";
          if Sim_type.sim.Sim_type.trace_brw then os "show trace browser;\n\n";
          if se.workspace <> "" then (
            os "set workspace              = "; os se.workspace; os ";\n";
          );
          
          if se.matlab_dir <> "" then (
            os "set Matlab directory       = "; os se.matlab_dir; os ";\n\n"
          );
          if se.scilab_dir <> "" then (
            os "set Scilab directory       = "; os se.scilab_dir; os ";\n"
          );
          if se.nu_smv_dir <> "" then (
            os "set NuSMV directory        = "; os se.nu_smv_dir; os ";\n"
          );
          close_out out

(* ==========================================================================
definition of the TK environment
========================================================================== *)
let mk_environment () =
  
  let _, se_size, se_weight = se.se_font
  and _, ge_size, ge_weight = se.ge_font
  and _, sim_size, sim_weight = se.sim_font in
    ( match Constant.windowingsystem() with
      | "win32"
      -> Font.create Font.se_font "Courier New" se_size se_weight;
          Font.create Font.ge_font "Courier New" ge_size ge_weight;
          Font.create Font.sim_font "Courier New" sim_size sim_weight
      | "aqua"
      -> Font.create Font.se_font "Monaco" se_size se_weight;
          Font.create Font.ge_font "TkTextFont" ge_size ge_weight;
          Font.create Font.sim_font "Monaco" sim_size sim_weight
      | "x11"
      -> Font.create Font.se_font "Courier" se_size se_weight;
          Font.create Font.ge_font "Courier" ge_size ge_weight;
          Font.create Font.sim_font "Courier" sim_size sim_weight
      | _ -> Err.intern "tk_env"
    );
    let add_option option value = tK [| "option";"add"; option; value |] in
      add_option "*tearOff" "0";
      add_option "*All.background" Constant.background;
      add_option "*Frame.background" Constant.background;
      add_option "*Label.background" Constant.background;
      add_option "*Button.background" Constant.background;
      add_option "*RadioButton.background" Constant.background;
      add_option "*Text.background" "White";
      add_option "*Text.relief" "ridge";
      add_option "*Text.borderwidth" "1";
      add_option "*Text.selectbackground" "AlliceBlue";
      add_option "*Listbox.background" "White";
      add_option "*Listbox.relief" "ridge";
      add_option "*Listbox.borderwidth" "1";
      add_option "*Listbox.selectbackground" "AlliceBlue";
      add_option "*Entry.background" "White";
      add_option "*Entry.relief" "ridge";
      add_option "*Entry.borderwidth" "1";
      add_option "*Entry.selectbackground" "AlliceBlue";
      add_option "*Canvas.background" "White";
      add_option "*Canvas.relief" "ridge";
      add_option "*Canvas.borderwidth" "1";
      add_option "*Canvas.selectbackground" "AlliceBlue";
      add_option "*Menu.font" Font.sys_font;
      
      let add_controlkey_event event value =
        if Constant.windowingsystem() = "aqua"
        then tK [| "event";"add"; event;"<M1-KeyPress-"^value^">" |]
        else tK [| "event";"add"; event;"<Control-KeyPress-"^value^">" |] in
        add_controlkey_event "<<New>>" "n";
        add_controlkey_event "<<New>>" "N";
        add_controlkey_event "<<Open>>" "o";
        add_controlkey_event "<<Open>>" "O";
        add_controlkey_event "<<Save>>" "s";
        add_controlkey_event "<<Save>>" "S";
        add_controlkey_event "<<Window>>" "w";
        add_controlkey_event "<<Window>>" "W";
        add_controlkey_event "<<Print>>" "p";
        add_controlkey_event "<<Print>>" "P";
        add_controlkey_event "<<Quit>>" "q";
        add_controlkey_event "<<Quit>>" "Q";
        add_controlkey_event "<<Undo>>" "z";
        add_controlkey_event "<<Undo>>" "Z";
        add_controlkey_event "<<Redo>>" "y";
        add_controlkey_event "<<Redo>>" "Y";
        add_controlkey_event "<<Copy>>" "c";
        add_controlkey_event "<<Copy>>" "C";
        add_controlkey_event "<<Paste>>" "v";
        add_controlkey_event "<<Paste>>" "V";
        add_controlkey_event "<<Cut>>" "x";
        add_controlkey_event "<<Cut>>" "X"

let tk_start ?(window_name ="OcamlTk") ?(class_name ="OcamlTk") display =
  let display =
    if display = ""
    then
      try Sys.getenv "DISPLAY"
      with _ -> ":0.0"
    else display
  in
  let executable = Sys.argv.(0) in
    main_window display window_name class_name executable

let tk_main_loop () = main_loop ()