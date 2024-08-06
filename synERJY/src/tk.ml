(* --------------------------------------------------------------------------
error_log ... holds error - info, which is entered by the fun ...
error_push e ... puts ''e'' into the error log (RB)
-------------------------------------------------------------------------- *)
module Error =

struct
  let log = ref ([] : string list)
  let push e = log := e::!log
  let raise e = push e; raise (Failure e)
  
  let print () =
    if !log = [] then (
    ) else (
      P.ps "THE error_log CONTAINS:\n";
      List.iter (fun s -> P.ps s; P.pn ()) !log
    )
  
  let guard f x =
    try f x
    with
    | Ly.Error e ->
        P.pn (); P.ps e; P.pn (); P.pF ();
        raise ""
    | e ->
        raise ("\nSystem error: exception was "^(Printexc.to_string e)^"\n")
end

(* ==========================================================================
connection to tcl / tk via C - functions contained in file "ocamltk.c"
========================================================================== *)
external main_window: string -> string -> string -> string -> unit = "c_MainWindow"
external create_timer: unit -> unit = "c_CreateTimer"
external main_loop: unit -> unit = "c_MainLoop"
external one_event: unit -> unit = "c_OneEvent"
external tk: string array -> string = "c_TkEval"

let tcl_cmds = ( (Hashtbl.create 23) :
  (string, string array -> unit) Hashtbl.t )

(* ==========================================================================
called from Constant. Extracts the closure to be called using hash table tcl_cmds
returns the result string
========================================================================== *)
let tk2ocaml2tk name args =
  try
      (Hashtbl.find tcl_cmds name) args;
      ""
  with
  | Not_found -> "tk2ocaml2tk: unknown "^name
  | _ -> "tk2ocaml2tk: error in "^name

let _ = Callback.register "tk2ocaml2tk" tk2ocaml2tk

(* ==========================================================================
registers the commands that can be called from Tk in the hash table tcl_cmds
- pure callbacks [button pressed, e.g.]
- binding event sequences [RETURN key pressed inside a graphic canvas]
remark: tags [from TclTk] are called widgets here,
sequence [from TclTk] are called event here
========================================================================== *)
let tK c = ignore(tk c)
let tL sl = tK (Array.of_list sl)
let tk x = tk x

let int_of_tk a =
  int_of_string (tk a)
let bool_of_tk a =
  tk a = "1"
(*let float_of_tk a =     *)
(*	float_of_string (tk a)*)

let mk_fnid =
  let fnid = ref 0 in
    fun () -> incr fnid; string_of_int (!fnid)

let callback (f : string array -> unit) =
  let name = "_c_"^(mk_fnid()) in
    Hashtbl.add tcl_cmds name f;
    "ocaml " ^ name

let callbackUnit (f : unit -> unit) =
  let name = "_c_"^(mk_fnid()) in
    Hashtbl.add tcl_cmds name (fun _ -> f ());
    "ocaml " ^ name

let tag_bind wdgt tag event pattern (f : string array -> unit) =
  let pattern = if pattern = "" then pattern else " "^pattern in
  let name = "_bind_" ^ (mk_fnid ()) in
    Hashtbl.add tcl_cmds name f;
    tK [| wdgt; "tag"; "bind"; tag; event; "ocaml "^name^pattern |]

let bind wdgt event ?(pattern ="") (f : string array -> unit) =
  let name = "_bind_" ^ (mk_fnid ()) in
    Hashtbl.add tcl_cmds name f;
    tK [| "bind"; wdgt; event; "ocaml "^name^" "^pattern |]

let canvas_bind cv wdgt event pattern (f : string array -> unit) =
  let pattern = if pattern = "" then pattern else " "^pattern in
  let name = "_bind_" ^ (mk_fnid ()) in
    Hashtbl.add tcl_cmds name f;
    tK [| cv;"bind"; wdgt; event; "ocaml "^name^pattern |]

(* ==========================================================================
tk basics
========================================================================== *)

(* --------------------------------------------------------------------------
Tk constants
-------------------------------------------------------------------------- *)
module Constant =

struct
  let wxy = "%W %x %y"
  
  let b1press = "<ButtonPress-1>"
  let b1rel = "<ButtonRelease-1>"
  let brel = "<ButtonRelease>"
  let b1motion = "<Button1-Motion>"
  let shiftB1press = "<Shift-ButtonPress-1>"
  let shiftB1rel = "<Shift-ButtonRelease-1>"
  let ctrlB1press = "<Control-ButtonPress-1>"
  let ctrlB1rel = "<Control-ButtonRelease-1>"
  let doubleB1press = "<Double-ButtonPress-1>"
  let doubleB1rel = "<Double-ButtonRelease-1>"
  let shiftDoubleB1press = "<Shift-Double-ButtonRelease-1>"
  let shiftDoubleB1rel = "<Shift-Double-ButtonRelease-1>"
  
  let b2press = "<ButtonPress-2>"
  let b2rel = "<ButtonRelease-2>"
  let shiftB2press = "<Shift-ButtonPress-2>"
  let shiftB2rel = "<Shift-ButtonRelease-2>"
  let ctrlB2press = "<Control-ButtonPress-2>"
  let ctrlB2rel = "<Control-ButtonRelease-2>"
  let doubleB2press = "<Double-ButtonPress-2>"
  let doubleB2rel = "<Double-ButtonRelease-2>"
  let shiftDoubleB2press = "<Shift-Double-ButtonRelease-2>"
  let shiftDoubleB2rel = "<Shift-Double-ButtonRelease-2>"
  
  let b3press = "<ButtonPress-3>"
  let b3rel = "<ButtonRelease-3>"
  let shiftB3press = "<Shift-ButtonPress-3>"
  let shiftB3rel = "<Shift-ButtonRelease-3>"
  let ctrlB3press = "<Control-ButtonPress-3>"
  let ctrlB3rel = "<Control-ButtonRelease-3>"
  let doubleB3press = "<Double-ButtonPress-3>"
  let doubleB3rel = "<Double-ButtonRelease-3>"
  let shiftDoubleB3press = "<Shift-Double-ButtonRelease-3>"
  let shiftDoubleB3rel = "<Shift-Double-ButtonRelease-3>"
  
  let return = "<Return>"
  
  let life_cycle = "life_cycle"
  let popup = ".popup"
  
  let notebookTabChanged = "<<NotebookTabChanged>>"
  
  let treeviewOpen = "<<TreeviewOpen>>"
  let treeviewClose = "<<TreeviewClose>"
  let treeviewSelect = "<<tk_TreeviewSelect>>"
  
  let _new = "<<New>>"
  let _open = "<<Open>>"
  let save = "<<Save>>"
  let print = "<<Print>>"
  let quit = "<<Quit>>"
  
  let aqua = "aqua"
  let win32 = "win32"
  let x11 = "x11"
  
  type state_t =
    | Active | NotActive
    | ReadOnly | NotReadOnly
    | Disabled | NotDisabled
  
  let state2str = function
    | Active -> "active" | NotActive -> "!active"
    | ReadOnly -> "readonly" | NotReadOnly -> "!readonly"
    | Disabled -> "disabled" | NotDisabled -> "!disabled"
  
  let str2state = function
    | "active" -> Active | "!,active" -> NotActive
    | "readonly" -> ReadOnly | "!readonly" -> NotReadOnly
    | "disabled" -> Disabled | "!disabled" -> NotDisabled
    | _ -> assert false
  
  let version () =
    tk [| "set";"tcl_version" |]
  let os () =
    tk [| "set";"platform(os)" |]
  
  let package_require pkg =
    tK [| "package";"require"; pkg |]
  
  let background = "LightBlue"
  
  let rec string2list s =
    if s = "" then [] else
    if String.get s 0 = ' '
    then ( string2list (String.sub s 1 ((String.length s) - 1)) )
    else ( try let i = String.index s ' ' in
          (String.sub s 0 i)::
          (string2list (String.sub s (i + 1) ((String.length s) - i - 1)))
    with Not_found -> [s] )
  
  let list s =
    string2list s
  let array s =
    Array.of_list (string2list s)
  
  let cancelRepeat () =
    tK [| "tk::CancelRepeat" |]
  
  let set var value =
    tK [| "set"; var; value |]
  
  let setPrivX p =
    tK [| "set";"tk::Priv(x)"; p |]
  let setPrivY p =
    tK [| "set";"tk::Priv(y)"; p |]
  let entryButton1 w x =
    tK [| "tk::EntryButton1"; w; x |]
  let entryMouseSelect w x =
    tK [| "tk::EntryMouseSelect"; w; x |]
  
  let windowingsystem () =
    tk [| "tk";"windowingsystem" |]
  
  let line_of_index i =
    int_of_string (String.sub i 0 (String.index i '.'))
  
  let column_of_index index =
    let i = (String.index index '.') + 1 in
      int_of_string (String.sub index i ((String.length index) - i))
  
  let file_join l = tk (Array.append [|"file";"join" |] l)
  
  let destroy w = tK [| "destroy"; w |]
  
  let show_preferences cmd =
    tK [| "proc"; "::tk::mac::ShowPreferences"; " "; callbackUnit cmd |]
  
  let show_about cmd =
    tK [| "proc"; "tk::mac::standardAboutPanel"; " "; callbackUnit cmd |]
  
  let show_help cmd =
    tK [| "proc"; "::tk::mac::ShowHelp"; " "; callbackUnit cmd |]
end

(* ==========================================================================
options
========================================================================== *)

module Option =

struct
  type relief_t =
    | Raised
    | Sunken
    | Flat
    | Ridge
    | Solid
    | Groove
  
  let string_of_relief =
    function
    | Raised -> "raised"
    | Sunken -> "sunken"
    | Flat -> "flat"
    | Ridge -> "ridge"
    | Solid -> "solid"
    | Groove -> "groove"
  
  type options_t =
    | Activebackground of string
    | Arrow of string
    | Anchor of string
    | Bd of int
    | Bg of string
    | Borderwidth of int
    | Cmd of (unit -> unit)
    | CPadx of string
    | CPady of string
    | Cursor of string
    | Default of Constant.state_t
    | Exportselection of int
    | Fg of string
    | Fill of string
    | Font of string
    | Height of int
    | Highlightthickness of int
    | Highlightbackground of string
    | Highlightcolor of string
    | Indicatoron of bool
    | Id of string
    | Image of string
    | Justify of string
    | Menu of string
    | Minsize of int
    | Padding of int
    | Open of bool
    | Outline of string
    | Offrelief of string
    | Overrelief of string
    | Relief of relief_t
    | Scrollregion of int * int * int * int
    | Selectcolor of string
    | Selectmode of string
    | Setgrid of bool
    | Smooth
    | Sticky of string
    | Style of string
    | Tags of string list
    | Takefocus of int
    | Tearoff
    | Text of string
    | TkState of Constant.state_t
    | Uniform of string
    | Value of string
    | Values of string list
    | Wrap of string
    | Width of int
    | Window of string
    | Weight of int
    | Xscroll of string
    | Yscroll of string
  
  let to_string =
    function
    | Activebackground(n) -> [|"-activebackground"; n |]
    | Anchor(x) -> [|"-anchor"; x |]
    | Arrow(x) -> [|"-arrow"; x |]
    | Bd(w) -> [|"-bd"; string_of_int w |]
    | Bg(c) -> [|"-background"; c |]
    | Borderwidth(w) -> [|"-bd"; string_of_int w |]
    | Cmd(f) -> [|"-command"; callbackUnit(Error.guard f) |]
    | CPadx(x) -> [|"-padx"; x |]
    | CPady(x) -> [|"-pady"; x |]
    | Cursor(x) -> [|"-cursor"; x |]
    | Default(x) -> [|"-default"; Constant.state2str x |]
    | Exportselection(x) -> [|"-exportselection"; string_of_int x |]
    | Fg(c) -> [|"-foreground"; c |]
    | Fill(x) -> [|"-fill"; x |]
    | Font(f) -> [|"-font"; f |]
    | Height(n) -> [|"-height"; string_of_int n |]
    | Highlightthickness(n) -> [|"-highlightthickness"; string_of_int n |]
    | Highlightbackground(n) -> [|"-highlightbackground"; n |]
    | Highlightcolor(n) -> [|"-highlightcolor"; n |]
    | Indicatoron(n) -> [|"-indicatoron"; string_of_bool n |]
    | Id(i) -> [|"-id"; i |]
    | Image(i) -> [|"-image"; i |]
    | Justify(s) -> [|"-justify"; s |]
    | Menu(x) -> [|"-menu"; x |]
    | Minsize(n) -> [|"-minsize"; string_of_int n |]
    | Padding(n) -> [|"-padding"; string_of_int n |]
    | Open(x) -> [|"-open"; string_of_bool x |]
    | Outline(x) -> [|"-outline"; x |]
    | Offrelief(r) -> [|"-offrelief"; r |]
    | Overrelief(r) -> [|"-overrelief"; r |]
    | Relief(r) -> [|"-relief"; string_of_relief r |]
    | Scrollregion(a, b, c, d) -> ( let coords =
            string_of_int a^" "^string_of_int b^" "^
            string_of_int c^" "^string_of_int d in
            [|"-scrollregion"; coords |] )
    | Selectcolor(c) -> [|"-selectcolor"; c |]
    | Selectmode(m) -> [|"-selectmode"; m |]
    | Setgrid(b) -> [|"-setgrid"; if b then "0" else "1" |]
    | Smooth -> [|"-smooth";"1" |]
    | Sticky(s) -> [|"-sticky"; s |]
    | Style(s) -> [|"-style"; s |]
    | Tags(tgs) -> [|"-tags"; String.concat " " tgs |]
    | Takefocus(x) -> [|"-takefocus"; string_of_int x |]
    | Tearoff -> [|"-tearoff";"0" |]
    | Text(t) -> [|"-text"; t |]
    | TkState(t) -> [|"-state"; Constant.state2str t |]
    | Uniform(t) -> [|"-uniform"; t |]
    | Value(v) -> [|"-value"; v |]
    | Values(vl) -> [|"-values"; String.concat " " vl |]
    | Width(n) -> [|"-width"; string_of_int n |]
    | Window(s) -> [|"-window"; s |]
    | Weight(n) -> [|"-weight"; string_of_int n |]
    | Wrap(w) -> [|"-wrap"; w |]
    | Xscroll(w) -> [|"-xscrollcommand"; w^" set" |]
    | Yscroll(w) -> [|"-yscrollcommand"; w^" set" |]
  
end

(* ==========================================================================
packing
========================================================================== *)

module Pack =

struct
  
  type option_t = Left | Right | Top | Bottom
    | Fillx | Filly | Fillxy | Expand
    | Padx of string | Pady of string
    | In of string
  
  let string_of_option =
    function
    | Left -> [|"-side";"left" |]
    | Right -> [|"-side";"right" |]
    | Top -> [|"-side";"top" |]
    | Bottom -> [|"-side";"bottom" |]
    | Fillx -> [|"-fill";"x" |]
    | Filly -> [|"-fill";"y" |]
    | Fillxy -> [|"-fill";"both" |]
    | Expand -> [|"-expand";"true" |]
    | Padx(x) -> [|"-padx"; x |]
    | Pady(x) -> [|"-pady"; x |]
    | In(x) -> [|"-in"; x |]
end
(* ==========================================================================
Tk modules
========================================================================== *)
module Grid =

struct
  
  type grid_options_t =
    | Column of int
    | Columnspan of int
    | In of string
    | Ipadx of int
    | Ipady of int
    | Padx of int
    | Pady of int
    | Row of int
    | Rowspan of int
    | Sticky of string
  
  let grid_option2str =
    function
    | Column(c) -> [|"-column"; string_of_int c |]
    | Columnspan(c) -> [|"-columnspan"; string_of_int c |]
    | In(x) -> [|"-in"; x |]
    | Ipadx(c) -> [|"-ipadx"; string_of_int c |]
    | Ipady(c) -> [|"-ipady"; string_of_int c |]
    | Padx(c) -> [|"-padx"; string_of_int c |]
    | Pady(c) -> [|"-pady"; string_of_int c |]
    | Row(r) -> [|"-row"; string_of_int r |]
    | Rowspan(c) -> [|"-rowspan"; string_of_int c |]
    | Sticky(s) -> [|"-sticky"; s |]
  
  let anchor master anchor =
    tK [| "grid"; "achor"; master; anchor |]
  
  let bbox master columnrowlist =
    tk (Array.concat ([| "grid"; "bbox"; master |]:: columnrowlist))
  
  let columnconfigure master index options =
    tK (Array.concat ([| "grid"; "columnconfigure"; master; index |]::
            (List.map Option.to_string options)))
  
  let configure slavelist options =
    tK (Array.concat ([| "grid";"configure" |]:: Array.of_list slavelist::
            (List.map grid_option2str options)))
  
  let forget slavelist =
    tK (Array.append [| "grid";"forget" |] (Array.of_list slavelist))
  
  let grid slavelist options =
    tK (Array.concat([| "grid" |]:: Array.of_list slavelist::
            (List.map grid_option2str options)))
  
  let info slave =
    tk [| "grid"; "info"; slave |]
  
  let	location master x y =
    tk [| "grid"; "location"; master; x; y |]
  
  let propagate master b =
    tK [| "grid"; "location"; master; b |]
  
  let remove slavelist =
    tK (Array.append [| "grid";"remove" |] (Array.of_list slavelist))
  
  let rowconfigure master index options =
    tK (Array.concat ([| "grid"; "rowconfigure"; master; index |]::
            (List.map Option.to_string options)))
  
  let slaves master =
    tk [| "grid"; "size"; master |]
end

(* ==========================================================================
fonts
========================================================================== *)
module Font =

struct
  let create name family size weight =
    tK [| "font";"create"; name;
      "-family"; family;
      "-size"; size;
      "-weight"; weight |]
  
  let configure name kind value =
    tK [| "font";"configure"; name;"-"^kind; value |]
  let metrics_linespace name =
    int_of_tk [| "font";"metrics"; name;"-linespace" |]
  
  let sys_font = "sys_font"
  let se_font = "se_font"
  let ge_font = "ge_font"
  let sim_font = "sim_font"
  
  let txt_font = ref se_font
  
  let set font size weight =
    tK [| "font";"configure"; font;"-size"; size;"-weight"; weight |]
  
  let get_size font =
    tk [| "font";"configure"; font;"-size" |]
  let get_weight font =
    tk [| "font";"configure"; font;"-weight" |]
  let get_family font =
    tk [| "font";"configure"; font;"-family" |]
  
  (* --------------------------------------------------------------------------
  count_lines t ... return the number of lines for text ''t''
  height_of_text font text ... returns height of a text
  -------------------------------------------------------------------------- *)
  let count_lines t =
    let len = String.length t in
      if len = 0
      then 1
      else
        let c = ref 0 in
          for x = 0 to len - 1 do
            if t.[x] = '\n'
            then ( c := !c + 1 )
            else ( )
          done;
          !c + 1
  
  let height_of_text font text =
    let c = count_lines text in
    let lsi = metrics_linespace font in
      9 + (c * lsi)
  
end

(* ==========================================================================
''high - level'' functions for tcl / tk connection
========================================================================== *)
module Event =

struct
  
  let generate w event =
    tK[|"event";"generate"; w; event |]
  
end

(* ------------------------------------------------------------------------- *)
module Image =

struct
  
  type image_t = Photo | Bitmap
  
  let delete img = tK [| "image"; "delete"; img |]
  
  let height img = tK [| "image"; "height"; img |]
  
  let width img = tK [| "image"; "width"; img |]
  
  let inuse img = tK [| "image"; "inuse"; img |]
  
  let typ img =
    match tk [| "image"; "type"; img |] with
    | "photo" -> Photo
    | "bitmap" -> Bitmap
    | _ -> assert false
  
  let create typ ?(options =[]) name =
    let typ = match typ with
      | Photo -> "photo"
      | Bitmap -> "bitmap" in
      tk (Array.concat ([| "image"; "create"; typ; "-file"; name |]
              :: (List.map Option.to_string options)))
end

(* ------------------------------------------------------------------------- *)
module Wm =

struct
  let attributes w opt value = tK [| "wm";"attributes"; w;"-"^opt; value |]
  let geometry w = tk [| "wm";"geometry"; w |]
  let set_geometry w geometry = tK [| "wm";"geometry"; w; geometry |]
  let transient w m = tK [| "wm";"transient"; w; m |]
  let withdraw w = tK [| "wm";"withdraw"; w |]
  let deiconify w = tK [| "wm";"deiconify"; w |]
  let title w t = tK [| "wm";"title"; w; t |]
  let not_resizable w = tK [| "wm";"resizable"; w;"0";"0" |]
  let resize_height_only w = tK [| "wm";"resizable"; w;"0";"1" |]
  let resize_width_only w = tK [| "wm";"resizable"; w;"1";"0" |]
  let manage w = tK [| "wm";"manage"; w |]
  
  let zoom w =
    if Constant.windowingsystem () = Constant.aqua
    then tK [| "wm"; "state"; w ; "zoomed" |]
    else tK [| "wm"; "attribute"; w ; "-zoomed" |]
  
  let raise w =
    tK [| "wm";"withdraw"; w |];
    tK [| "wm";"deiconify"; w |];
    tK [| "raise"; w |]
end

(* ------------------------------------------------------------------------- *)
module Info =

struct
  let exists w = bool_of_tk [| "info";"exists"; w |]
end

module Winfo =

struct
  let exists w = bool_of_tk [| "winfo";"exists"; w |]
  let rootx w = int_of_tk [| "winfo";"rootx"; w |]
  let rooty w = int_of_tk [| "winfo";"rooty"; w |]
  let ismapped w = bool_of_tk [| "winfo";"ismapped"; w |]
  let height w = int_of_tk [|"winfo";"height"; w |]
  let width w = int_of_tk [|"winfo";"width"; w |]
  let reqheight w = int_of_tk [|"winfo";"reqheight"; w |]
  let reqwidth w = int_of_tk [|"winfo";"reqwidth"; w |]
end

(* ------------------------------------------------------------------------- *)
module Dialog =

struct
  (* -------------------------------------------------------------------------
  make system commands target independent
  ------------------------------------------------------------------------- *)
  let rec name4win32 f n =
    try let i = String.index_from f n '/' in
          String.set f i '\\';
          name4win32 f (n + 1)
    with Not_found -> ();
        f
  
  let name4win32 f =
    match Sys.os_type with
    | "Win32" -> name4win32 f 0
    | _ -> f
  
  let messageBox ?(default ="") ?(title ="") typ icon msg =
    tk [| "tk_messageBox";
      "-default"; default;
      "-type"; typ;
      "-title"; title;
      "-icon"; icon;
      "-message"; msg |]
  
  let getOpenFile ?(ext ="") ?(initdir ="") ?(initfile ="")
      ?(parent ="") ?(filetypes ="") title =
    name4win32
      (tk [| "tk_getOpenFile";
          "-defaultextension"; ext;
          "-parent"; parent;
          "-title"; title;
          "-filetypes"; filetypes;
          "-initialdir"; initdir;
          "-initialfile"; initfile |])
  
  let chooseDirectory ?(parent ="") ?(initdir ="") title =
    name4win32
      (tk [| "tk_chooseDirectory";
          "-parent"; parent;
          "-title"; title;
          "-initialdir"; initdir |])
  
  let getSaveFile ?(ext ="") ?(initdir ="")
      ?(initfile ="") ?(parent ="") title =
    name4win32
      (tk [| "tk_getSaveFile";
          "-defaultextension"; ext;
          "-parent"; parent;
          "-title"; title;
          "-initialfile"; initfile;
          "-initialdir"; initdir |])
  
  let getSaveFile2 ?(ext ="") ?(initdir ="")
      ?(initfile ="") ?(parent ="") ?(filetypes ="") title =
    name4win32
      (tk [| "tk_getSaveFile";
          "-defaultextension"; ext;
          "-parent"; parent;
          "-title"; title;
          "-initialfile"; initfile;
          "-filetypes"; filetypes;
          "-initialdir"; initdir |])
end

(* ==========================================================================
widget classes
========================================================================== *)
module Tk =

struct
  
  class widget path =
  
  object(self)
    
    val mutable id = ""
    
    method set_id x = id <- x
    method get_id = id
    method path = path
    
    method pack options =
      tK (Array.concat ([|"pack"; path |]:: (List.map Pack.string_of_option options)))
    method pack_forget =
      tK [| "pack";"forget"; path |]
    
    method cget opt =
      tk [| path;"cget";"-"^opt |]
    method configure options =
      tK (Array.concat ([| path;"configure" |]:: (List.map Option.to_string options)))
    
    method instate s c = tk [| path;"state"; s; c |]
    method state s = tK [| path;"state"; Constant.state2str s |]
    
    method grab = tK [| "grab";"set"; path |]
    method grab_global = tK [| "grab";"set";"-global"; path |]
    method grab_release = tK [| "grab";"release"; path |]
    
    method raise = tK [| "raise"; path |]
    method lower = tK [| "lower"; path |]
    method focus = tK [| "focus"; path |]
    method destroy = tK [| "destroy"; path |]
    
    method bindtags tags =
      tK [| "bindtags"; path; tags |]
    
    method bind event cllbck =
      tK [| "bind"; path ; event; callbackUnit(cllbck) |]
    
    method bind_pattern event pattern cllbck =
      bind path event ~pattern: pattern cllbck
  end
  
  (* ------------------------------------------------------------------------- *)
  class scrollable_widget path =
  
  object(self)
    inherit widget path
    
    method xview args = tK (Array.append [| path; "xview" |] args)
    method yview args = tK (Array.append [| path; "yview" |] args)
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class button ?(text ="") ?(cllbck = fun() -> ()) ?(value = "default") path =
  
  object(self)
    inherit widget path
    
    method invoke = tK [| path; "invoke" |]
    
    method flash = tK [| path; "flash" |]
    
    initializer (
      tK [| "tk::button"; path; "-text"; text; "-command"; callbackUnit(cllbck) |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class canvas path =
  
  object(self)
    inherit scrollable_widget path
    
    method typ id = tk [| path; "type"; id |]
    
    method find_id id = bool_of_tk [| path; "find"; "withtag"; id |]
    
    method item_configure id l =
      tK (Array.concat ([| path; "itemconfigure"; id |]:: (List.map Option.to_string l)))
    
    method canvas_bind w event cllbck =
      tK [| path; "bind"; w; event; callbackUnit(cllbck) |]
    
    method delete id = tK [| path; "delete"; id |]
    
    method canvasx x = tk [| path; "canvasx"; x |]
    method canvasy y = tk [| path; "canvasy"; y |]
    
    method scale_all f =
      tK [| path;"scale";"all";"0";"0"; string_of_float f; string_of_float f |];
      let coords = Constant.array (self#cget "scrollregion") in
      let coords = Array.map
          ( fun x -> int_of_float (f *.(float_of_string x))
          ) coords in
        self#configure ([Option.Scrollregion(0, 0, coords.(2), coords.(3))])
    
    method find_overlapping x0 y0 x1 y1 =
      tk [| path; "find"; "overlapping";
        string_of_int x0; string_of_int y0;
        string_of_int x1; string_of_int y1 |]
    
    method find_closest ?(halo ="") ?(start ="") x y =
      if halo = "" then (
        tk [| path; "find"; "closest"; string_of_int x; string_of_int y |]
      ) else if start = "" then (
        tk [| path; "find"; "closest"; string_of_int x; string_of_int y; halo |]
      ) else (
        tk [| path; "find"; "closest"; string_of_int x; string_of_int y; halo; start |]
      )
    
    method lower_all id =
      tK [| path;"lower"; id; "all" |]
    
    method coords id coords =
      let coords = Array.map string_of_int coords in
        tK (Array.append [| path;"coords"; id |] coords)
    
    method create_rectangle x0 y0 x1 y1 options =
      let id =
        tk [| path;"create";"rectangle";
          string_of_int x0; string_of_int y0;
          string_of_int x1; string_of_int y1 |] in
        self#item_configure id options;
        id
    
    method coord_rounded_rectangle x0 y0 x3 y3 =
      let d = 2.0 *. float_of_string
          (tk [| "winfo";"fpixels"; path;"8" |]) in
      (* XXX radius of the corner = 8 *)
      let x0 = float_of_int x0 and y0 = float_of_int y0
      and x3 = float_of_int x3 and y3 = float_of_int y3 in
      let dx = 0.75 *.(x3 -. x0) and dy = 0.75 *.(y3 -. y0) in
      let d = if d > dx then dx else d in
      let d = if d > dy then dy else d in
      let x1 = x0 +. d and x2 = x3 -. d
      and y1 = y0 +. d and y2 = y3 -. d in
      let coord = [| x0; y0; x1; y0; x2; y0; x3; y0; x3; y1; x3; y2;
        x3; y3; x2; y3; x1; y3; x0; y3; x0; y2; x0; y1; x0; y0 |] in
      let coord = Array.map (fun x -> int_of_float x) coord in
        coord
    
    method create_rounded_rectangle x0 y0 x3 y3 =
      let coords = self#coord_rounded_rectangle x0 y0 x3 y3 in
      let coords = Array.map string_of_int coords in
      let id = tk (Array.concat [ [| path;"create";"line" |];
              coords;[|"-smooth";"1" |] ]) in
        id
    
    method create_polygon coords options =
      let coords = Array.map string_of_int coords in
      let id = tk (Array.concat [ [| path;"create";"polygon" |]; coords ]) in
        self#item_configure id options;
        id
    
    method create_rounded_polygon x0 y0 x3 y3 =
      let coords = self#coord_rounded_rectangle x0 y0 x3 y3 in
      let coords = Array.map string_of_int coords in
      let id = tk (Array.concat [ [| path;"create";"polygon" |];
              coords;[|"-smooth";"1";|] ]) in
        id
    
    method create_oval x0 y0 x1 y1 options =
      let id = tk [| path;"create";"oval"; string_of_int x0; string_of_int y0; string_of_int x1; string_of_int y1 |] in
        self#item_configure id options;
        id
    
    method create_text x y options =
      let id = tk [| path;"create";"text"; string_of_int x; string_of_int y |] in
        self#item_configure id options;
        id
    
    method create_line coords options =
      let coords = Array.map string_of_int coords in
      let id = tk (Array.append [| path;"create";"line" |] coords) in
        self#item_configure id options;
        id
    
    method create_window x y options =
      let id = tk [| path;"create";"window"; string_of_int x; string_of_int y |] in
        self#item_configure id options;
        id
    
    initializer (
      tK [|
        "canvas"; path;
        "-bg"; "white";
        "-bd"; if Constant.windowingsystem() = "aqua" then "2" else "1";
        "-relief"; "solid";
        "-highlightcolor"; "red1"
        |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class listbox path =
  
  object(self)
    inherit scrollable_widget path
    
    method read_only =
      tK [| "bindtags"; path; path^" all" |];
    
    method size =
      int_of_tk [| path;"size" |]
    
    method delete ?(first ="0") ?(last ="end") () =
      tK [| path;"delete"; first; last |]
    
    method insert ?(index ="end") entry =
      tK [| path;"insert"; index; entry |]
    
    method insert_list ?(index ="end")items =
      List.iter (fun x -> self#insert ~index: index x) items;
    
    method sort =
      let selection = Constant.list (tk [| path;"selection";"clear";"0";"end" |]) in
      let selection = List.sort compare selection in
        self#selection_clear ();
        self#insert_list selection
    
    method curselection =
      Constant.list (tk [| path;"curselection" |])
    
    method selection_set index0 index1 =
      tK [| path;"selection";"set"; index0; index1 |]
    method selection_set_anchor n =
      tK [| path;"selection";"set";"anchor"; n |]
    method selection_includes index =
      bool_of_tk [| path;"selection";"includes"; index |]
    method selection_clear_all =
      tK [| path;"selection";"clear" |]
    method selection_clear ?(first ="0") ?(last ="end") () =
      tK [| path;"selection";"clear"; first; last |]
    
    method select () =
      match self#curselection with
      | [x] -> self#get ~index: x ()
      | _ -> ""
    
    method delete_selection () =
      let sel = ref self#curselection in
        if !sel <> []
        then (
          self#delete ~first: (List.hd !sel) ();
          sel := List.tl !sel;
          self#delete_selection ()
        )
    
    method nearest index = int_of_tk [| path; "nearest"; index |]
    
    method get ?(index ="0") () =
      tk [| path;"get"; index |]
    
    method get_list ?(first ="0") ?(last ="end") () =
      Constant.list (tk [| path;"get"; first; last |])
    
    initializer (
      tK [|
        "listbox"; path;
        "-bg";"white";
        "-font";!Font.txt_font;
        "-bd"; if Constant.windowingsystem() = "aqua" then "2" else "1";
        "-relief";"sunken";
        "-bd";"1" |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class menu ?(font = Font.sys_font) path =
  
  object(self)
    inherit widget path
    
    method add_cascade_no_label menu =
      tK [| path;"add";"cascade";
        "-menu"; (menu: menu)#path |]
    
    method add_cascade label ?(underl = 0) menu =
      tK [| path;"add";"cascade";
        "-menu"; (menu: menu)#path;
        "-label"; label;
        "-underl"; string_of_int underl |]
    
    method add_command ?(cllbck = fun() -> ()) ?(underl = 0)
        ?(acc ="") label =
      tK [| path;"add";"command";
        "-label"; label;
        "-underl"; string_of_int underl;
        "-command"; callbackUnit(cllbck);
        "-acc"; acc |]
    
    method add_radiobutton ?(cllbck = fun() -> ()) ?(underl = 0) ?(acc ="") var label =
      tK [| path;"add";"radiobutton";
        "-label"; label;
        "-underl"; string_of_int underl;
        "-command"; callbackUnit(cllbck);
        "-indicatoron";"1";
        "-variable"; var;
        "-value"; label;
        "-acc"; acc |]
    
    method add_separator =
      tK [| path;"add";"separator" |]
    
    method delete ?(first ="0") ?(last ="end") () =
      tK [| path;"delete"; first; last |]
    
    method entryconfigure entry cllbck =
      tK [| path;"entryconfigure"; string_of_int entry;
        "-command"; callback cllbck |]
    
    method entrycget index opt =
      tk [| path;"entrycget"; index;"-"^opt |]
    
    initializer (
      tK [| "menu"; path; "-relief";"groove"; "-tearoff";"0"; "-font"; font |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class popup_menu path =
  
  object(self)
    inherit menu path
    
    method destroy =
      tK [| "destroy"; path |]
    method popup x y =
      tK [| "tk_popup"; path; string_of_int x; string_of_int y |]
    method activate n =
      tK [| path; "activate"; string_of_int n |]
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class text ?(height = 60) ?(width = 60) path =
  
  object(self)
    inherit scrollable_widget path
    
    method read_only =
      tK [| "bindtags"; path; path^" all" |];
    
    method delete ?(first ="1.0") ?(last ="end") () =
      tK [| path;"delete"; first; last |]
    
    method delete_one index =
      tK [| path;"delete"; index |]
    
    method get ?(first ="1.0") ?(last ="end") () =
      tk [| path;"get"; first; last |]
    
    method insert ?(index ="end") ?(tag ="") entry =
      tK [| path;"insert"; index; entry; tag |]
    
    method see index =
      tK [| path;"see"; index |]
    
    method compare index0 op index1 =
      bool_of_tk [| path; "compare"; index0; op; index1 |]
    
    method window_create w =
      tK [| path;"window";"create";"end";"-window"; w;
        "-padx";"0";"-pady";"0";"-stretch";"true" |]
    
    method tag_add ?(first ="1.0") ?(last ="end") tag =
      tK [| path;"tag";"add"; tag; first; last |]
    
    method tag_remove ?(first ="1.0") ?(last ="end") tag =
      tK [| path;"tag";"remove"; tag; first; last |]
    
    method tag_nextrange tag index0 index1 =
      tk [| path;"tag";"nextrange"; tag; index0; index1 |]
    
    method tag_configure tag l =
      tK (Array.concat ([| path;"tag";"configure"; tag |]::
              (List.map Option.to_string l)))
    
    method tag_bind tag event cllbck =
      tK [| path; "tag"; "bind"; tag; event; callbackUnit(cllbck) |]
    
    method index x y = tk [| path; "index"; "@"^x^","^y |]
    method index_insert = tk [| path; "index"; "insert" |]
    
    method cut () =
      tK [| "Tk.textCut"; path |]
    method copy () =
      tK [| "Tk.textCopy"; path |]
    method paste () =
      tK [| "Tk.textPaste"; path |]
    method upDownLine d =
      tk [| "tk::TextUpDownLine"; path; d |]
    method setCursor l =
      tK [| "tk::TextSetCursor"; path; l |]
    method textInsert y =
      tK [| "tk::TextInsert"; path; y |]
    method textButton1 x y =
      tK [| "tk::TextButton1"; path; x; y |]
    method selectTo x y =
      tK [| "tk::TextSelectTo"; path; x; y |]
    method embedWindow e =
      tK [| path;"window";"create";"end";"-window"; e;
        "-padx";"0";"-pady";"0";"-stretch";"true" |]
    
    method mark_set mark index = tK [| path; "mark"; "set"; mark; index |]
    
    method undo () = tK [| path; "edit"; "undo" |]
    method redo () = tK [| path; "edit"; "redo" |]
    method clear () = tK [| path; "delete"; "sel.first"; "sel.last" |]
    
    initializer (
      tK [|
        "text"; path;
        "-bd";"1";
        "-relief"; if Constant.windowingsystem() = "aqua" then "sunken" else "solid";
        "-font"; !Font.txt_font;
        "-bg";"white";
        "-highlightbackground";"LightGrey";
        "-cursor";"arrow";
        "-wrap";"none";
        "-spacing1";"1";
        "-spacing3";"1";
        "-width"; string_of_int width;
        "-height"; string_of_int height;
        "-undo";"1"
        |]
    )
  end
  
end
(* ==========================================================================
Ttk widget classes
========================================================================== *)
module Ttk =

struct
  
  type layouts =
    | Side of string
    | Children of layouts list
  
  (* ------------------------------------------------------------------------- *)
  class composite_widget path =
  
  object(self)
    inherit Tk.widget path
    
    val nil = tK [| "ttk::frame"; path |]
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class button ?(text ="") ?(cllbck = fun() -> ()) ?(value = "default") path =
  
  object(self)
    inherit Tk.widget path
    
    method invoke = tK [| path; "invoke" |]
    
    initializer (
      tK [| "ttk::button"; path; "-text"; text; "-command"; callbackUnit(cllbck) |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class check_button ?(text ="") ?(cllbck = fun() -> ()) path =
  
  object(self)
    inherit Tk.widget path
    
    method invoke = tK [| path; "invoke" |]
    
    initializer (
      tK [| "ttk::checkbutton"; path; "-text"; text; "-command"; callbackUnit(cllbck) |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class combobox path =
  
  object(self)
    inherit Tk.widget path
    
    method current index =
      tK [| path; "current"; string_of_int index |]
    
    method get =
      tk [| path; "get" |]
    
    method identify x y =
      tk [| path; "identify"; x; y |]
    
    method set value =
      tK [| path; "set"; value |]
    
    method delete ?(first ="1") ?(last ="end") () =
      tK [| path;"delete"; first; last |]
    
    method set_cmd cmd =
      bind path "<<ComboboxSelected>>" cmd
    
    method load_items items =
      self#configure [ Option.Values(items) ]
    
    initializer (
      tK [| "ttk::combobox"; path |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class entry width path =
  
  object(self)
    inherit Tk.scrollable_widget path
    
    method read_only =
      tK [| "bindtags"; path; path^" all" |];
    
    method get =
      tk [| path;"get" |]
    
    method set value =
      tK [| "set"; path^"_#var"; value |]
    
    method insert ?(index ="end") value =
      tK [| path;"insert"; index; value |]
    
    method delete ?(first ="0") ?(last ="end") () =
      tK [| path;"delete"; first; last |]
    
    method icursor ?(index ="end") () =
      tK [| path;"icursor"; index |]
    
    initializer (
      tK [| "ttk::entry"; path; "-width"; string_of_int width; "-textvariable"; path^"_#var" |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class frame path =
  
  object(self)
    inherit Tk.widget path
    
    initializer (
      tK [| "ttk::frame"; path |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class image name image imageSpec =
  
  object(self)
    inherit Tk.widget name
    
    val mutable image = ""
    
    initializer (
      image <- tk [| "ttk::style"; "create"; image; imageSpec |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class label ?(text ="") path =
  
  object(self)
    inherit Tk.widget path
    
    initializer (
      tK [| "ttk::label"; path; "-text"; text |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class labelframe ?(text ="") path =
  
  object(self)
    inherit Tk.widget path
    
    initializer (
      tK [| "ttk::labelframe"; path; "-text"; text |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class menu_button ?(text ="") path =
  
  object(self)
    inherit Tk.widget path
    
    val _menu = new Tk.menu (path^"i.menu")
    method menu = _menu
    
    initializer (
      tK [| "menubutton"; path; "-text"; text; "-menu"; path^"i.menu"; |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class notebook path =
  
  object(self)
    inherit Tk.widget path
    
    method add widget options =
      tK (Array.concat ([| path; "add"; widget |] :: (List.map Option.to_string options)))
    
    method forget tabid =
      tK [| path; "forget"; tabid |]
    
    method hide tabid =
      tK [| path; "hide"; tabid |]
    
    method index tabid =
      tK [| path; "index"; tabid |]
    
    method insert pos subwindow options =
      tK (Array.concat ([| path; "insert"; pos; subwindow |]:: (List.map Option.to_string options)))
    
    method select tabid =
      tK [| path; "select"; tabid |]
    
    method current =
      tk [| path; "select" |]
    
    method tab tabid options =
      tK (Array.concat ([| path; "tab"; tabid |]:: (List.map Option.to_string options)))
    
    method tabs =
      tK [| path; "tabs" |]
    
    initializer (
      tK [| "ttk::notebook"; path |];
      tK [| "ttk::notebook::enableTraversal"; path |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class paned_window ?(orientation ="horizontal") path =
  
  object(self)
    inherit Tk.scrollable_widget path
    
    method add pane =
      tK [| path; "add"; pane |]
    
    method forget pane =
      tK [| path; "forget"; pane |]
    
    method insert pos subwindow options =
      tK (Array.concat ([| path; "insert"; pos; subwindow |]:: (List.map Option.to_string options)))
    
    method pane pane options =
      tK (Array.concat ([| path; "pane"; pane |]:: (List.map Option.to_string options)))
    
    method sashpos index newpos =
      tK [| path; "sashpos"; string_of_int index;
        string_of_int newpos |]
    
    method identify index x y =
      tK [| path; "sashpos"; string_of_int index;
        string_of_int x; string_of_int y |]
    
    initializer (
      tK [| "ttk::panedwindow"; path; "-orient"; orientation |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class progressbar path =
  
  object(self)
    inherit Tk.widget path
    
    method start ?(interval ="") () =
      tK [| path; "start"; interval |]
    
    method step ?(amount ="") () =
      tK [| path; "step"; amount |]
    
    initializer (
      tK [| "ttk::progressbar"; path |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class sizegrip path =
  
  object(self)
    inherit Tk.widget path
    
    initializer (
      tK [| "ttk::sizegrip"; path |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class radio_button ?(text ="") ?(cllbck = fun() -> ()) ?(value = "default") path =
  
  object(self)
    inherit Tk.widget path
    
    method invoke = tK [| path; "invoke" |]
    
    initializer (
      tK [| "ttk::radiobutton"; path; "-text"; text; "-value"; value; "-command"; callbackUnit(cllbck) |]
    )
  end
  
  (* ------------------------------------------------------------------------- *)
  class scrollbar ~orientation widget path =
  
  object(self)
    inherit Tk.widget path
    
    method insert index entry =
      tK [| path; "insert"; string_of_int index; entry |]
    
    method moveto fraction = tK [| path; "moveto"; fraction |]
    method scroll_units number = tK [| path; number; "units" |]
    method scroll_pages number = tK [| path; number; "pages" |]
    
    initializer (
      if orientation = "vertical" then (
        tK [| "ttk::scrollbar"; path;
          "-orient"; orientation;
          "-command"; callback widget#yview |]
      ) else if orientation = "horizontal" then (
        tK [| "ttk::scrollbar"; path;
          "-orient"; orientation;
          "-command"; callback widget#xview |]
      ) else (
        assert false
      )
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  class separator path =
  
  object(self)
    inherit Tk.widget path
    
    initializer (
      tK [| "ttk::separator"; path |]
    )
    
  end
  
  (* ------------------------------------------------------------------------- *)
  module Style =
  
  struct
    let configure style options =
      tK (Array.concat ([| "ttk::style"; "configure"; style |]:: (List.map Option.to_string options)))
    
    let map style layouts =
      tK [| "ttk::style"; "map"; style; layouts |]
    
    let lookup style state =
      tK [| "ttk::style"; "lookup"; style; "-option"; state |]
    
    let layout style layouts =
      tK [| "ttk::style"; "layout"; style; layouts |]
    
    let element_create elementName typ args =
      tK [| "ttk::style"; "element_create"; elementName; typ; args |]
    
    let element names =
      tK [| "ttk::style"; "element"; "names" |]
    
    let element_options element =
      tK [| "ttk::style"; "element_options"; element |]
    
    let theme_create themeName options =
      tK (Array.concat ([| "ttk::style"; "theme_create"; themeName; |]
              :: (List.map Option.to_string options)))
    
    let theme_settings themeName script =
      tK [| "ttk::style"; "theme_settings"; themeName; script |]
    
    let theme names =
      tK [| "ttk::style"; "theme"; "names" |]
    
    let theme_use themeName =
      tK [| "ttk::style"; "theme_use"; themeName |]
  end
  
  (* ------------------------------------------------------------------------- *)
  class treeview path =
  
  object(self)
    inherit Tk.scrollable_widget path
    
    method bbox item column =
      tK [| path; "bbox"; item; column |]
    
    method set_children item new_children =
      let items = String.concat " " new_children in
        tK [| path; "children"; item; items |]
    
    method get_children item =
      Constant.list (tk [| path; "children"; item |])
    
    method column column options =
      tK (Array.concat ([| path; "column"; column |]:: (List.map Option.to_string options)))
    
    method delete items =
      tK [| path; "delete"; String.concat " " items |]
    
    method detach items =
      tK [| path; "detach"; String.concat " " items |]
    
    method exists item =
      bool_of_tk [| path; "exists"; item |]
    
    method heading column options =
      tK (Array.concat ([| path; "heading"; column |]:: (List.map Option.to_string options)))
    
    method identify_row x y =
      tK [| path; "identify"; "row"; x; y |]
    
    method identify_column x y =
      tK [| path; "identify"; "column"; x; y |]
    
    method index item =
      tK [| path; "index"; item |]
    
    method insert parent index options =
      tk (Array.concat ([| path; "insert"; parent; index |]:: (List.map Option.to_string options)))
    
    method set_options item options =
      tK (Array.concat ([| path; "item"; item |]:: (List.map Option.to_string options)))
    
    method get_option item option =
      tk [| path; "item"; item; "-"^option |]
    
    method move item parent index =
      tK [| path; "move"; item; parent; index |]
    
    method next item =
      tK [| path; "next"; item |]
    
    method parent item =
      tK [| path; "parent"; item |]
    
    method prev item =
      tK [| path; "prev"; item |]
    
    method see item =
      tK [| path; "see"; item |]
    
    method selection_set items =
      tK [| path; "selection"; "set"; String.concat " " items |]
    
    method selection_add items =
      tK [| path; "selection"; "add"; String.concat " " items |]
    
    method selection_remove items =
      tK [| path; "selection"; "remove"; String.concat " " items |]
    
    method selection_toggle items =
      tK [| path; "selection"; "toggle"; String.concat " " items |]
    
    method set item column value =
      tK [| path; "set"; item; column; value |]
    
    method tag_bind tagName sequence pattern cllbck =
      tag_bind path tagName sequence pattern cllbck
    
    method tag_configure tagName options =
      tK (Array.concat ([| path; "tag"; "configure"; tagName |]:: (List.map Option.to_string options)))
    
    method xview args =
      tK (Array.append [| path; "xview" |] args)
    
    method yview args =
      tK (Array.append [| path; "yview" |] args)
    
    initializer (
      tK [| "ttk::treeview"; path |]
    )
  end
  
end (* module Ttk *)


(* ------------------------------------------------------------------------- *)
module Toplevel =

struct
  
  class toplevel text ?(about = None) ?(help = None) ?(geometry ="") path =
    
    let m = if path = "." then ".mbar" else path^".mbar" in
    let apple = m^".apple" in
    
      object(self)
        inherit Tk.widget path
        
        val top =
          if Winfo.exists path then (
            let geometry = Wm.geometry path in
              tK [| "destroy"; path |];
              tK [| "toplevel"; path; "-geometry"; geometry |]
          ) else (
            tK [| "toplevel"; path |]
          )
        
        val menu = new Tk.menu m
        val windows = new Tk.menu (m^".windows")
        
        method text = text
        method menu = menu
        method windows = windows
        
        method geometry = Wm.geometry path
        method set_geometry geometry = tK [| "wm"; "geometry"; path; geometry |]
        
        method deiconify = Wm.deiconify path
        method title text = Wm.title path text
        
        method raise =
          Wm.withdraw path;
          Wm.deiconify path;
          tK [| "raise"; path |]
        
        method transient m = Wm.transient path m
        method withdraw = Wm.withdraw path
        method overrideredirect v = tK [| "wm";"overrideredirect"; path; string_of_int v |]
        
        method not_resizable = tK [| "wm";"resizable"; path;"0";"0" |]
        
        method set_cmd cllbck =
          tK [| "wm";"protocol"; path;"WM_DELETE_WINDOW"; callbackUnit(cllbck) |]
        
        method focusmodel focus =
          tK [| "wm";"focusmodel"; path; focus |]
        
        initializer (
          tK [| "wm";"title"; path;"  "^text |];
          tK [| "wm";"iconname"; path; text |];
          tK [| "wm";"geometry"; path; geometry |];
          tK [| "wm";"protocol"; path;"WM_DELETE_WINDOW"; callbackUnit(Error.guard (fun () -> self#destroy)) |];
          
          if Constant.windowingsystem() = Constant.aqua then (
            ( match about with
              | None -> ()
              | Some(name, cllbck) ->
                  tK [| "menu"; apple |];
                  tK [| apple; "add"; "command";"-label"; name; "-command"; callbackUnit(cllbck) |];
                  tK [| apple; "add"; "command";"-label"; "License" |];
                  tK [| apple; "add"; "command";"-label"; "Check for Updates" |];
                  tK [| apple; "add"; "separator" |];
                  tK [| menu#path; "add"; "cascade"; "-menu"; apple |];
            )
          );
          self#configure [Option.Menu(menu#path)]
        )
      end
  
  let windows_r = ref []
  
  let register (top: toplevel) =
    List.iter
      ( fun t ->
            (t#windows: Tk.menu)#add_command ~cllbck: (fun () -> top#raise) t#text
      ) !windows_r;
    windows_r := top:: !windows_r
  
  let deregister (top: toplevel) =
    windows_r := List.fold_right
      ( fun t l ->
            if t#path = top#path then l else t:: l
      ) !windows_r [];
    List.iter
      ( fun t ->
            (t#windows: Tk.menu)#delete ~first: t#text ()
      ) !windows_r;
end