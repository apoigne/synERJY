open Ast
open Util
open Tk
open Util_tk

open Sim_type   (* sim_type.ml *)
open Sim_base   (* sim_base.ml *)

let _stp = ".stp"
(* -------------------------------------------------------------------------
Interface
------------------------------------------------------------------------- *)
let replay = Sim_play.replay
let control = Sim_tool.control

let trap (s: string) = () ;;  (* to be traced for debugging purposes *)
(* -------------------------------------------------------------------------
Miscellaneous
------------------------------------------------------------------------- *)
let max_height l =
  let h = Array.length l in
    if h > stp.max_height then stp.max_height else
    if h < 10 then 10 else
      h

let max_width_signals l =
  let w = ref 5 in
    Array.iteri (fun i m -> w := max (String.length m#get_name) !w) l;
    if !w > stp.max_width_var then stp.max_width_var else !w ;;

let max_width_types l =
  let w = ref 5 in
    Array.iteri (fun i m -> w := max (String.length m#get_type) !w) l;
    if !w > stp.max_width_type then stp.max_width_type else !w ;;

let normal_font() = let (f, s, w) = se.sim_font in
    f^" "^s^" normal"
let bold_font() = let (f, s, w) = se.sim_font in
    f^" "^s^" bold"

(* -------------------------------------------------------------------------
logging error messages
------------------------------------------------------------------------- *)
let sim_guard f () =
  try f ()
  with e -> stp.update_log e

(* -------------------------------------------------------------------------
saving and loading a configuration
------------------------------------------------------------------------- *)
type sim_tools_t =
  | ControlPanel of string * string * string list
  (* geometry,root,names of selected fields *)
  | ObjectBrowser of string
  | TraceBrowser of string * string * string list
  (* geometry,root,names of selected fields *)
  | ValueSample of string * string * string list * int * int
  (* geometry,object_name,names of selected fields,upper_instant,number *)
  | ReplayPanel of string * ((string * bool) list)
  | TextViewer of string * string * string * (int option) * string list
  | GraphicViewer of string * string * string * int * string list

exception FalseSimEnv of string

let rec find_matching_object n sfns (o: c_sE_target c_sE_object) =
  let rec bs_find_matching_object =
    function
    | [] ->
        None
    | o':: l ->
        match find_matching_object n sfns o' with
        | None -> bs_find_matching_object l
        | Some o -> Some o
  in
    if n = o#get_full_name
    then (
      o#check_and_select_fields sfns;
      Some o
    ) else
      bs_find_matching_object o#objects

let mk_text_viewer =
  ref
    ( fun (o: c_sE_target c_sE_object) (w: string) (name: string)
      (lbl: int option) (geometry: string) () -> ()
    )

let mk_graphic_viewer =
  ref
    ( fun (o: c_sE_target c_sE_object) (w: string) (name: string)
      (lbl: int) (geometry: string) () -> ()
    )

(* -------------------------------------------------------------------------
TRANSCRIPT tool
used to display and log message
------------------------------------------------------------------------- *)
class transcript path =

object (self)
  inherit Tk.widget path
  
  val entry = new Ttk.entry 50 path
  
  val mutable scripts = ([],[])
  
  method entry = entry
  
  method execute () =
    let s = entry#get in
      if (s = "") then (
        self#script_to_end
      ) else (
        self#script_append s;
        entry#set ""
      )
  
  method previous () =
    let f = fst scripts and s = snd scripts in
      entry#set (if f = [] then "" else List.hd f);
      if f <> [] then scripts <- (List.tl f, (List.hd f):: s)
  
  method next () =
    let f = fst scripts and s = snd scripts in
      entry#set (if s = [] then "" else List.hd s);
      if s <> [] then scripts <- ((List.hd s):: f, List.tl s)
  
  method update msg =   (* transcript *)
    entry#set (msg);
    if (msg <> "") then (
      entry#bind "<KeyPress>"
        (fun () -> entry#set "";
              entry#bind "<KeyPress>" (fun () -> ()));
      self#script_append msg;
    )
  
  method private script_to_end =
    scripts <- (((List.rev(snd scripts))@(fst scripts)), [])
  
  method private script_append msg =
    scripts <- (msg :: ((List.rev(snd scripts))@(fst scripts)), [])
  
  initializer
  entry#bind "<Return>" self#execute;
  entry#bind "<Control-p>" self#previous;
  entry#bind "<Control-Up>" self#previous;
  entry#bind "<Control-n>" self#next;
  entry#bind "<Control-Down>" self#next;
  
end  (* c_transcript *)

(* -------------------------------------------------------------------------
Widgets for presenting of complex literals
------------------------------------------------------------------------- *)

(* -------------------- literal lists to vector/matrix -------------------- *)
let lit2str (_, v, _) = v

let nlitlist2nlitarray vl =
  Array.of_list (List.map (fun (n, (_, v, _)) -> (n, v)) vl)

let litlist2litarray vl =
  Array.of_list (List.map lit2str vl)

let litlists2litarrays vll =
  Array.of_list (List.map litlist2litarray vll)

(* ------------------------------------------------------------------------- *)
class object_grid nvector ?(write = false) path =
  
  let max_label =
    List.fold_left
      ( fun i (n, _) -> max i (String.length n + 2)
      ) 0 nvector in
  let w = 2 + 22 + max_label in
  let h = 2 + int_of_float(1.5 *. float_of_int (List.length nvector)) in
  let nvector = nlitlist2nlitarray nvector in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val text = new Tk.text ~height: (min 35 h) ~width: w (path^".txt")
      
      val labels =
        Array.map
          ( fun (n, _) -> new Ttk.label ~text: n (path^".l_"^n)
          ) nvector
      
      val entries =
        Array.map
          ( fun (n, t) ->
                let e = new Ttk.entry 20 (path^".e_"^n) in
                  e#insert t;
                  if not write then e#read_only;
                  e
          ) nvector
      
      method entries = entries
      method labels = Array.map fst nvector
      
      method update (vl: (string * tliteral) list) =
        try List.iter2
              ( fun (e: Ttk.entry) (_, (_, v, _)) ->
                    e#delete();
                    e#insert v
              ) (Array.to_list entries) vl
        with _ -> Sim_error.intern "tk_object_grid"
      
      initializer (
        let text_with_scrolls = new Util_tk.widget_with_scrolls (text:> Tk.scrollable_widget) (path^".t") in
          text#configure [Option.Bg(Constant.background)];
          text_with_scrolls#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
          if not write then text#read_only;
          text#insert "\n";
          Array.iteri
            ( fun i _ ->
                  labels.(i)#configure [Option.Width(max_label)];
                  text#insert "  ";
                  text#window_create labels.(i)#path;
                  text#window_create entries.(i)#path;
                  text#insert "\n"
            ) nvector
      )
      
    end

(* ------------------------------------------------------------------------- *)
class vector_grid vector ?(write = false) path =
  
  let c = 20 in
  let l = List.length vector in
  let max_label = String.length (i2s l) + 2 in
  let h = 2 + int_of_float(1.5 *. float_of_int(if l < c then l else c)) in
  let w = 2 + (21 + max_label) * (if l < c then 1 else l / c + 1) in
  let w = min (2 + (21 + max_label) * 4) w in
  let h = min 35 h in
  let vector = litlist2litarray vector in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val text = new Tk.text ~height: h ~width: w (path^".txt")
      
      val labels =
        Array.mapi
          ( fun i e -> let i = "  "^i2s i^" " in
                  new Ttk.label ~text: i (path^".l_"^i)
          ) vector
      
      val entries =
        Array.mapi
          ( fun i t ->
                let e = new Ttk.entry 20 (path^".e_"^i2s i) in
                  e#insert t;
                  if not write then e#read_only;
                  e
          ) vector
      
      method entries = entries
      method text = text
      
      method update (vl: tliteral list) =
        try List.iter2
              ( fun e (_, v, _) ->
                    (e: Ttk.entry)#delete();
                    e#insert v
              ) (Array.to_list entries) vl
        with _ -> Sim_error.intern "tk_vector_grid"
      
      initializer (
        let text_with_scrolls = new Util_tk.widget_with_scrolls (text:> Tk.scrollable_widget) (path^".t") in
          if not write then text#read_only;
          text#configure [Option.Bg(Constant.background); Option.Setgrid(true)];
          text_with_scrolls#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
          text#insert "\n";
          for i = 0 to c - 1 do
            text#insert "  ";
            for j = 0 to l / c + (if l mod c = 0 then 0 else 1) - 1 do
              try labels.(j * c + i)#configure [Option.Width(max_label)];
                  text#window_create labels.(j * c + i)#path;
                  text#window_create entries.(j * c + i)#path;
              with _ -> ()
            done;
            text#insert "\n"
          done
      )
      
    end

(* ------------------------------------------------------------------------- *)
class matrix_grid matrix ?(write = false) path =
  
  let rows = List.length matrix in
  let cols = try List.length (List.hd matrix) with _ -> 0 in
  let matrix = litlists2litarrays matrix in
  let l = Array.length matrix in
  let max_label = String.length (i2s l) + 2 in
  let w = 2 + max_label + cols * 21 in
  let h = 2 + int_of_float(1.5 *. float_of_int (rows + 1)) in
  let h = min 35 h in
  let w = min (2 + max_label + 84) w in
  
    object(self)
      inherit Ttk.composite_widget path
      
      val text = new Tk.text ~height: h ~width: w (path^".txt")
      val empty = new Ttk.label ~text:"" (path^".e")
      
      val lrows =
        Array.mapi
          ( fun i e -> let i = i2s i in
                  new Ttk.label ~text: i (path^".r_"^i)
          ) matrix
      
      val lcols =
        try Array.mapi
              ( fun i e -> let i = i2s i in
                      new Ttk.label ~text: i (path^".c_"^i)
              ) matrix.(0)
        with _ -> [||]
      
      val entries =
        Array.mapi
          ( fun i v ->
                Array.mapi
                  (fun j t ->
                        let e = new Ttk.entry 20
                            (path^".e_"^i2s i^"_"^i2s j) in
                          e#insert t;
                          if not write then e#read_only;
                          e
                  ) v
          ) matrix
      
      method entries = entries
      
      method update (vll: tliteral list list) =
        try List.iter2
              ( fun el vl ->
                    List.iter2
                      ( fun e (_, v, _) ->
                            (e: Ttk.entry)#delete();
                            e#insert v
                      ) (Array.to_list el) vl
              ) (Array.to_list entries) vll
        with _ -> Sim_error.intern "tk_matrix_grid"
      
      initializer (
        let text_with_scrolls =
          new Util_tk.widget_with_scrolls (text:> Tk.scrollable_widget) (path^".t") in
          if not write then text#read_only;
          text#configure [Option.Bg(Constant.background); Option.Setgrid(true)];
          text_with_scrolls#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
          let left_up = new Ttk.label (path^".lu") in
            left_up#configure [Option.Width(max_label)];
            text#insert "  ";
            text#window_create left_up#path;
            for j = 0 to cols - 1 do
              lcols.(j)#configure [Option.Width(20)];
              text#window_create lcols.(j)#path;
            done;
            text#insert"\n";
            for i = 0 to rows - 1 do
              text#insert "  ";
              lrows.(i)#configure [Option.Width(max_label)];
              text#window_create lrows.(i)#path;
              for j = 0 to cols - 1 do
                try text#window_create entries.(i).(j)#path;
                with _ -> ()
              done;
              text#insert "\n"
            done
      )
      
    end

(* -------------------------------------------------------------------------
Widgets for presenting complex input literals
------------------------------------------------------------------------- *)
class object_input name namedvector path =

object(self)
  
  val top = new Toplevel.toplevel name path
  val obj = new object_grid namedvector ~write: true (path^".obj")
  val transcript = new transcript (path^".e")
  
  method entries = obj#entries
  method labels = obj#labels
  method transcript = transcript
  
  method to_val () =
    let vl = Array.map (fun e -> e#get) self#entries in
    let nvl = List.combine (Array.to_list self#labels) (Array.to_list vl) in
      Sim_util.to_objectstr nvl
  
  initializer (
    top#set_cmd (fun () -> Wm.withdraw path);
    transcript#entry#configure [Option.Width(30)];
    obj#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    transcript#pack [Pack.Top; Pack.Fillx];
    Wm.withdraw path
  )
  
end

(* ------------------------------------------------------------------------- *)
class vector_input name vector path =

object(self)
  
  val top = new Toplevel.toplevel name path
  val vector = new vector_grid vector ~write: true (path^".vec")
  val transcript = new transcript (path^".e")
  
  method entries = vector#entries
  method transcript = transcript
  
  method to_val () =
    let vl = Array.map (fun e -> e#get) self#entries in
      Sim_util.to_vectorstr (Array.to_list vl)
  
  initializer (
    top#set_cmd (fun () -> Wm.withdraw path);
    vector#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    transcript#entry#configure [Option.Width(3)];
    transcript#pack [Pack.Top; Pack.Fillx];
    Wm.withdraw path
  )
  
end

(* ------------------------------------------------------------------------- *)
class matrix_input name matrix path =

object(self)
  
  val top = new Toplevel.toplevel name path
  val matrix = new matrix_grid matrix ~write: true (path^".mat")
  val transcript = new transcript (path^".e")
  
  method entries = matrix#entries
  method transcript = transcript
  
  method to_val () =
    let getl el = Array.to_list (Array.map ( fun e -> e#get) el) in
    let vl = Array.map getl self#entries in
      Sim_util.to_matrixstr (Array.to_list vl)
  
  initializer (
    top#set_cmd (fun () -> Wm.withdraw path);
    matrix#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    transcript#entry#configure [Option.Width(3)];
    transcript#pack [Pack.Top; Pack.Fillx];
    Wm.withdraw path
  )
  
end

(* -------------------------------------------------------------------------
Widgets for presenting complex value sample literals
------------------------------------------------------------------------- *)
class object_sample name namedvector path =

object(self)
  
  val top = new Toplevel.toplevel name path
  val obj = new object_grid namedvector (path^".grid")
  
  method object_w = obj
  
  initializer (
    top#set_cmd (fun () -> top#destroy);
    obj#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* ------------------------------------------------------------------------- *)
class vector_sample name vector path =

object(self)
  val top = new Toplevel.toplevel name path
  val vector = new vector_grid vector ~write: true (path^".grid")
  
  method vector_w = vector
  
  initializer (
    top#set_cmd (fun () -> top#destroy);
    vector#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* ------------------------------------------------------------------------- *)
class matrix_sample name matrix path =

object(self)
  
  val top = new Toplevel.toplevel name path
  val matrix = new matrix_grid matrix (path^".grid")
  
  method matrix_w = matrix
  
  initializer (
    top#set_cmd (fun () -> top#destroy);
    matrix#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* -------------------------------------------------------------------------
Widgets for presenting of trace literals
------------------------------------------------------------------------- *)
class object_trace_sample namedvector signal delete path =

object(self)
  
  val top = new Toplevel.toplevel signal#name path
  val obj = new object_grid namedvector (path^".grid")
  
  method reset =
    match signal#initvalue with
    | SObject vl -> obj#update vl
    | _ -> Sim_error.intern "object_trace_sample:1"
  
  method select_instant =
    match signal#get_value_at stp.pres_instant with
    | SObject vl -> obj#update vl
    | _ -> self#reset
  
  method update =
    match signal#get_value_at control#get_prev_instant with
    | SObject vl -> obj#update vl
    | _ -> self#reset
  
  initializer (
    top#set_cmd (fun () -> delete(); top#destroy);
    obj#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* ------------------------------------------------------------------------- *)
class vector_trace_sample vector signal delete path =

object(self)
  
  val top = new Toplevel.toplevel signal#name path
  val vector = new vector_grid vector ~write: true (path^".grid")
  
  method reset =
    match signal#initvalue with
    | SVector vl -> vector#update vl
    | _ -> Sim_error.intern "vector_trace_sample:1"
  
  method select_instant =
    match signal#get_value_at stp.pres_instant with
    | SVector vl -> vector#update vl
    | _ -> self#reset
  
  method update =
    match signal#get_value_at control#get_prev_instant with
    | SVector vl -> vector#update vl
    | _ -> self#reset
  
  initializer (
    top#set_cmd (fun () -> delete(); top#destroy);
    vector#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* ------------------------------------------------------------------------- *)
class matrix_trace_sample matrix signal delete path =

object(self)
  
  val top = new Toplevel.toplevel signal#name path
  val matrix = new matrix_grid matrix (path^".grid")
  
  method reset =
    match signal#initvalue with
    | SMatrix vl -> matrix#update vl
    | _ -> Sim_error.intern "matrix_trace_sample:1"
  
  method select_instant =
    match signal#get_value_at stp.pres_instant with
    | SMatrix vll -> matrix#update vll
    | _ -> self#reset
  
  method update =
    match signal#get_value_at control#get_prev_instant with
    | SMatrix vll -> matrix#update vll
    | _ -> self#reset
  
  initializer (
    top#set_cmd (fun () -> delete(); top#destroy);
    matrix#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
    top#raise
  )
  
end

(* -------------------------------------------------------------------------
for simulator configuration
------------------------------------------------------------------------- *)
module Dispatcher =

struct
  
  let trace_browser_tbl = Hashtbl.create 13
  let value_sample_tbl = Hashtbl.create 13
  let replay_panel_tbl = Hashtbl.create 13
  (*let graphic_viewer_tbl = Hashtbl.create 13*)
  let text_viewer_tbl = Hashtbl.create 13
  let object_viewer_tbl = Hashtbl.create 13
  let vector_viewer_tbl = Hashtbl.create 13
  let matrix_viewer_tbl = Hashtbl.create 13
  
  let reset () =
    Hashtbl.iter (fun _ o -> o#reset) trace_browser_tbl;
    Hashtbl.iter (fun _ o -> o#reset) value_sample_tbl;
    Hashtbl.iter (fun _ o -> o#reset) text_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#reset) replay_panel_tbl;
    (*Hashtbl.iter (fun _ o -> o#reset) graphic_viewer_tbl;*)
    
    Hashtbl.iter (fun _ o -> o#reset) object_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#reset) vector_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#reset) matrix_viewer_tbl
  
  let update () =
    Hashtbl.iter (fun _ o -> o#update) trace_browser_tbl;
    Hashtbl.iter (fun _ o -> o#update) value_sample_tbl;
    Hashtbl.iter (fun _ o -> o#update) replay_panel_tbl;
    (*Hashtbl.iter (fun _ o -> o#update) graphic_viewer_tbl;*)
    Hashtbl.iter (fun _ o -> o#update) object_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#update) vector_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#update) matrix_viewer_tbl
  
  let update_display () =
    Hashtbl.iter (fun _ o -> o#update_display) trace_browser_tbl;
    Hashtbl.iter (fun _ o -> o#update_display) value_sample_tbl
  
  let select_instant () =
    Hashtbl.iter (fun _ o -> o#select_instant) trace_browser_tbl;
    Hashtbl.iter (fun _ o -> o#select_instant) value_sample_tbl;
    Hashtbl.iter (fun _ o -> o#select_instant) text_viewer_tbl;
    (*Hashtbl.iter (fun _ o -> o#select_instant) graphic_viewer_tbl;*)
    Hashtbl.iter (fun _ o -> o#select_instant) object_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#select_instant) vector_viewer_tbl;
    Hashtbl.iter (fun _ o -> o#select_instant) matrix_viewer_tbl
  
  let clear () =
    Hashtbl.clear trace_browser_tbl;
    Hashtbl.clear value_sample_tbl;
    Hashtbl.clear text_viewer_tbl;
    Hashtbl.clear replay_panel_tbl
  (*Hashtbl.clear Dispatcher.graphic_viewer_tbl;*)
  
  let trace_browser_remove path =
    Hashtbl.remove trace_browser_tbl path
  
  let value_sample_remove path =
    Hashtbl.remove value_sample_tbl path
  
  let text_viewer_remove path =
    Hashtbl.remove text_viewer_tbl path
  
  let replay_panel_remove path =
    Hashtbl.remove replay_panel_tbl path
  
  let object_viewer_remove path =
    Hashtbl.remove object_viewer_tbl path
  
  let vector_viewer_remove path =
    Hashtbl.remove vector_viewer_tbl path
  
  let matrix_viewer_remove path =
    Hashtbl.remove matrix_viewer_tbl path
  
  let trace_browser_add path obj =
    Hashtbl.add trace_browser_tbl path obj
  
  let value_sample_add path obj =
    Hashtbl.add value_sample_tbl path obj
  
  let text_viewer_add path obj =
    Hashtbl.add text_viewer_tbl path obj
  
  let replay_panel_add path obj =
    Hashtbl.add replay_panel_tbl path obj
  
  let object_viewer_add path obj =
    Hashtbl.add object_viewer_tbl path obj
  
  let vector_viewer_add path obj =
    Hashtbl.add vector_viewer_tbl path obj
  
  let matrix_viewer_add path obj =
    Hashtbl.add matrix_viewer_tbl path obj
  
end

(* -------------------------------------------------------------------------
REPLAY_PANEL
------------------------------------------------------------------------- *)
class replay_panel geometry =

object (self)
  
  val top = new Toplevel.toplevel "synERJYsim Replay panel" ".stp.replay"
  val border = new Util_tk.bordered_frame ~left: 5
      ~top: 10 ~bottom: 10 ".stp.replay.brd"
  val frm = new Ttk.frame ".stp.replay.brd.frm"
  val lbl1 = new Ttk.label ~text:"Files" ".stp.replay.brd.frm.files"
  val lbl2 = new Ttk.label ~text:"left" ".stp.replay.brd.frm.left"
  val lbl3 = new Ttk.label ~text:"max" ".stp.replay.brd.frm.max"
  val lbl4 = new Ttk.label ~text:"actual" ".stp.replay.brd.frm.actual"
  
  val files = new Tk.text ".stp.replay.brd.frm.files_t"
  val left = new Tk.text ".stp.replay.brd.frm.left_t"
  val max = new Tk.text ".stp.replay.brd.frm.max_t"
  val act = new Tk.text ".stp.replay.brd.frm.actual_t"
  
  val pop = new Tk.popup_menu ".stp.replay.popup"
  
  (* ---------------------------------------------------------------------- *)
  val mutable _files = [||]
  val mutable _height = 1
  val mutable _width = 30
  val mutable _replay = false
  
  (* ---------------------------------------------------------------------- *)
  method top = top
  
  method close () =
    stp.replay_panel_ex <- false;
    stp.replay_panel_wp <- ref (Wm.geometry _stp);
    top#destroy
  
  (* ---------------------------------------------------------------------- *)
  method yview args =
    files#yview args;
    left#yview args;
    max#yview args;
    act#yview args
  
  method xview _ = ()
  (* ---------------------------------------------------------------------- *)
  method popup id line pos =
    let x = s2i pos.(1) and y = s2i pos.(2) in
      pop#entryconfigure 0 (fun _ -> self#delete_trace id line);
      pop#entryconfigure 1 (fun _ -> self#pause id line);
      pop#entryconfigure 2 (fun _ -> self#play id line);
      let rootx = Winfo.rootx files#path
      and rooty = Winfo.rooty files#path in
        pop#popup (rootx + x + 1) (rooty + y + 1);
        pop#activate 0
  
  (* ---------------------------------------------------------------------- *)
  method open_trace () =
    let file = Dialog.getOpenFile ~ext: sim.tr_suffix ~parent:".stp"
        ~filetypes: ("{{Simulator Traces} {"^sim.tr_suffix^"}} {{All Files}  {*}}")
        "get trace"
    in
      if file <> "" then (
        control#open_trace file;
        self#update_display;
      )
  
  (* ---------------------------------------------------------------------- *)
  method reset =
    self#update
  
  method update_panel =        (* replay_panel *)
    self#update_display;
    self#update
  
  method update_display =      (* replay_panel *)
    files#delete ();
    max#delete ();
    (* configure *)
    let replay_files = control#get_replay_files in
      _files <- Array.of_list (Constant.string2list replay_files);
      let h = max_height _files in
        files#configure [Option.Height(h + 1)];
        left#configure [Option.Height(h)];
        max#configure [Option.Height(h)];
        act#configure [Option.Height(h)];
        Array.iteri
          ( fun i f ->
                let n = if i = 0 then "" else "\n" in
                  files#insert ~tag: ("down "^(i2s i)) (n^f);
                  max#insert (n^(i2s (control#get_number_of_instants_of_trace f)));
                  tag_bind files#path (i2s i) "<Button-1>" Constant.wxy (self#popup f i)
          ) _files;
        self#update
  
  method update =        (* replay_panel *)
    left#delete ();
    act#delete ();
    _replay <- replay#is_active;
    Array.iteri
      ( fun i f ->
            let l = i2s i in
            let _act = control#get_actual_instant_of_trace f in
            let _left = (control#get_number_of_instants_of_trace f) - _act in
              if control#is_active_trace f
              then files#tag_configure l [Option.Fg("red")]
              else files#tag_configure l [Option.Fg("blue")];
              let n = if i = 0 then "" else "\n" in
                left#insert (n^(i2s _left));
                act#insert (n^(i2s _act))
      ) _files;
  
  (* ---------------------------------------------------------------------- *)
  method delete_trace file line =
    control#delete_trace file;
    self#update_display
  
  method pause file line =
    control#set_inactive_trace file;
    self#update_display
  
  method play file line =
    control#set_active_trace file;
    self#update_display
  
  (* ---------------------------------------------------------------------- *)
  initializer (
    
    top#set_cmd self#close;
    Wm.not_resizable top#path;
    
    if Constant.windowingsystem() = "aqua" then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (".stp.replay.mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: self#open_trace ~acc:"G" "Load trace file ...";
      top#bind "<Control-KeyPress-g>" self#open_trace;
      top#bind "<Control-KeyPress-G>" self#open_trace;
      ff#add_separator;
      ff#add_command ~cllbck: self#close ~acc:"W" "Close Window" ;
      top#bind "<<Window>>" self#close;
      
      let yscroll = new Ttk.scrollbar ~orientation:"vertical" self ".stp.replay.brd.frm.scroll" in
      
      (* display of file names *)
      
        lbl1#configure [ Option.Justify("left") ];
        files#configure [ Option.Height(20); Option.Width(stp.max_width_files); Option.Yscroll(yscroll#path) ];
        files#read_only;
        files#tag_configure"up" [ Option.Fg("red") ];
        
        left#configure [ Option.Width(stp.max_width_inst); Option.Height(10); Option.Yscroll(yscroll#path) ];
        left#read_only;
        max#configure [ Option.Width(stp.max_width_inst); Option.Height(10); Option.Yscroll(yscroll#path) ];
        max#read_only;
        act#configure [ Option.Width(stp.max_width_inst); Option.Height(10); Option.Yscroll(yscroll#path) ];
        act#read_only;
        
        Grid.grid [ lbl1#path; lbl2#path; lbl3#path; lbl4#path; "x"] [Grid.Sticky("nsew")];
        Grid.grid [ files#path; left#path; max#path; act#path; yscroll#path ] [Grid.Sticky("nsew")];
        
        frm#pack [Pack.Top];
        border#pack [Pack.Top];
        pop#add_command "Delete";
        pop#add_command "Pause";
        pop#add_command "Play";
        
        self#update_display
  )
  
end (* c_replay_panel *)

let mk_replay_panel ?(geometry = !(stp.replay_panel_wp)) () =
  let replay_panel = new replay_panel "500x200" in
    Dispatcher.replay_panel_remove replay_panel#top#path;
    Dispatcher.replay_panel_add replay_panel#top#path replay_panel

(* -------------------------------------------------------------------------
SELECT_FIELDS tool
------------------------------------------------------------------------- *)
class select_fields o geometry =

object(self)
  
  val top = new Toplevel.toplevel "Select" (".stp.sel_"^o#get_name)
  val top_left = new Ttk.label ~text:"  all signals" (".stp.sel_"^o#get_name^".all_lbl")
  val top_right = new Ttk.label ~text:"to be traced" (".stp.sel_"^o#get_name^".obs_lbl")
  
  val all = new Tk.listbox (".stp.sel_"^o#get_name^".all_l")
  val pos = new Tk.listbox (".stp.sel_"^o#get_name^".pos")
  val obs = new Tk.listbox (".stp.sel_"^o#get_name^".obs_l")
  
  val middle = new Util_tk.bordered_frame ~top: 30 ~bottom: 20 ~left: 10 ~right: 10 (".stp.sel_"^o#get_name^".middle")
  val copy = new Ttk.button ~text:"  >>   " (".stp.sel_"^o#get_name^".middle.copy")
  val kill = new Ttk.button ~text:"  <<   " (".stp.sel_"^o#get_name^".middle.kill")
  val close = new Ttk.button ~text:" Close " (".stp.sel_"^o#get_name^".middle.delete")
  
  val mutable _pointer = List.length o#get_selected_fields
  val mutable _c = 0
  
  (* ---------------------------------------------------------------------- *)
  method close () =
    Constant.destroy top#path
  
  (* ---------------------------------------------------------------------- *)
  method insert_pointer pos =
    _pointer <- obs#nearest pos.(2);
    self#update_display
  
  (* ---------------------------------------------------------------------- *)
  method copy () =
    let sel = List.map s2i all#curselection in
      if sel <> []
      then (
        let low = List.hd sel
        and up = Util.list_last sel in
        let insl = List.map (List.nth o#fields) sel in
          if sel = [] then () else
            o#set_selected_fields
              (Util.list_insert o#get_selected_fields _pointer insl);
          _pointer <- _pointer + (up - low + 1);
          all#selection_clear ();
          self#update_display
      )
  
  (* ---------------------------------------------------------------------- *)
  method kill () =
    let sel = List.map s2i obs#curselection in
      if sel <> []
      then (
        let low = List.hd sel
        and up = Util.list_last sel in
          if sel = [] then () else
            o#set_selected_fields
              (Util.list_replace o#get_selected_fields low up []);
          _pointer <- if low <= _pointer && _pointer <= up then low else
          if _pointer < low then _pointer else
            _pointer - (up - low + 1);
          obs#selection_clear ();
          self#update_display;
      )
  
  (* ---------------------------------------------------------------------- *)
  method update_display = (* select_fields *)
    obs#delete ();
    pos#delete ();
    _c <- 0;
    List.iter
      ( fun f ->
            if _c = _pointer
            then pos#insert ">"
            else pos#insert " ";
            obs#insert f#get_name;
            _c <- _c + 1
      ) o#get_selected_fields;
    if _c = _pointer
    then pos#insert ">"
    else pos#insert " ";
    stp.update_display()
  
  (* ---------------------------------------------------------------------- *)
  initializer (
    
    top#set_cmd self#close;
    Wm.not_resizable top#path;
    
    if Constant.windowingsystem() = "aqua" then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (top#path^".mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: self#close ~acc:"W" "Close Window";
      top#bind "<<Window>>" self#close;
      ff#add_separator;
      m#add_command ~cllbck: (stp.close true) ~acc:"S" "Save&Quit";
      top#bind "<<Window>>" (stp.close false);
      ff#add_command ~cllbck: (stp.close false) ~acc:"Q" "Quit";
      top#bind "<<Window>>" (stp.close false);
      
      let all_with_scrolls = new Util_tk.widget_with_scrolls (all:> Tk.scrollable_widget) (".stp.sel_"^o#get_name^".all")
      and obs_with_scrolls = new Util_tk.widget_with_scrolls (obs:> Tk.scrollable_widget) (".stp.sel_"^o#get_name^".obs") in
      
      let h = (List.length o#fields) + 1 in
      let h = if h > 30 then 30 else h in
      let w = 30 in
        all#configure [Option.Width(w); Option.Height(h); Option.Selectmode("extended"); Option.Fg("blue")];
        
        List.iter (fun f -> all#insert f#get_name) o#fields;
        
        copy#configure [Option.Cmd(self#copy)];
        kill#configure [Option.Cmd(self#kill)];
        close#configure [Option.Cmd(self#close)];
        
        copy#pack [Pack.Top];
        kill#pack [Pack.Top];
        close#pack [Pack.Top];
        
        obs#configure [Option.Width(w); Option.Height(h); Option.Selectmode("extended"); Option.Fg("blue")];
        pos#configure [Option.Width(1); Option.Selectmode("single"); Option.Yscroll(obs_with_scrolls#yscroll#path)];
        
        bind pos#path Constant.b1rel ~pattern: Constant.wxy self#insert_pointer;
        
        Grid.grid [ top_left#path; "-"; "-"; top_right#path ] [Grid.Sticky("nsew")];
        Grid.grid [ all_with_scrolls#path; middle#path; pos#path; obs_with_scrolls#path ] [Grid.Sticky("nsew")];
        
        self#update_display
  )
  
end (* c_select_fields *)

(* -------------------------------------------------------------------------
VALUE_SAMPLE tool
------------------------------------------------------------------------- *)
class value_sample wn obj upper_index number =

object (self)
  
  val top = new Toplevel.toplevel ("Sample: "^(obj#get_name)) ("."^wn)
  
  val pane = new Ttk.paned_window ("."^wn^".p")
  
  val left = new Ttk.frame ("."^wn^".p.left")
  val fill = new Util_tk.bordered_frame ~top: 1 ~left: 5 ~right: 5 ("."^wn^".p.left.fill")
  val previous = new Ttk.button ~text:"<" ("."^wn^".p.left.fill.previous")
  val next = new Ttk.button ~text:">" ("."^wn^".p.left.fill.next")
  
  val left_pane = new Ttk.paned_window ("."^wn^".p.left.p")
  val variables = new Tk.text ("."^wn^".p.left.p.var")
  val types = new Tk.text ("."^wn^".p.left.p.typ")
  
  val right = new Ttk.frame ("."^wn^".p.right")
  val frames = Array.init number (fun n -> new Ttk.frame ("."^wn^".p.right.val"^i2s n))
  val instants = Array.init number (fun n -> new Ttk.entry stp.max_width_val ("."^wn^".p.right.val"^i2s n^".inst"))
  val values = Array.init number (fun n -> new Tk.text ("."^wn^".p.right.val"^i2s n^".val"))
  
  val mutable _upper_index = upper_index
  val mutable _last = number - 1
  val mutable _is_mutable = upper_index = control#get_prev_instant + 1
  val mutable _options = ""
  val mutable _signals = Array.of_list obj#get_selected_fields
  
  method upper_index = _upper_index
  method number = number
  method obj = (obj: c_sE_target c_sE_object)
  
  (* ---------------------------------------------------------------------- *)
  method close () =
    Dispatcher.value_sample_remove wn;
    top#destroy
  
  (* ---------------------------------------------------------------------- *)
  method yview args =
    variables#yview args;
    types#yview args;
    Array.iter (fun w -> w#yview args) values
  
  method xview _ = ()
  
  (* ---------------------------------------------------------------------- *)
  method update_display = (* value_sample *)
    let fields = obj#get_selected_fields in
      _signals <- Array.of_list fields;
      let h = max_height _signals in
      let w = max_width_signals _signals in
      let tw = max_width_types _signals in
        variables#configure [Option.Height(h); Option.Width(w)];
        types#configure [Option.Height(h); Option.Width(tw)];
        Array.iter ( fun w -> w#read_only) instants;
        Array.iter
          ( fun w ->
                w#configure [Option.Height(h); Option.Width(stp.max_width_val)];
                w#read_only
          ) values;
        variables#delete ();
        types#delete ();
        Array.iteri
          ( fun i f ->
                let n = if i = 0 then "" else "\n" in
                  variables#insert (n^f#get_name);
                  types#insert (n^f#get_type)
          ) _signals;
        self#set_val_display
  
  (* ---------------------------------------------------------------------- *)
  method set_val_display =
    Array.iteri
      ( fun i (w: Ttk.entry) ->
            let ii = _upper_index - number + i in
              w#delete ();
              if ii >= 0 then w#insert (i2s ii)
      ) instants;
    Array.iteri
      ( fun i (w: Tk.text) ->
            w#delete ();
            Array.iteri
              ( fun j f ->
                    let ii = _upper_index - number + i in
                      if f#is_present_at ii then (
                        try ( match f#get_value_at ii with
                              | SNull ->
                                  w#insert "*\n"
                              | SLiteral (_, v, _) ->
                                  w#insert (v^"\n")
                              | SObject vl ->
                                  let x = i2s j in
                                  let tag = f#name^x in
                                    w#insert ~tag:"<object>\n" (tag^" blue");
                                    let mk_object_sample () =
                                      ignore (new object_sample f#name vl (w#path^"."^x))
                                    in
                                      w#tag_bind tag "<Button-1>" mk_object_sample
                              
                              | SVector vl ->
                                  let x = i2s j in
                                  let tag = f#name^x in
                                    w#insert ~tag:"<vector>\n" (tag^" blue");
                                    let mk_vector_sample () =
                                      ignore(new vector_sample f#name vl (w#path^"."^x))
                                    in
                                      w#tag_bind tag "<Button-1>" mk_vector_sample
                              | SMatrix vl ->
                                  let x = i2s j in
                                  let tag = f#name^x in
                                    w#insert ~tag:"<matrix>\n" (tag^" blue");
                                    let mk_matrix_sample () =
                                      ignore(new matrix_sample f#name vl (w#path^"."^x))
                                    in
                                      w#tag_bind tag "<Button-1>" mk_matrix_sample
                            )
                        with _ -> w#insert ".\n"
                      ) else (
                        w#insert "-\n"
                      )
              ) _signals;
      ) values;
    self#select_instant
  
  (* ---------------------------------------------------------------------- *)
  method reset =
    if _is_mutable
    then (
      _upper_index <- 0;
      self#set_val_display
    ) else self#close ()
  
  (* ---------------------------------------------------------------------- *)
  method update = (* value_sample *)
    if _is_mutable then _upper_index <- _upper_index + 1;
    self#set_val_display
  
  (* ---------------------------------------------------------------------- *)
  method next () =
    if _upper_index <= control#get_prev_instant then (
      _upper_index <- _upper_index + 1;
      self#set_val_display
    )
  
  (* ---------------------------------------------------------------------- *)
  method previous () =
    if _upper_index > number then _upper_index <- _upper_index - 1;
    self#set_val_display
  
  (* ---------------------------------------------------------------------- *)
  method select_instant =
    instants.(_last)#configure [Option.Bg("white")];
    values.(_last)#configure [Option.Bg("white")];
    let diff = stp.pres_instant - (_upper_index - number) in
      if _upper_index - number <= stp.pres_instant &
      stp.pres_instant < _upper_index && stp.pres_instant >= 0 then (
        instants.(diff)#configure [Option.Bg("lightgrey")];
        values.(diff)#configure [Option.Bg("lightgrey")];
        _last <- diff
      )
  
  (* ---------------------------------------------------------------------- *)
  method highlight =
    try instants.(_last)#configure [Option.Bg("white")];
        values.(_last)#configure [Option.Bg("white")]
    with _ -> ();
        for i = 0 to number - 1 do
          if stp.pres_instant < number then (
            instants.(stp.pres_instant)#configure [Option.Bg("lightgrey")];
            values.(stp.pres_instant)#configure [Option.Bg("lightgrey")]
          );
          _last <- stp.pres_instant
        done
  
  (* -------------------------------------------------------------------- *)
  initializer (
    
    top#set_cmd self#close;
    
    if Constant.windowingsystem() = "aqua" then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (top#path^".mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: self#close ~acc:"W" "Close Window";
      top#bind "<<Window>>" self#close;
      
      fill#configure [Option.Height(60)];
      previous#configure [Option.Width(3); Option.Cmd(self#previous)];
      next#configure [Option.Width(3); Option.Cmd(self#next)];
      previous#pack [Pack.Left];
      next#pack [Pack.Left];
      
      let yscroll = new Ttk.scrollbar ~orientation:"vertical" self
          ("."^wn^".p.right.yscroll") in
      (* display of signal names *)
      let h = max (max_height _signals) 6 in
      let w = max (max_width_signals _signals) 10 in
      let tw = max_width_types _signals in
        variables#configure [Option.Height(h + 1); Option.Width(w); Option.Yscroll(yscroll#path)];
        variables#read_only;
        types#configure [Option.Height(h + 1); Option.Width(tw); Option.Yscroll(yscroll#path)];
        types#read_only;
        left_pane#add variables#path;
        left_pane#add types#path;
        left_pane#pack [Pack.Bottom; Pack.Fillxy; Pack.Expand];
        fill#pack [Pack.Top; Pack.Fillx];
        
        Array.iteri
          ( fun i w ->
                w#pack [Pack.Top; Pack.Fillx];
                frames.(i)#pack [Pack.Left; Pack.Fillxy; Pack.Expand]
          ) frames;
        Array.iteri
          ( fun i w ->
                w#read_only;
                w#pack [Pack.Top; Pack.Fillx];
                values.(i)#pack [Pack.Left; Pack.Fillxy; Pack.Expand]
          ) instants;
        Array.iteri
          ( fun i w ->
                w#configure [Option.Height(h + 1); Option.Width(stp.max_width_val); Option.Yscroll(yscroll#path)];
                w#tag_configure "blue" [Option.Fg("blue")];
                w#read_only;
                w#pack [Pack.Bottom; Pack.Fillxy; Pack.Expand];
                values.(i)#pack [Pack.Left; Pack.Fillxy; Pack.Expand]
          ) values;
        
        (* vertical scrollbar *)
        yscroll#pack [Pack.Top; Pack.Filly; Pack.Expand];
        right#pack [Pack.Left; Pack.Filly];
        
        pane#add left#path;
        pane#add right#path;
        
        pane#pack [Pack.Left; Pack.Fillxy; Pack.Expand];
        
        self#update_display;
        
        top#raise;
        top#focus
  )
  
end

let mk_value_sample obj upper_index number geometry =
  let wn = "stp.sample_"^obj#get_name^"_"^(i2s upper_index)^"_"^(i2s number) in
  let value_sample = new value_sample wn obj upper_index number in
    Dispatcher.value_sample_remove wn;
    Dispatcher.value_sample_add wn value_sample

(* -------------------------------------------------------------------------
TRACE BROWSER tool
------------------------------------------------------------------------- *)
class trace_browser wn obj =

object (self)
  
  val top =
    new Toplevel.toplevel ~geometry: obj#get_geometry ("Trace Browser: "^obj#get_full_name) ("."^wn)
  
  val pane = new Ttk.paned_window ("."^wn^".p")
  val left = new Ttk.frame ("."^wn^".p.left")
  
  (* display of previous instant *)
  val left_top = new Ttk.frame ("."^wn^".p.left.top")
  val now = new Ttk.entry 6 ("."^wn^".p.left.top.instant")
  (* entry for got instant *)
  val goto = new Ttk.label ~text:" goto " ("."^wn^".p.left.top.goto")
  val enter = new Ttk.entry 6 ("."^wn^".p.left.top.e")
  
  (* varibles / types / values *)
  val left_pane = new Ttk.paned_window ("."^wn^".p.left.p")
  val variables = new Tk.text ("."^wn^".p.left.p.var")
  val types = new Tk.text ("."^wn^".p.left.p.typ")
  val values = new Tk.text ("."^wn^".p.left.p.val")
  
  val left_bottom = new Ttk.frame ("."^wn^".p.left.bottom")
  
  (* instances and traces *)
  val right = new Ttk.frame ("."^wn^".p.right")
  
  val right_t = new Ttk.frame ("."^wn^".p.right.top")
  val right_tf = new Ttk.frame ("."^wn^".p.right.top.fill")
  val instants = new Tk.text ("."^wn^".p.right.top.instant")
  
  val right_b = new Ttk.frame ("."^wn^".p.right.bot")
  val right_bf = new Ttk.frame ("."^wn^".p.right.bot.fill")
  val traces = new Tk.text ("."^wn^".p.right.bot.trace")
  
  val mutable _signals = Array.of_list (obj#fields: c_sE_target c_field list)
  
  val mutable _highlighted = - 1
  val mutable _c0 = 0
  val mutable _c1 = 0
  
  (* ------------------------------------------------------------------------- *)
  method obj = (obj: c_sE_target c_sE_object)
  
  (* ------------------------------------------------------------------------- *)
  method close () =
    obj#set_geometry (Wm.geometry top#path);
    Dispatcher.trace_browser_remove wn;
    top#destroy
  
  (* ------------------------------------------------------------------------- *)
  method xview args =
    traces#xview args;
    instants#xview args
  
  (* ------------------------------------------------------------------------- *)
  method yview args =
    variables#yview args;
    types#yview args;
    values#yview args;
    traces#yview args
  
  (* ------------------------------------------------------------------------- *)
  method update_display = (* trace_browser *)
    _signals <- Array.of_list obj#get_selected_fields;
    variables#delete ();
    types#delete ();
    let width = stp.max_width_trace + 1
    and h = max_height _signals
    and w = max_width_signals _signals
    and tw = max_width_types _signals in
      variables#configure [Option.Height(h); Option.Width(w)];
      types#configure [Option.Height(h); Option.Width(tw)];
      values#configure [Option.Height(h); Option.Width(stp.max_width_val)];
      traces#configure [Option.Height(h); Option.Width(width)];
      Array.iteri
        ( fun i f ->
              let t = f#get_type in
              let n = if i = 0 then "" else "\n" in
              let tag = ("down "^(i2s i)^" right") in
                variables#insert ~tag: tag (n^f#get_name);
                types#insert (n^t)
        ) _signals;
      
      self#generate_traces
  
  (* ------------------------------------------------------------------------- *)
  method set_val_display =
    values#delete ();
    Array.iteri
      ( fun i f ->
            if i > 0 then values#insert "\n";
            let x = i2s i in
            let cnormal = [Option.Fg("black"); Option.Font(normal_font())] in
            let cpresent = [Option.Fg("red"); Option.Font(bold_font())] in
            let tag = f#name^i2s i in
            let vtag = "down "^x^" "^tag^" right blue" in
              ( match f#get_value_at stp.pres_instant with
                | SNull ->
                    values#insert ~tag: ("down "^x^" right") ""
                | SLiteral (_, v, _) ->
                    values#insert ~tag: ("down "^x^" right") v
                | SObject vl ->
                    values#insert ~tag: vtag "<object>";
                    let mk_object_trace_sample () =
                      let w = values#path^"."^x in
                      let del() = Dispatcher.object_viewer_remove w in
                      let o = new object_trace_sample vl f del w in
                        Dispatcher.object_viewer_add w o
                    in
                      values#tag_bind tag "<Button-1>" mk_object_trace_sample
                | SVector vl ->
                    values#insert ~tag: vtag "<vector>";
                    let mk_vector_trace_sample () =
                      let w = values#path^"."^x in
                      let del() = Dispatcher.vector_viewer_remove w in
                      let v = new vector_trace_sample vl f del w in
                        Dispatcher.vector_viewer_add x v
                    in
                      values#tag_bind tag "<Button-1>" mk_vector_trace_sample
                | SMatrix vl ->
                    values#insert ~tag: vtag "<matrix>";
                    let mk_matrix_trace_sample () =
                      let w = values#path^"."^x in
                      let del() = Dispatcher.matrix_viewer_remove w in
                      let m = new matrix_trace_sample vl f del w in
                        Dispatcher.matrix_viewer_add w m
                    in
                      values#tag_bind tag "<Button-1>" mk_matrix_trace_sample
              );
              if f#is_present_at stp.pres_instant then (
                variables#tag_configure x cpresent;
              ) else (
                variables#tag_configure x cnormal;
              );
      ) _signals
  
  (* ------------------------------------------------------------------------- *)
  method generate_traces =
    traces#delete ();
    instants#delete ();
    _c0 <- - 1;
    _c1 <- 0;
    Array.iteri
      ( fun i s ->
            if i > 0 then traces#insert ~index: ((i2s (i + 1))^".end") "\n"
      ) _signals;
    for column = 0 to control#get_prev_instant do
      self#set_points column
    done;
    self#select_instant
  
  (* ------------------------------------------------------------------------- *)
  method set_points column =
    tK [| traces#path; "xview"; "scroll"; "1"; "unit" |];
    tK [| instants#path; "xview"; "scroll"; "1"; "unit" |];
    if _c0 >= 9
    then (
      _c0 <- 0;
      if _c1 >= 9
      then _c1 <- 0
      else _c1 <- _c1 + 1
    ) else
      _c0 <- _c0 + 1;
    if _c0 = 0
    then instants#insert (i2s _c1)
    else (
      if _c0 = 5
      then instants#insert ":"
      else instants#insert "."
    );
    Array.iteri
      (fun i f ->
            traces#insert ~index: ((i2s (i + 1))^".end") (f#get_traceinfo column)
      ) _signals
  
  (* ------------------------------------------------------------------------- *)
  method update = (* trace_browser *)
    if control#get_prev_instant = stp.pres_instant
    then (
      traces#xview [| "scroll"; "1"; "unit" |];
      instants#xview [| "scroll"; "1"; "unit" |]
    ) else (
      self#generate_traces
    );
    let range_begin = control#get_last_instant + 1
    and range_end = control#get_prev_instant in
      for i = range_begin to range_end do
        self#set_points i
      done
  
  (* ------------------------------------------------------------------------- *)
  method select_instant =
    now#delete ();
    if stp.pres_instant > - 1
    then
      now#insert (i2s stp.pres_instant)
    else
      now#insert "->";
    self#set_val_display;
    instants#see ("1."^(i2s stp.pres_instant));
    traces#see ("1."^(i2s stp.pres_instant));
    self#highlight
  
  (* ------------------------------------------------------------------------- *)
  method highlight =
    let nextinstant = stp.pres_instant + 1 in
      instants#tag_remove "hl";
      traces#tag_remove "hl";
      instants#tag_add ~first: ("1."^(i2s stp.pres_instant))
        ~last: ("1."^(i2s nextinstant)) "hl";
      Array.iteri
        ( fun i s ->
              traces#tag_add ~first: ((i2s (i + 1))^"."^(i2s stp.pres_instant))
                ~last: ((i2s (i + 1))^"."^(i2s nextinstant)) "hl";
        ) _signals
  
  (* ------------------------------------------------------------------------- *)
  method reset =
    now#delete ();
    now#insert "->";
    variables#delete ();
    instants#delete ();
    traces#delete ();
    Array.iteri
      ( fun i s ->
            if i = 0
            then
              traces#insert ""
            else
              traces#insert ~index: ((i2s (i + 1))^".end") "\n";
            variables#tag_configure (i2s i) [Option.Fg("black")]
      ) _signals;
    _highlighted <- - 1;
    _c0 <- - 1;
    _c1 <- 0
  
  (* ------------------------------------------------------------------------- *)
  method goto () =
    let s = enter#get in
      try stp.pres_instant <- s2i s;
          stp.select_instant();
      with _ ->
          stp.transcript_update ("goto: "^s^" is not an integer")
  
  (* ------------------------------------------------------------------------- *)
  method trace_hightlight pos =
    let index = traces#index pos.(1) pos.(2) in
    let column1 = Constant.column_of_index index in
      stp.pres_instant <- column1;
      stp.select_instant()
  
  (* ------------------------------------------------------------------------- *)
  method generate_sample pos =
    let index = instants#index pos.(1) pos.(2) in
    let column1 = Constant.column_of_index index in
      bind instants#path Constant.b1motion ~pattern: Constant.wxy (self#highlight_area column1);
      bind instants#path Constant.b1rel ~pattern: Constant.wxy (self#create_sample column1)
  
  (* ------------------------------------------------------------------------- *)
  method highlight_area column1 pos =
    instants#tag_remove "sl";
    let index = instants#index pos.(1) pos.(2) in
    let column2 = Constant.column_of_index index in
    let c1 = i2s column1 and c2 = i2s column2 in
      if column1 <= column2
      then instants#tag_add ~first: ("1."^c1) ~last: ("1."^c2) "sl"
      else instants#tag_add ~first: ("1."^c2) ~last: ("1."^c1) "sl"
  
  (* ------------------------------------------------------------------------- *)
  method create_sample column1 pos =
    let index = instants#index pos.(1) pos.(2) in
    let column2 = Constant.column_of_index index in
      if column1 = column2
      then (
        stp.pres_instant <- column1;
        stp.select_instant()
      ) else (
        if column1 < column2
        then
          let number = column2 - column1 in
            mk_value_sample obj column2 number ""
        else
          let number = column1 - column2 in
            mk_value_sample obj column1 number ""
      );
      instants#tag_remove "sl";
      instants#bind Constant.b1motion (fun () -> ());
      instants#bind Constant.b1rel (fun () -> ())
  
  (* ------------------------------------------------------------------------- *)
  initializer (
    
    top#set_cmd self#close;
    
    if Constant.windowingsystem() = "aqua"
    then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (top#path^".mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: self#close ~acc:"W" "Close Window";
      top#bind "<<Window>>" self#close;
      ff#add_separator;
      m#add_command ~cllbck: (stp.close true) ~acc:"S" "Save&Quit";
      top#bind "<<Window>>" (stp.close false);
      ff#add_command ~cllbck: (stp.close false) ~acc:"Q" "Quit";
      top#bind "<<Window>>" (stp.close false);
      
      let fu = new Tk.menu (top#path^".mbar.utils") in
        m#add_cascade "Utils" fu;
        let choose () = let _ = new select_fields obj "" in () in
          fu#add_command ~cllbck: choose ~acc:"I" "Select Fields";
          top#bind "<Control-KeyPress-i>" choose;
          top#bind "<Control-KeyPress-I>" choose;
          
          left_top#configure [Option.Relief(Option.Flat)];
          now#configure [Option.Justify("right")];
          now#read_only;
          goto#configure [Option.Anchor("e")];
          
          enter#bind "<Return>" self#goto;
          
          now#pack [Pack.Left; Pack.Padx("2m")];
          goto#pack [Pack.Left; Pack.Padx("2m"); Pack.Fillx; Pack.Expand];
          enter#pack [Pack.Right];
          
          _signals <- Array.of_list obj#get_selected_fields;
          let h = max_height _signals
          and w = max_width_signals _signals
          and tw = max_width_types _signals
          and vw = stp.max_width_val in
          
          let xscroll = new Ttk.scrollbar self ~orientation:"horizontal" ("."^wn^".p.right.bot.xscroll")
          and yscroll = new Ttk.scrollbar self ~orientation:"vertical" ("."^wn^".p.right.yscroll") in
          
            variables#configure [Option.Bd(0); Option.Yscroll(yscroll#path); Option.Width(w); Option.Height(h)];
            variables#read_only;
            types#configure [Option.Bd(0); Option.Yscroll(yscroll#path); Option.Width(tw); Option.Height(h)];
            types#read_only;
            values#configure [Option.Bd(0); Option.Yscroll(yscroll#path); Option.Width(vw); Option.Height(h)];
            values#read_only;
            values#tag_configure "blue" [Option.Fg("blue")];
            
            left_bottom#configure [Option.Height(14); Option.Relief(Option.Flat)];
            (* pack left side *)
            left_top#pack [Pack.Top; Pack.Fillx];
            left_bottom#pack [Pack.Bottom; Pack.Fillx];
            left_pane#add variables#path;
            left_pane#add types#path;
            left_pane#add values#path;
            left_pane#pack [Pack.Left; Pack.Fillxy; Pack.Expand];
            
            instants#read_only;
            instants#configure [Option.Height(1); Option.Fg("blue"); Option.Xscroll(xscroll#path)];
            instants#tag_configure "sl" [Option.Bg("lightblue")];
            instants#tag_configure "hl" [Option.Bg("lightgrey")];
            instants#pack [Pack.Left; Pack.Fillx; Pack.Expand];
            
            bind instants#path Constant.b1press ~pattern: Constant.wxy self#trace_hightlight;
            
            right_tf#configure [Option.Width(14)];
            right_tf#pack [Pack.Right; Pack.Filly];
            
            (* display of traces *)
            traces#configure [Option.Bd(0); Option.Xscroll(xscroll#path); Option.Yscroll(yscroll#path)];
            traces#tag_configure "hl" [Option.Bg("lightgrey")];
            traces#read_only;
            
            bind instants#path Constant.b1press ~pattern: Constant.wxy self#generate_sample;
            
            right_t#pack [Pack.Top; Pack.Fillx];
            
            xscroll#pack [Pack.Bottom; Pack.Fillx];
            traces#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
            
            right_b#pack [Pack.Left; Pack.Fillxy; Pack.Expand];
            right_bf#pack [Pack.Bottom; Pack.Fillx];
            yscroll#pack [Pack.Top; Pack.Filly; Pack.Expand];
            
            pane#add left#path;
            pane#add right#path;
            
            pane#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
            
            self#update_display
  )
  
end

let mk_trace_browser obj () =
  let wn = "stp.trace_"^obj#oid in
  let trace_browser = new trace_browser wn obj in
    Dispatcher.trace_browser_remove wn;
    Dispatcher.trace_browser_add wn trace_browser

(* ------------------------------------------------------------------------- *)
class preference_panel =

object(self)
  
  val top = new Toplevel.toplevel "synERJY Simulator Preferences" ".stp.prefs"
  
  val border = new Util_tk.bordered_frame ~top: 20 ~bottom: 20 ~left: 20 ~right: 20 ".stp.prefs.bd"
  val frame1 = new Ttk.frame ".stp.prefs.bd.f1"
  val showobjbrw = new Ttk.check_button ~text:" Show object browser" ".stp.prefs.bd.f1.ob"
  val showtrcbrw = new Ttk.check_button ~text:" Show top level trace browser" ".stp.prefs.bd.f1.cc"
  
  val lbl = new Ttk.label ".stp.prefs.bd.f1.lbl"
  val txt_lbl = new Ttk.label ~text:"  Text font "".stp.prefs.bd.f1.txt"
  val txt_size_lbl = new Ttk.label ~text:"  Size  " ".stp.prefs.bd.f1.tsize"
  val txt_size_box = new Ttk.combobox ".stp.prefs.bd.f1.tsize_e"
  
  val txt_weight_lbl = new Ttk.label ~text:" Weight  " ".stp.prefs.bd.f1.weight"
  val txt_weight_box = new Ttk.combobox ".stp.prefs.bd.f1.weight_e"
  
  val frame2 = new Util_tk.bordered_frame ~top: 10 ~bottom: 10 ~left: 40 ~right: 40 ".stp.prefs.f2"
  val save = new Ttk.button ~text:"Save" ".stp.prefs.f2.save"
  val close = new Ttk.button ~text:"Dismiss" ".stp.prefs.f2.discard"
  
  method clear () =
    txt_size_box#delete();
    txt_weight_box#delete()
  
  method set () =
    self#clear();
    let (f, size, weight) = se.sim_font in
      txt_size_box#set size;
      txt_weight_box#set weight
  
  method apply _ =
    let txt_size = txt_size_box#get
    and txt_weight = txt_weight_box#get in
      se.sim_font <-
      ( let (f, _, _) = se.sim_font in
          (f, txt_size, txt_weight)
      );
      self#set();
      Font.set Font.sim_font txt_size txt_weight(* ;
  Puctrl.status#has_changed() *)
  
  method save () =
    self#apply [||];
    save_preferences ();
    top#destroy
  
  initializer (
    top#set_cmd (fun () -> top#destroy);
    Wm.not_resizable top#path;
    
    txt_size_lbl#configure [Option.Width(8)];
    txt_size_box#state Constant.ReadOnly;
    txt_size_box#load_items se.font_sizes;
    txt_size_box#set_cmd self#apply;
    txt_weight_lbl#configure [Option.Width(8)];
    txt_weight_box#state Constant.ReadOnly;
    txt_weight_box#load_items se.font_weights;
    txt_weight_box#set_cmd self#apply;
    
    Grid.grid [showobjbrw#path; "-" ] [Grid.Sticky("nsew")];
    Grid.grid [showtrcbrw#path; "-" ] [Grid.Sticky("nsew")];
    (* Grid.grid [lbl#path; "x" ] [Grid.Sticky("nsew")];
    Grid.grid [ txt_lbl#path; "x"] [Grid.Sticky("nsew")];
    Grid.grid [ txt_size_lbl#path; txt_size_box#path] [Grid.Sticky("nsew")]; frame1#configure [Relief(Flat)];
    Grid.grid [ txt_weight_lbl#path; txt_weight_box#path]
    [Grid.Sticky("nsew")]; frame1#pack [Top];
    *)
    border#pack [Pack.Top];
    
    frame2#pack [Pack.Bottom; Pack.Fillx];
    frame2#configure [Option.Relief(Option.Flat)];
    save#configure [Option.Cmd(self#save)];
    save#pack [Pack.Right];
    close#configure [Option.Cmd(fun () -> top#destroy)];
    close#pack [Pack.Right];
    self#set()
  )
  
end

(* -------------------------------------------------------------------------
LOG CONSOLE
------------------------------------------------------------------------- *)
class log_console path =

object (self)
  
  val top = new Toplevel.toplevel "synERJY simulator log" path
  val txt = new Tk.text (path^".text")
  
  (* ------------------------------------------------------------------------- *)
  method update x =
    Wm.withdraw path;
    Wm.deiconify path;
    txt#insert ("\n"^x^"\n");
    txt#see "end"
  
  method show =
    Wm.deiconify path;
    Wm.raise path
  
  method close () =
    top#destroy
  
  (* ------------------------------------------------------------------------- *)
  initializer (
    
    top#set_cmd self#close;
    
    if Constant.windowingsystem() = "aqua" then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (top#path^".mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: self#close ~acc:"W" "Close Window";
      top#bind "<<Window>>" self#close;
      
      let fe = new Tk.menu (top#path^".mbar.edit") in
        m#add_cascade "Edit" fe;
        fe#add_command ~cllbck: txt#cut ~acc:"X" "Cut";
        top#bind "<<Cut>>" txt#cut;
        fe#add_command ~cllbck: txt#cut ~acc:"C" "Copy";
        top#bind "<<Copy>>" txt#cut;
        fe#add_command ~cllbck: txt#cut ~acc:"V" "Paste";
        top#bind "<<Paste>>" txt#copy;
        
        let scrolledtext = new Util_tk.widget_with_yscroll (txt:> Tk.scrollable_widget) (path^".scrlldtxt") in
          scrolledtext#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
          Wm.withdraw path
  )
  
end

(* -------------------------------------------------------------------------
SIGNAL INPUT tool
------------------------------------------------------------------------- *)
class signal_input =

object(self)
  
  val pane = new Ttk.paned_window ".stp.input"
  val variables = new Tk.text ".stp.input.var"
  val types = new Tk.text ".stp.input.typ"
  val values = new Tk.text ".stp.input.val"
  
  val mutable _prev = 0
  val mutable _inputs = ([||] : c_sE_target c_input array)
  
  val mutable ok = false
  
  (* ------------------------------------------------------------------------- *)
  method pack where =
    pane#pack where
  
  (* ------------------------------------------------------------------------- *)
  method yview args =
    variables#yview args;
    types#yview args;
    values#yview args
  
  method xview _ = ()
  
  (* ------------------------------------------------------------------------- *)
  method val_cut () =
    let index = variables#index_insert in
    let line = Constant.line_of_index index in
      _inputs.(line - 1)#unset_present;
      let line = i2s line in
        self#down line;
        if (( values#compare (line^".0") "<=" "sel.first" &&
            values#compare "sel.last" "<" (line^".end") )
          ||
          ( values#compare (line^".0") "<" "sel.first" &&
            values#compare "sel.last" "<=" (line^".end") ))
        then
          values#cut ()
        else (
          values#insert "";
          values#cut ()
        )
  
  (* ------------------------------------------------------------------------- *)
  method val_copy () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let line = i2s line in
      if (( values#compare (line^".0") "<=" "sel.first" &&
          values#compare "sel.last" "<" (line^".end") )
        ||
        ( values#compare (line^".0") "<" "sel.first" &&
          values#compare "sel.last" "<=" (line^".end") ))
      then
        values#copy ()
  
  (* ------------------------------------------------------------------------- *)
  method val_paste () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
      self#down (i2s line);
      _inputs.(line - 1)#unset_present;
      values#tag_remove "sel";
      values#paste ()
  
  (* ------------------------------------------------------------------------- *)
  method val_up () =
    values#setCursor (values#upDownLine "-1")
  
  (* ------------------------------------------------------------------------- *)
  method val_down () =
    values#setCursor (values#upDownLine "1")
  
  (* ------------------------------------------------------------------------- *)
  method val_right () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let line = i2s line in
      if not (values#compare "insert" ">=" (line^".end"))
      then
        values#setCursor "insert+1c"
  
  (* ------------------------------------------------------------------------- *)
  method val_left () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let line = i2s line in
      if not (values#compare "insert" "<=" (line^".0"))
      then
        values#setCursor "insert-1c"
  
  (* ------------------------------------------------------------------------- *)
  method val_keypress pos =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let s = _inputs.(line - 1) in
      self#down (i2s line);
      s#unset_present;
      values#textInsert pos.(0)
  
  (* ------------------------------------------------------------------------- *)
  method val_backspace () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let s = _inputs.(line - 1) in
      if Sim_util.is_primitive_or_void_typ s#get_type
      then (
        s#unset_present;
        let line = i2s line in
          self#down line;
          if (values#tag_nextrange "sel" "1.0" "end" = "")
          then (   (* empty selection *)
            if values#compare "insert" "<=" (line^".0")
            then ()
            else (
              values#delete_one "insert-1c";
              values#see "insert"
            )
          ) else ( (* non empty selection *)
            values#delete ~first:"sel.first" ~last:"sel.last" ()
          )
      )
  
  (* ------------------------------------------------------------------------- *)
  method val_delete () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let s = _inputs.(line - 1) in
      if Sim_util.is_primitive_or_void_typ s#get_type
      then (
        s#unset_present;
        let line = i2s line in
          self#down line;
          if (values#tag_nextrange "sel" "1.0" "end" = "")
          then (   (* empty selection *)
            if values#compare "insert" ">=" (line^".end")
            then ()
            else (
              values#delete_one "insert";
              values#see "insert"
            )
          ) else ( (* non empty selection *)
            values#delete ~first:"sel.first" ~last:"sel.last" ()
          )
      )
  
  (* ------------------------------------------------------------------------- *)
  method val_return () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    (* checking the format of inputs *)
      if self#setkeepSignal line
      then (
        values#mark_set "insert" (i2s (line + 1)^".end")
      )
  
  (* ------------------------------------------------------------------------- *)
  method select_line () =
    let index = values#index_insert in
    let line = Constant.line_of_index index in
    let l = i2s line in
      values#tag_add ~first: (l^".0") ~last: (l^".end") "sel"
  
  (* ------------------------------------------------------------------------- *)
  method val_button1 pos =
    let index = values#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
    let s = _inputs.(line - 1) in
      if Sim_util.is_primitive_or_void_typ s#get_type
      then (
        values#textButton1 pos.(1) pos.(2);
        values#tag_remove "sel";
        _prev <- line
      ) else (
        Wm.deiconify (values#path^"."^i2s line);
        Wm.raise (values#path^"."^i2s line)
      )
  
  (* ------------------------------------------------------------------------- *)
  method val_button1_motion pos =
    let index = values#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
      if Sim_util.is_primitive_or_void_typ _inputs.(line - 1)#get_type
      then (
        if _prev = line
        then (
          Constant.setPrivX pos.(1);
          Constant.setPrivY pos.(2);
          values#selectTo pos.(1) pos.(2)
        ) else (
          Constant.cancelRepeat()
        )
      )
  
  (* ------------------------------------------------------------------------- *)
  method val_button1_release pos =
    let index = values#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
      if Sim_util.is_primitive_or_void_typ _inputs.(line - 1)#get_type
      then (
        Constant.cancelRepeat()
      )
  (* ------------------------------------------------------------------------- *)
  method up line =
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "down";
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "keep";
    variables#tag_add ~first: (line^".0") ~last: (line^".end") "up"
  
  (* ------------------------------------------------------------------------- *)
  method down line =
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "up";
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "keep";
    variables#tag_add ~first: (line^".0") ~last: (line^".end") "down"
  
  (* ------------------------------------------------------------------------- *)
  method keep line =
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "down";
    variables#tag_remove ~first: (line^".0") ~last: (line^".end") "up";
    variables#tag_add ~first: (line^".0") ~last: (line^".end") "keep"
  
  (* ------------------------------------------------------------------------- *)
  method update_display =           (* signal_input *)
    variables#delete ();
    types#delete ();
    _inputs <- Array.of_list (control#root_object#get_selected_inputs);
    (* configure *)
    let h = max_height _inputs
    and w = max_width_signals _inputs
    and tw = max_width_types _inputs in
      variables#configure [Option.Height(h); Option.Width(w)];
      types#configure [Option.Height(h); Option.Width(tw)];
      values#configure [Option.Height(h); Option.Width(stp.max_width_val)];
      Array.iteri
        ( fun i s ->
              let n, t = s#get_name, s#get_type in
              let x = if i = 0 then "" else "\n" in
                variables#insert ~tag:"down" (x^n);
                types#insert (x^t)
        ) _inputs;
      self#update_value
  
  (* ------------------------------------------------------------------------- *)
  method setkeepSignal line =
    let l = i2s line in
    let s = _inputs.(line - 1) in
      if self#check_format s l then (
        if s#is_permanent then (
          self#keep l;
          s#set_permanent
        ) else (
          self#up l;
          s#set_present
        );
        true
      ) else (
        self#down l;
        s#unset_present;
        false
      )
  (* ------------------------------------------------------------------------- *)
  method setSignal pos =
    let index = variables#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
    let l = i2s line in
      values#mark_set "insert" (l^".end");
      let s = _inputs.(line - 1) in
        if self#check_format s l then (
          self#up l;
          s#set_present
        ) else (
          self#down l;
          s#unset_present;
        )
  
  (* ------------------------------------------------------------------------- *)
  method unsetSignal pos =
    let index = values#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
    let l = i2s line in
      values#mark_set "insert" (l^".end");
      self#down l;
      _inputs.(line - 1)#unset_present
  
  (* ------------------------------------------------------------------------- *)
  method keepSignal pos =
    let index = variables#index pos.(1) pos.(2) in
    let line = Constant.line_of_index index in
    let l = i2s line in
      values#mark_set "insert" (l^".end");
      let s = _inputs.(line - 1) in
        if self#check_format s l then (
          self#keep l;
          s#set_permanent
        ) else (
          self#down l;
          s#unset_present
        )
  
  (* ------------------------------------------------------------------------- *)
  method check_format (s: c_sE_target c_input) l =
    if Sim_util.is_primitive_or_void_typ s#valuetype && s#valuedim = []
    then (
      let v = values#get ~first: (l^".0") ~last: (l^".end") () in
        ok <- s#check_format v
    ) else (
      ok <- false;
      Event.generate pane#path ("<<CheckFormat-"^s#get_name^">>")
    );
    if ok then (
      stp.transcript_update "";
      values#tag_remove ~first: (l^".0") ~last: (l^".end") "error";
      true
    ) else (
      stp.transcript_update ("not of type "^ s#value_format);
      values#tag_add ~first: (l^".0") ~last: (l^".end") "error";
      false
    )
  
  (* ------------------------------------------------------------------------- *)
  method check_entry transcript s to_val line ?(index = None) e =
    let keypress () =
      if s#check_format (to_val()) then (
        e#configure [Option.Bg("White")];
        transcript#update "";
        stp.transcript_update ""
      ) else ( s#unset_present;
        self#down (i2s (line + 1));
        e#configure [Option.Bg("AliceBlue")];
        transcript#update ("not of type "^s#value_format)
      )
    in
      e#bind "<KeyRelease>" keypress;
  
  (* ------------------------------------------------------------------------- *)
  method check_input_format transcript s to_val i e =
    let err = "not of type "^s#value_format in
    let check_format () =
      if s#check_format (to_val ()) then (
        transcript#update "";
        Event.generate pane#path "<<CheckPassed>>"
      ) else (
        transcript#update err
      )
    and check_format_for_return () =
      if s#check_format (to_val()) then (
        s#set_present;
        transcript#update "";
        stp.transcript_update "";
        self#up (i2s (i + 1));
      ) else (
        transcript#update err;
        stp.transcript_update err
      )
    in
      e#bind "<Return>" check_format_for_return;
      pane#bind ("<<CheckFormat-"^s#get_name^">>") check_format
  
  (* ------------------------------------------------------------------------- *)
  method update_value =           (* signal_input *)
    Array.iteri
      ( fun i s ->
            let l = i2s (i + 1) in
              self#down l;
              if s#is_present
              then (
                if s#is_permanent (* next_instant !!!!!!! *)
                then self#keep l
                else self#up l
              )
      ) _inputs
  
  (* ------------------------------------------------------------------------- *)
  method reset =
    _inputs <- Array.of_list control#root_object#get_selected_inputs;
    self#update_value
  
  (* ------------------------------------------------------------------------- *)
  method focus = values#focus
  
  (* ------------------------------------------------------------------------- *)
  initializer (
    
    let yscroll = new Ttk.scrollbar ~orientation:"vertical" self ".stp.input.scr" in
    
      _inputs <- Array.of_list (control#root_object#get_selected_inputs);
      (* configure *)
      let height = max_height _inputs
      and varwidth = max_width_signals _inputs
      and typewidth = max_width_types _inputs in
      
      (* display of signal names *)
        variables#read_only;
        variables#configure [Option.Yscroll(yscroll#path); Option.Height(height); Option.Width(varwidth)];
        variables#tag_configure "up" [Option.Fg("red"); Option.Font(bold_font())];
        variables#tag_configure "down" [Option.Fg("blue"); Option.Font(normal_font())];
        variables#tag_configure "keep" [Option.Fg("LimeGreen"); Option.Font(bold_font())];
        
        (* display of signal types *)
        types#read_only;
        types#configure [Option.Yscroll(yscroll#path); Option.Height(height); Option.Width(typewidth)];
        
        (* display of signal values *)
        values#read_only;
        values#configure [Option.Yscroll(yscroll#path); Option.Height(height)];
        values#tag_configure "up" [Option.Fg("red"); Option.Font(bold_font())];
        values#tag_configure "down" [Option.Fg("blue"); Option.Font(normal_font())];
        values#tag_configure "keep" [Option.Fg("LimeGreen"); Option.Font(bold_font())];
        values#tag_configure "error"[Option.Bg("AliceBlue"); Option.Font(normal_font())];
        
        pane#add variables#path;
        pane#add types#path;
        pane#add values#path;
        
        yscroll#pack [Pack.Right; Pack.Filly];
        
        (* Bindings for setting signals *)
        tag_bind variables#path "down" Constant.b1press Constant.wxy self#setSignal;
        tag_bind variables#path "down" Constant.shiftB1press Constant.wxy self#keepSignal;
        tag_bind variables#path "up" Constant.b1press Constant.wxy self#unsetSignal;
        tag_bind variables#path "up" Constant.shiftB1press Constant.wxy self#keepSignal;
        tag_bind variables#path "keep" Constant.b1press Constant.wxy self#setSignal;
        tag_bind variables#path "keep" Constant.shiftB1press Constant.wxy self#keepSignal;
        
        (* the value widget as a restricted text widget *)
        bind values#path "<KeyPress>" ~pattern:"%A" self#val_keypress;
        values#bind "<Key-Up>" self#val_up;
        values#bind "<Key-Down>" self#val_down;
        values#bind "<Key-Right>" self#val_right;
        values#bind "<Key-Left>" self#val_left;
        values#bind "<Return>" self#val_return;
        values#bind "<KeyPress-BackSpace>" self#val_backspace;
        values#bind "<KeyPress-Delete>" self#val_delete;
        values#bind "<Control-KeyPress-l>" self#select_line;
        values#bind "<Control-KeyPress-L>" self#select_line;
        values#bind "<Double-Button-1>" self#select_line;
        
        (* global bindings *)
        let root = control#root_object in
          if Constant.windowingsystem() = "aqua" then (
            values#bind "<M1-KeyPress-O>" (stp.close false);
            values#bind "<M1-KeyPress-o>" (stp.close false);
            values#bind "<M1-KeyPress-s>" (stp.close false);
            values#bind "<M1-KeyPress-S>" (stp.close false);
            values#bind "<M1-KeyPress-q>" (stp.close false);
            values#bind "<M1-KeyPress-Q>" (stp.close false);
            values#bind "<M1-KeyPress-b>" (stp.object_browser Gui);
            values#bind "<M1-KeyPress-B>" (stp.object_browser Gui);
            values#bind "<M1-KeyPress-t>" (mk_trace_browser root);
            values#bind "<M1-KeyPress-T>" (mk_trace_browser root);
            values#bind "<M1-KeyPress-r>" mk_replay_panel;
            values#bind "<M1-KeyPress-R>" mk_replay_panel
          ) else (
            values#bind "<Control-KeyPress-O>" (stp.close false);
            values#bind "<Control-KeyPress-o>" (stp.close false);
            values#bind "<Control-KeyPress-s>" (stp.close false);
            values#bind "<Control-KeyPress-S>" (stp.close false);
            values#bind "<Control-KeyPress-q>" (stp.close false);
            values#bind "<Control-KeyPress-Q>" (stp.close false);
            values#bind "<Control-KeyPress-b>" (stp.object_browser Gui);
            values#bind "<Control-KeyPress-B>" (stp.object_browser Gui);
            values#bind "<Control-KeyPress-t>" (mk_trace_browser root);
            values#bind "<Control-KeyPress-T>" (mk_trace_browser root);
            values#bind "<Control-KeyPress-r>" mk_replay_panel;
            values#bind "<Control-KeyPress-R>" mk_replay_panel
          );
          
          bind values#path Constant.b1press ~pattern: Constant.wxy self#val_button1;
          bind values#path Constant.b1motion ~pattern: Constant.wxy self#val_button1_motion;
          bind values#path Constant.b1rel ~pattern: Constant.wxy self#val_button1_release;
          
          values#bind "<<Copy>>" self#val_copy;
          values#bind "<<Cut>>" self#val_cut;
          values#bind "<<Paste>>" self#val_paste;
          
          pane#bind "<<CheckPassed>>" (fun () -> ok <- true);
          
          values#focus;
          
          (* Generate the templates for inputs of structured data.
          Will be persistent, but withdrawn in the beginning
          *)
          Array.iteri
            ( fun i s ->
                  match s#value with
                  | SObject vl ->
                      let w = values#path^"."^i2s (i + 1) in
                      let obj = new object_input s#name vl w in
                        Array.iteri
                          ( fun index e ->
                                self#check_entry obj#transcript s obj#to_val i
                                  ~index: (Some index) e
                          ) obj#entries;
                        Array.iter
                          ( self#check_input_format obj#transcript s obj#to_val i
                          ) obj#entries
                  
                  | SVector vl ->
                      let w = values#path^"."^i2s (i + 1) in
                      let vec = new vector_input s#name vl w in
                        Array.iter
                          ( self#check_entry vec#transcript s vec#to_val i
                          ) vec#entries;
                        Array.iter
                          ( self#check_input_format vec#transcript s vec#to_val i
                          ) vec#entries
                  
                  | SMatrix vl ->
                      let w = values#path^"."^i2s (i + 1) in
                      let mat = new matrix_input s#name vl w in
                        Array.iter
                          (fun el ->
                                Array.iter
                                  ( self#check_entry mat#transcript s mat#to_val i
                                  ) el
                          ) mat#entries;
                        Array.iter
                          ( fun el ->
                                Array.iter
                                  ( self#check_input_format
                                      mat#transcript s mat#to_val i
                                  ) el
                          ) mat#entries
                  | _ -> ()
            ) _inputs;
          
          self#update_display;
          Array.iteri
            ( fun i s ->
                  let l = i2s (i + 1) in
                    if i > 0 then values#insert "\n";
                    match s#initvalue with
                    | SNull ->
                        values#insert ""
                    | SLiteral (_, v, _) ->
                        values#insert v
                    | SObject v ->
                        let tag = s#name^l in
                          values#insert ~tag: ("down "^l^" "^tag^" right blue")
                            "<object>"
                    | SVector vl ->
                        let tag = s#name^l in
                          values#insert ~tag: ("down "^l^" "^tag^" right blue")
                            "<vector>"
                    | SMatrix vll ->
                        let tag = s#name^l in
                          values#insert ~tag: ("down "^l^" "^tag^" right blue")
                            "<matrix>"
            ) _inputs;
          values#mark_set "insert" "1.end"
  )
  
end

(* -------------------------------------------------------------------------
CONTROL_PANEL tool
------------------------------------------------------------------------- *)
class control_panel =

object (self)
  
  val top = new Toplevel.toplevel "synERJY Simulator" ".stp"
  
  (* buttons *)
  val buttons = new Ttk.frame ".stp.buttons"
  
  val load = new Ttk.button ~text:"Load" ".stp.buttons.reload"
  val save = new Ttk.button ~text:"Save" ".stp.buttons.save"
  val quit = new Ttk.button ~text:"Quit" ".stp.buttons.quit"
  
  val sE_f = new Util_tk.bordered_frame ~left: 8 ~right: 8 ~bottom: 8 ".stp.buttons.f"
  val sE = new Ttk.label ".stp.buttons.f.sE"
  
  val line = new Ttk.frame ".stp.line"
  
  val top_right = new Ttk.frame ".stp.m"
  
  val controls = new Ttk.frame ".stp.m.top"
  val break = new Ttk.button ~text:"Break" ".stp.m.top.break"
  val store = new Ttk.button ~text:"Store" ".stp.m.top.store"
  val react = new Ttk.button ~text:"React" ".stp.m.top.react"
  val reset = new Ttk.button ~text:"Reset" ".stp.m.top.reset"
  
  val replay_f = new Ttk.frame ".stp.m.top.replay"
  val actual = new Ttk.entry 6 ".stp.m.top.instant"
  val replay_f1 = new Ttk.frame ".stp.m.top.fill1"
  val replay_f2 = new Ttk.frame ".stp.m.top.fill2"
  val replay_f3 = new Ttk.frame ".stp.m.top.fill3"
  val replay_f4 = new Ttk.frame ".stp.m.top.fill4"
  
  val trace = new Ttk.label ~text:"trace" ".stp.m.top.trace"
  
  val fill = new Ttk.frame ".stp.m.fill"
  
  val choice = new Ttk.frame ".stp.m.choices"
  val choice_f1 = new Ttk.frame ".stp.m.choices.fill1"
  val choice_f2 = new Ttk.frame ".stp.m.choices.fill2"
  
  val tozero = new Ttk.button ~text:"<<" ".stp.m.choices.<<"
  val previous = new Ttk.button ~text:"<" ".stp.m.choices.<"
  val next = new Ttk.button ~text:">" ".stp.m.choices.>"
  val toinstant = new Ttk.button ~text:">>" ".stp.m.choices.>>"
  
  val goto_f = new Ttk.frame ".stp.m.choices.goto"
  val goto_l = new Ttk.label ~text:" goto " ".stp.m.choices.goto.l"
  val goto_e = new Ttk.entry 6 ".stp.m.choices.goto.e"
  val choose = new Ttk.entry 6 ".stp.m.choices.instant"
  val timenow = new Ttk.entry 6 ".stp.m.choices.timenow"
  
  val signal_input = new signal_input
  val transcript = new transcript ".stp.cl"
  
  method react_button = react
  (* ------------------------------------------------------------------------- *)
  method about_aqua () =
    ignore (Dialog.messageBox ~default:"ok" "ok" "info"
          ("synERJYsim "^se.sesim_version^"\n\nCopyright:\n \
            Fraunhofer IAIS\n 2000 - 2007\n\n \
            http:// www.iais.fraunhofer.de /~sE\n")
      )
  
  (* ------------------------------------------------------------------------- *)
  method close save () =
    if save then self#save None ();
    Constant.destroy "."
  
  (* ------------------------------------------------------------------------- *)
  method react () =
    try control#react;
        stp.transcript_update ("Reaction time:   "^Int64.to_string sim_dyn.delta_time^" usec");
        tozero#state Constant.NotDisabled;
        previous#state Constant.NotDisabled;
        break#configure [Option.Text("Break"); Option.Cmd(self#set_break)];
    with e -> stp.update_log e
  
  (* ------------------------------------------------------------------------- *)
  method reset () =
    react#state Constant.NotDisabled;
    tozero#state Constant.Disabled;
    previous#state Constant.Disabled;
    next#state Constant.Disabled;
    toinstant#state Constant.Disabled;
    if control#soft_reset ~to_base_state: false then (
      Dispatcher.reset();
      stp.pres_instant <- control#get_prev_instant;
      actual#delete ();
      actual#insert (i2s ((control#get_prev_instant) + 1));
      stp.transcript_update "reset";
      self#select_instant
    )
  
  (* ------------------------------------------------------------------------- *)
  method open_trace () =
    let file =
      Dialog.getOpenFile ~ext: sim.tr_suffix ~parent:".stp"
        ~filetypes: ("{{Simulator Traces} {"^sim.tr_suffix^"}} {{All Files}  {*}}")
        "get trace"
         in
      if file <> "" then control#open_trace file
  
  (* ------------------------------------------------------------------------- *)
  method set_break () =
    control#set_break;
    break#configure [Option.Text("Unset"); Option.Cmd(self#unset_break)]
  
  (* ------------------------------------------------------------------------- *)
  method unset_break () =
    control#unset_break;
    break#configure [Option.Text("Break"); Option.Cmd(self#set_break)]
  
  (* ------------------------------------------------------------------------- *)
  method update_instant () = (* control_panel *)
    actual#delete ();
    actual#insert (i2s ((control#get_prev_instant) + 1));
    stp.pres_instant <- control#get_prev_instant;
    signal_input#update_value;
    Dispatcher.update ();
    self#select_instant
  
  (* ------------------------------------------------------------------------- *)
  val mutable _button_run_exists = None
  
  method private set_button_run =
    if replay#is_active then (
      if _button_run_exists = None then (
        let run = new Ttk.button ~text:" Run " ~cllbck: (fun () -> control#run) ".stp.run" in
        let cf = if Constant.windowingsystem() = "aqua" then [Option.Width(5); Option.CPady("4")] else [] in
          run#configure cf;
          replay_f#configure [Option.Bg("red")];
          run#pack [Pack.In(replay_f3#path)];
          _button_run_exists <- Some run;
      )
    ) else (
      match _button_run_exists with
      | None -> ()
      | Some run ->
          run#destroy;
          replay_f3#configure [Option.Width(0)];
          replay_f#configure [Option.Bg("white" )];
          _button_run_exists <- None;
    )
  
  method update_replay () = (* control_panel *)
    self#set_button_run;
    Dispatcher.update ()
  
  (* ------------------------------------------------------------------------- *)
  method update_display = (* control_panel *)
    signal_input#update_display;
    Dispatcher.update_display ()
  
  (* ------------------------------------------------------------------------- *)
  method update_entry txt = stp.transcript_update txt (* control_panel *)
  
  (* ------------------------------------------------------------------------- *)
  method select_instant =
    actual#delete ();
    timenow#delete ();
    if stp.pres_instant > - 1 then (
      let inst = stp.pres_instant in
        actual#insert (i2s inst);
        timenow#insert ( Int64.to_string sim_dyn.delta_time )
    ) else (
      actual#insert "->";
      timenow#insert "";
    );
    if control#get_prev_instant <= - 1 then (
      tozero#state Constant.Disabled;
      previous#state Constant.Disabled;
      next#state Constant.Disabled;
      toinstant#state Constant.Disabled
    ) else if stp.pres_instant <= - 1 then (
      tozero#state Constant.Disabled;
      previous#state Constant.Disabled;
      next#state Constant.NotDisabled;
      toinstant#state Constant.NotDisabled
    ) else if stp.pres_instant = control#get_prev_instant then (
      tozero#state Constant.NotDisabled;
      previous#state Constant.NotDisabled;
      next#state Constant.Disabled;
      toinstant#state Constant.Disabled
    ) else (
      tozero#state Constant.NotDisabled;
      previous#state Constant.NotDisabled;
      next#state Constant.NotDisabled;
      toinstant#state Constant.NotDisabled
    );
    Dispatcher.select_instant ()
  
  (* ------------------------------------------------------------------------- *)
  method tozero () =
    stp.pres_instant <- - 1;
    self#select_instant
  
  (* ------------------------------------------------------------------------- *)
  method previous () =
    stp.pres_instant <- stp.pres_instant - 1;
    self#select_instant
  
  (* ------------------------------------------------------------------------- *)
  method next () =
    stp.pres_instant <- stp.pres_instant + 1;
    self#select_instant;
    if stp.pres_instant >= control#get_prev_instant
    then signal_input#focus
  
  (* ------------------------------------------------------------------------- *)
  method toinstant () =
    stp.pres_instant <- control#get_prev_instant;
    self#select_instant;
    signal_input#focus
  
  (* ------------------------------------------------------------------------- *)
  method goto () =
    let s = goto_e#get in
      try stp.pres_instant <- s2i s;
          self#select_instant
      with _ ->
          stp.transcript_update "goto: not an integer"
  
  (* ------------------------------------------------------------------------- *)
  method save file () =
    let store = ref [] in
    let file = match file with
      | None -> Ast.se.file_prefix
          ^control#root_object#get_full_name^".sim"
      | Some file -> file
    in
      if stp.replay_panel_ex then
        store := (ReplayPanel(Wm.geometry ".stp.replay", replay#get_memento))::!store;
      Hashtbl.iter
        ( fun wn v ->
              let g = Wm.geometry top#path
              and n = v#obj#get_full_name
              and sfns = List.map (fun f -> f#get_full_name)
                  v#obj#get_selected_fields in
                store := (ValueSample(g, n, sfns, v#upper_index, v#number))::!store
        ) Dispatcher.value_sample_tbl;
      
      Hashtbl.iter
        ( fun wn f ->
              let g = Wm.geometry top#path
              and n = f#obj#get_full_name
              and sfns = List.rev_map (fun f -> f#get_full_name)
                  f#obj#get_selected_fields in
                store := (TraceBrowser(g, n, sfns))::!store
        ) Dispatcher.trace_browser_tbl;
      
      Hashtbl.iter
        ( fun wn f ->
              let g = Wm.geometry top#path
              and n = f#obj#get_full_name
              and sfns = List.rev_map (fun f -> f#get_full_name)
                  f#obj#get_selected_fields in
                store := (TextViewer(g, n, wn, f#get_label, sfns))::!store
        ) Dispatcher.text_viewer_tbl;
      
      (* Hashtbl.iter
      ( fun wn f ->
      let g = Wm.geometry top#path
      and n = f#obj#get_full_name
      and sfns = List.rev_map (fun f -> f#get_full_name)
      f#obj#get_selected_fields
      in
      store := (GraphicViewer(g, n, wn, f#fid, sfns))::!store
      ) Dispatcher.graphic_viewer_tbl;*)
      
      if stp.object_browser_ex
      then (Constant.set "object_browser" "yes";
        store := (ObjectBrowser(Wm.geometry ".stp.browser"))::!store
      );
      
      let root = control#root_object in
      let g = Wm.geometry _stp
      and n = root#get_full_name
      and sfns = List.map (fun f -> f#get_full_name)
          root#get_selected_inputs
      in
        store := (ControlPanel(g, n, sfns))::!store;
        
        let fileid = open_out_bin file in
          output_value fileid !store;
          close_out fileid
  
  (* ------------------------------------------------------------------------- *)
  method saveas () =
    let root_name = control#root_object#get_name in
    let file =
      Dialog.getSaveFile2 ~ext:".sim" ~parent: top#path
        ~initfile: (Ast.se.file_prefix^root_name^".sim")
        ~filetypes:"{{Simulator Configurations} {.sim}} {{All Files}  {*}}"
        "save configuration" in
      if file <> "" then self#save (Some file) ()
  
  (* ------------------------------------------------------------------------- *)
  method load_configuration file =
    trap "load_configuration";
    if control#soft_reset true
    then
      try
          let fileid =
            try open_in_bin file
            with _ ->
                raise (FalseSimEnv("file '"^file^"' not available")) in
          let conf = input_value fileid in
          let root = control#root_object in
            top#destroy;
            Dispatcher.clear ();
            List.iter
              ( function
                | ControlPanel(g, n, sfns) ->
                    let o = find_matching_object n sfns root in
                      ( match o with
                        | None -> raise (FalseSimEnv("object '"^n^"' not defined"))
                        | _ -> ()
                      );
                      let root = control#root_object in
                        root#check_and_select_inputs sfns
                | ObjectBrowser(g) ->
                    stp.object_browser_wp := g;
                    stp.object_browser Gui ()
                | TraceBrowser(g, n, sfns) ->
                    let o = find_matching_object n sfns root in
                      ( match o with
                        | None ->
                            raise (FalseSimEnv("object '"^n^"' not defined"))
                        | Some o ->
                            o#set_geometry g;
                            mk_trace_browser o ()
                      )
                | ValueSample(g, n, sfns, u, l) ->
                    let o = find_matching_object n sfns root in
                      ( match o with
                        | None ->
                            raise (FalseSimEnv("object '"^n^"' not defined"))
                        | Some o ->
                            mk_value_sample o u l g
                      )
                | ReplayPanel(g, mem) ->
                    mk_replay_panel ~geometry: g ();
                    replay#restore_memento mem
                | TextViewer(g, n, w, lbl, sfns) ->
                    let o = find_matching_object n sfns root in
                      ( match o with
                        | None ->
                            raise (FalseSimEnv("object '"^n^"' not defined"))
                        | Some o ->
                            !mk_text_viewer o w n lbl g ()
                      )
                | GraphicViewer(g, n, w, fid, sfns) ->
                    let o = find_matching_object n sfns root in
                      ( match o with
                        | None ->
                            raise (FalseSimEnv("object '"^n^"' not defined"))
                        | Some o ->
                            !mk_graphic_viewer o w n fid g ()
                      )
              ) conf;
            close_in fileid;
            
            self#update_instant ();
            self#update_replay ();
            self#update_entry "";
            self#update_display;
      with
      | FalseSimEnv(msg) ->
          control#message msg
      | e ->
          Sim_error.intern (Printexc.to_string e)
  
  (* ------------------------------------------------------------------------- *)
  method load_default () =
    trap "load_default";
    let fn = Ast.se.file_prefix
      ^control#root_object#get_full_name^".sim" in
      self#load_configuration fn
  
  (* ------------------------------------------------------------------------- *)
  method load () =
    trap "load";
    let rn = control#root_object#get_name in
    let fn =
      Dialog.getOpenFile ~ext:".sim" ~parent: top#path
        ~initfile: (Ast.se.file_prefix^rn^".sim")
        ~filetypes:"{Simulator Configurations} {.sim}} {{All Files}  {*}}"
        "open configuration"
    in
      self#load_configuration fn
  
  (* ------------------------------------------------------------------------- *)
  method save_trace () =
    let f =
      Dialog.getSaveFile2 ~ext: sim.tr_suffix ~parent: top#path
        ~initfile: (control#root_class#name^"."^sim.tr_suffix)
        ~filetypes: ("{{Simulator Traces} {"^sim.tr_suffix^
          "}} {{All Files}  {*}}")
        "Save input trace"
    in
      if not (f = "") then control#save_trace_into f
  
  (* ------------------------------------------------------------------------- *)
  initializer (
    
    if Constant.windowingsystem() = Constant.aqua
    then top#set_cmd (fun() -> ())
    else top#set_cmd (self#close true);
    
    if Constant.windowingsystem() = Constant.aqua
    then (
      Constant.show_preferences stp.preferences
    );
    
    let m = top#menu in
    let ff = new Tk.menu (".stp.mbar.file") in
      m#add_cascade "File" ff;
      ff#add_command ~cllbck: (sim_guard (self#open_trace)) ~acc:"O" "Load trace file ...";
      top#bind "<<Open>>" (sim_guard (self#open_trace));
      ff#add_separator;
      ff#add_command ~cllbck: (sim_guard self#load_default) ~underl: (- 1) "Open";
      ff#add_command ~cllbck: (sim_guard (self#load)) "Open ...";
      ff#add_separator;
      ff#add_command ~cllbck: (sim_guard (self#save None)) "Save";
      ff#add_command ~cllbck: (sim_guard (self#saveas)) ~underl: 5 "Save as ...";
      ff#add_separator;
      ff#add_command ~cllbck: (sim_guard (self#close true)) ~underl: (- 1) "Save&Quit";
      ff#add_command ~cllbck: (sim_guard (self#close false)) ~acc:"Q" "Quit";
      top#bind "<<Quit>>" (self#close false);
      
      let fe = new Tk.menu (".stp.mbar.edit") in
        m#add_cascade "Edit" fe;
        fe#add_command ~cllbck: signal_input#val_cut ~acc:"X" "Cut";
        fe#add_command ~cllbck: signal_input#val_copy ~acc:"C" "Copy";
        fe#add_command ~cllbck: signal_input#val_paste ~acc:"V" "Paste";
        fe#add_separator;
        if not(Constant.windowingsystem() = Constant.aqua)
        then fe#add_command ~cllbck: stp.preferences "Preferences";
        
        let fu = new Tk.menu (".stp.mbar.util") in
          m#add_cascade "Util" fu;
          fu#add_command ~cllbck: (stp.log_console Show "") "Log console";
          fu#add_separator;
          fu#add_command ~cllbck: (stp.object_browser Gui) ~underl: 7 ~acc:"B" "Object Browser";
          top#bind "<Control-KeyPress-b>" (stp.object_browser Gui);
          top#bind "<Control-KeyPress-B>" (stp.object_browser Gui);
          let root = control#root_object in
            fu#add_command ~cllbck: (mk_trace_browser root) ~underl: 1 ~acc:"T" "Top-level Trace";
            top#bind "<Control-KeyPress-t>" (mk_trace_browser root);
            top#bind "<Control-KeyPress-T>" (mk_trace_browser root);
            fu#add_command ~cllbck: mk_replay_panel ~underl: 5 ~acc:"R" "Replay Panel";
            top#bind "<Control-KeyPress-r>" mk_replay_panel;
            top#bind "<Control-KeyPress-R>" mk_replay_panel;
            
            let fh = new Tk.menu ".stp.mbar.help" in
              m#add_cascade "Help" fh;
              if not(Constant.windowingsystem() = Constant.aqua)
              then (
                fh#add_command ~cllbck: (Error.guard (Util_tk.Dialog.about se.sesim_version)) "About";
                fh#add_command ~cllbck: (Error.guard Util_tk.Dialog.help) ~acc:"H" "Help";
                top#bind "<<Help>>" (Error.guard Util_tk.Dialog.help)
              );
              
              (* left buttons *)
              
              let f = Constant.file_join [| se.se_home;"images";"synERJYtrace.gif" |] in
              let i = Image.create Image.Photo f in
                sE#configure [Option.Image(i)];
                
                line#configure [Option.Relief(Option.Ridge); Option.Width(2)];
                
                let cf = if Constant.windowingsystem() = Constant.aqua then [Option.Width(7)] else [] in
                  load#configure cf;
                  save#configure cf;
                  quit#configure cf;
                  
                  (* let pos = if se.gui = Aqua then [Top] else [Top; Fillx] in
                  reload#pack pos;`*)
                  if Constant.windowingsystem() = Constant.aqua
                  then (
                    save#pack [Pack.Top; Pack.Fillx];
                    quit#pack [Pack.Top; Pack.Fillx];
                  );
                  sE#pack [Pack.Bottom];
                  sE_f#pack [Pack.Bottom];
                  buttons#pack [Pack.Left; Pack.Filly];
                  line#pack [Pack.Left; Pack.Filly];
                  top_right#pack [Pack.Top; Pack.Fillx];
                  (*top_right#configure [Option.Bd(5)];*)
                  
                  break#configure [Option.Width(5); Option.Cmd(sim_guard self#set_break)];
                  store#configure [Option.Width(5); Option.Cmd(sim_guard self#save_trace)];
                  react#configure [Option.Width(5); Option.Cmd(sim_guard self#react)];
                  reset#configure [Option.Width(5); Option.Cmd(sim_guard self#reset)];
                  
                  replay_f#configure [Option.Height(10); Option.Width(10); Option.Relief(Option.Groove)];
                  actual#read_only;
                  actual#configure [Option.Justify("right")];
                  replay_f1#configure [Option.Width(5)];
                  replay_f2#configure [Option.Width(5)];
                  replay_f3#configure [Option.Width(0)];
                  replay_f4#configure [Option.Width(5)];
                  
                  if Constant.windowingsystem() = Constant.aqua then trace#configure [Option.Width(6)];
                  
                  controls#pack [Pack.Top; Pack.Fillx];
                  replay_f#pack [Pack.Left];
                  replay_f1#pack [Pack.Left];
                  actual#pack [Pack.Left];
                  replay_f2#pack [Pack.Left];
                  react#pack [Pack.Left];
                  reset#pack [Pack.Left];
                  replay_f3#pack [Pack.Left];
                  replay_f4#pack [Pack.Left; Pack.Expand];
                  store#pack [Pack.Right];
                  break#pack [Pack.Right];
                  trace#pack [Pack.Right];
                  
                  fill#configure [Option.Height(2); Option.Relief(Option.Flat)];
                  fill#pack [Pack.Fillx];
                  choice_f1#configure [Option.Width(15)];
                  choice_f2#configure [Option.Width(5)];
                  
                  tozero#configure [Option.Cmd(self#tozero); Option.Width(2)];
                  previous#configure [Option.Cmd(self#previous); Option.Width(2)];
                  next#configure [Option.Cmd(self#next); Option.Width(2)];
                  toinstant#configure [Option.Cmd(self#toinstant); Option.Width(2)];
                  
                  if Constant.windowingsystem() = Constant.aqua then goto_l#configure [Option.Width(6)];
                  goto_e#bind "<Return>" (self#goto);
                  actual#read_only;
                  actual#configure [Option.Justify("right")];
                  timenow#read_only;
                  timenow#configure [Option.Justify("right")];
                  choice#pack [Pack.Top; Pack.Fillx];
                  choice_f1#pack [Pack.Left];
                  actual#pack [Pack.Left];
                  timenow#pack [Pack.Left];
                  choice_f2#pack [Pack.Left];
                  tozero#pack [Pack.Left];
                  previous#pack [Pack.Left];
                  next#pack [Pack.Left];
                  toinstant#pack [Pack.Left];
                  goto_f#pack [Pack.Right];
                  goto_l#pack [Pack.Left];
                  goto_e#pack [Pack.Right];
                  
                  stp.transcript_update <- transcript#update;
                  transcript#configure [Option.Width(60)];
                  transcript#pack [Pack.Bottom; Pack.Fillx];
                  signal_input#pack [Pack.Top; Pack.Fillxy; Pack.Expand];
                  
                  self#update_display;
                  Wm.withdraw top#path;
                  Wm.deiconify top#path;
                  top#raise;
                  top#focus;
                  
                  if Constant.windowingsystem() = Constant.aqua then (
                    Wm.attributes top#path "notify" "1";
                    Wm.attributes top#path "topmost" "1";
                    
                  )
  )
end (* c_control_panel *)
(*
(* -------------------------------------------------------------------------
GRAPHIC_VIEWER tool
------------------------------------------------------------------------- *)
class c_graphic_viewer (obj: c_sE_target c_sE_object) w n fid =

object(self)

val ge_viewer = new Gredit_tk.GraphicEditorWindows.ge_viewer ()

val mutable _all_go = []

(* ------------------------------------------------------------------------- *)
method obj = obj
method fid = fid

(* ------------------------------------------------------------------------- *)
method close () =
Hashtbl.remove Dispatcher.graphic_viewer_tbl (obj#oid^i2s fid);
ge_viewer#top#destroy

(* ------------------------------------------------------------------------- *)
method select_instant =
let filter lbl =
match obj#get_class#get_srcpos lbl with
| LblGO dbg -> ge_viewer#highlight true (Graphic_util.id2go dbg.gselgo _all_go)
| _ -> ()
in
let rec check4highlight w' =
function
| [] -> ()
| lbl:: lbll ->
if w = w'
then highlight lbl
else check4highlight (w'^"_"^i2s lbl) lbll
in
List.iter (fun go -> Gredit_tk.highlight false go) _all_go;
List.iter
( fun dbg -> check4highlight (".stp."^o#oid) dbg.sim_lbl
) (o#get_dbgs_at stp.pres_instant)

(* ------------------------------------------------------------------------- *)
method reset =
List.iter (fun go -> Gredit_tk.highlight false go) _all_go

initializer (
if Winfo.exists w
then tk_raise w
else (
let file =
try List.assoc fid sim.target_header.target_files
with Not_found -> Err.intern "text_view: widget"
in
try Gredit_tk.load_file file
(Gredit_tk.ge_viewer (Gredit_tk.View(w, file))) ()
with _ -> control#message
("graphic file '"^file^" does not exist")
);
_all_go <- !Gredit_util.all_go;
self#select_instant
)

end;; (* c_graphic_viewer *)

mk_graphic_viewer := (
fun o w n fid geometry () ->
( try Hashtbl.remove graphic_viewer_tbl w
with _ -> ()
);
let graphic_viewer = new c_graphic_viewer o w n fid in
Hashtbl.add graphic_viewer_tbl w graphic_viewer;
graphic_viewer#widget geometry;
graphic_viewer#select_instant
)
*)
(* -------------------------------------------------------------------------
TEXT_VIEWER tool
------------------------------------------------------------------------- *)
class text_viewer obj path name label =
  
  let pos = match label with
    | None -> obj#get_class#source
    | Some lbl -> obj#get_class#get_text_pos lbl
  in
  
    object(self)
      
      val top = new Toplevel.toplevel name path
      
      val txt = new Tk.text (path^".t");
      
      val fid = pos.tselfilex
      
      val up_line = pos.originline
      val up_column = pos.origincol
      val low_line = pos.cornerline
      val low_column = pos.cornercol
      
      (* ------------------------------------------------------------------------- *)
      method get_label = label
      method obj = obj
      
      (* ------------------------------------------------------------------------- *)
      method close () =
        ( try Hashtbl.remove Dispatcher.text_viewer_tbl path
        with Not_found -> ()
        );
        top#destroy
      
      (* ------------------------------------------------------------------------- *)
      method reset =
        txt#tag_remove "halt";
        txt#tag_remove "emit";
        txt#tag_remove "text"
      
      (* ------------------------------------------------------------------------- *)
      method mk_links =
        List.iter
          ( function
            | SMTag (tg, clbl) ->
                let pos = obj#get_class#get_text_pos tg in
                let i0 = i2s (pos.originline - up_line + 1)^"."^i2s pos.origincol
                and i1 = i2s (pos.cornerline - up_line + 1)^"."^i2s pos.cornercol
                and tg = i2s tg in
                  txt#tag_add ~first: i0 ~last: i1 tg;
                  txt#tag_configure tg [Option.Fg("Blue")];
                  txt#tag_bind tg Constant.b1press
                    (!mk_text_viewer obj (path^"_"^tg)
                        (txt#get ~first: i0 ~last: i1 ()) (Some clbl) "")
            | SGTag (tg, fid) ->
                let pos = obj#get_class#get_text_pos tg in
                let i0 = i2s (pos.originline - up_line + 1)^"."^i2s pos.origincol
                and i1 = i2s (pos.cornerline - up_line + 1)^"."^i2s pos.cornercol
                and tg = i2s tg in
                  txt#tag_add ~first: i0 ~last: i1 tg;
                  txt#tag_configure tg [Option.Fg("Blue")];
                  txt#tag_bind tg Constant.b1press
                    (!mk_graphic_viewer obj (path^"_"^tg)
                        (txt#get ~first: i0 ~last: i1 ()) fid "")
          ) obj#get_class#get_tags
      
      (* ------------------------------------------------------------------------- *)
      method select_instant =
        let highlight lbl =
          match obj#get_class#get_srcpos lbl with
          | LblHaltSel dbg ->
              txt#tag_add "halt"
                ~first: (i2s (dbg.originline - up_line + 1)^"."^i2s (dbg.origincol - 1))
                ~last: (i2s (dbg.cornerline - up_line + 1)^"."^i2s dbg.cornercol)
          | LblEmitSel dbg ->
              txt#tag_add "emit"
                ~first: (i2s (dbg.originline - up_line + 1)^"."^i2s (dbg.origincol - 1))
                ~last: (i2s (dbg.cornerline - up_line + 1)^"."^i2s dbg.cornercol)
          | LblTextSel dbg ->
              txt#tag_add "text"
                ~first: (i2s (dbg.originline - up_line + 1)^"."^i2s (dbg.origincol - 1))
                ~last: (i2s (dbg.cornerline - up_line + 1)^"."^i2s dbg.cornercol)
          | LblGO dbg -> ()
        in
        let rec check4highlight path' =
          function
          | [] -> ()
          | lbl:: lbll -> if path = path'
              then highlight lbl
              else check4highlight (path'^"_"^i2s lbl) lbll
        in
          match label with
          | None -> ()
          | Some _ ->
              txt#tag_remove "halt";
              txt#tag_remove "emit";
              txt#tag_remove "text";
              List.iter
                ( fun dbg -> check4highlight (".stp."^obj#oid) dbg.sim_lbl
                ) (obj#get_dbgs_at stp.pres_instant)
      
      (* ------------------------------------------------------------------------- *)
      method highlight_syntax =
        ()
      
      (* ------------------------------------------------------------------------- *)
      initializer (
        
        top#set_cmd self#close;
        
        if Constant.windowingsystem() = "aqua" then (
          Constant.show_preferences stp.preferences
        );
        
        let m = top#menu in
        let ff = new Tk.menu (path^".mbar.file") in
          m#add_cascade "File" ff;
          ff#add_command ~cllbck: self#close ~underl: 7 ~acc:"W" "Close Window";
          txt#bind "<<Window>>" self#close;
          if not(Constant.windowingsystem() = Constant.aqua)
          then (
            ff#add_separator;
            ff#add_command ~cllbck: (stp.close true) ~acc:"S" "Save&Quit" ;
            ff#add_command ~cllbck: (stp.close false) ~acc:"Q" "Quit";
          );
          
          let txt_with_scrolls = new Util_tk.widget_with_scrolls (txt:> Tk.scrollable_widget) (path^".f") in
          let height = min stp.max_height_view (low_line - up_line + 1) in
            txt#read_only;
            txt#configure [Option.Width(stp.max_width_view); Option.Height(height)];
            txt#tag_configure "halt" [Option.Bg("Yellow")];
            txt#tag_configure "emit" [Option.Bg("AliceBlue")];
            txt#tag_configure "text" [Option.Bg("Yellow")];
            txt_with_scrolls#pack [Pack.Fillxy; Pack.Expand];
            
            let file =
              try List.assoc fid sim.target_header.target_files
              with Not_found -> Err.intern "text_view: widget"
            in
              ( try
                  let file_id = open_in file in
                    ( try
                        for i = 1 to up_line - 1 do
                          let _ = input_line file_id in ()
                        done;
                        for i = up_line to low_line do
                          txt#insert ((input_line file_id)^"\n")
                        done;
                        close_in file_id
                    with End_of_file -> close_in file_id
                    ) ;
              with Sys_error _ ->
                  control#message ("text file '"^file^" does not exist")
              );
              self#highlight_syntax;
              self#mk_links;
              self#select_instant
      )
      
    end;; (* c_text_viewer *)

mk_text_viewer :=
( fun o w n label geometry () ->
      let text_viewer = new text_viewer o w n label in
        ( match label with
          | None -> ()
          | Some lbl ->
              ( try
                  Hashtbl.remove Dispatcher.text_viewer_tbl w
              with Not_found -> ()
              );
              Hashtbl.add Dispatcher.text_viewer_tbl w text_viewer
        )
)
;;

(*
(* -------------------------------------------------------------------------
OBJECT_BROWSER tool
------------------------------------------------------------------------- *)
class stp_browser_widget parent =
object(self)
inherit [c_sE_target c_sE_object] c_browser_widget parent

method get_childs o = o#objects
method get_name o = o#get_name
method get_id o = o#get_name
method get_full_name o = o#get_full_name
method get_type o = " : "^o#get_type
method set_open o = o#set_open
method set_closed o = o#set_closed
method is_open o = o#is_open
method popup o (x: string array) =
let _p = _w^tk_popup in
tk_destroy _p;
se_menu _p;
tk_configure _p [Tearoff];
if o#get_class#is_reactive then (
se_menu_add_command (_p) ("Show traces")
~cllbck: (mk_trace_browser o) (- 1) "";
se_menu_add_command (_p) ("Show animation")
~cllbck: (!mk_text_viewer o (".stp."^o#oid)
(o#get_full_name) (o#get_class#constr_label) "") (- 1) ""
);
se_menu_add_command (_p) ("Show class")
~cllbck: (!mk_text_viewer o (".stp.c_"^o#get_class#name)
o#get_class#name None "") (- 1) "" ;
let rootx = tk_winfo_rootx _t
and rooty = tk_winfo_rooty _t in
let x = rootx + (s2i x.(1)) + 1
and y = rooty + (s2i x.(2)) + 1 in
tk_popup _p x y;
tk_activate _p 0
end (* stp_browser_widget *)

(* ------------------------------------------------------------------------- *)
class c_object_browser w =
object(self)

method widget geometry =
let m = w^".mbar" in
if Winfo.exists w
then tk_raise w
else (
new Toplevel.toplevel w "Object Browser" geometry self#close m;
se_menu m;
apple_menu m "About synERJYcharts" ("synERJYcharts"^se.ge_version)
stp.preferences;
let ff = ".stp.browser.mbar.file" in
se_menu_add_cascade m ff "File" 0;
se_menu ff;
se_menu_add_command ff "Close Window" ~cllbck: self#close 0 ~acc:"W";
tk_bind w "<<Window>>" self#close;
se_menu_add_separator ff;
se_menu_add_command ff "Save&Quit" ~cllbck: (stp.close true) 0 ~acc:"S";
tk_bind w "<<Window>>" (stp.close false);
se_menu_add_command ff "Quit Simulator" ~cllbck: (stp.close false) 0 ~acc:"Q";
tk_bind w "<<Window>>" (stp.close false);
let object_browser = new stp_browser_widget wn in
object_browser#widget;
object_browser#expand control#root_object 0 1;
)

method close () =
stp.object_browser_ex <- false;
stp.object_browser_wp := Wm.geometry ".stp.browser";
tk_destroy w

method update = ()         (* object_browser *)
method update_display = () (* object_browser *)
method reset = ()

end (* c_object_browser *)

(* ------------------------------------------------------------------------- *)
let the_object_browser = new c_object_browser ".stp.browser";;

stp.object_browser <-
( fun meth () ->
match meth with
| Gui ->
stp.object_browser_ex <- true;
the_object_browser#widget !(stp.object_browser_wp)
| Reset -> the_object_browser#reset
| Update -> the_object_browser#update
| Update_display -> the_object_browser#update_display
| Show -> ()
)
;;
*)
stp.mk_tkstepper <-
( fun () ->
      Wm.withdraw ".";
      
      stp.preferences <- (fun _ -> ignore(new preference_panel));
      
      let the_control_panel = new control_panel in
      let the_log_console = new log_console ".stp.log_console" in
        stp.update_instant <- the_control_panel#update_instant;
        stp.update_replay <- the_control_panel#update_replay;
        stp.update_log <-
        ( fun e ->
              match e with
              | Sim_error.SimParse (line, msg)
              -> stp.transcript_update
                    ("[[ParseError]] "^msg^" in line "^string_of_int line)
              | Sim_error.SimError msg
              -> the_log_console#show;
                  the_log_console#update msg;
                  the_control_panel#react_button#state Constant.Disabled;
                  stp.transcript_update "React button disabled due to error"
              | e -> stp.transcript_update (Printexc.to_string e)
        );
        stp.select_instant <- (fun () -> the_control_panel#select_instant);
        stp.update_display <- (fun () -> the_control_panel#update_display);
        stp.close <- (fun b -> the_control_panel#close b);
        stp.open_trace <- the_control_panel#open_trace;
        stp.initial_load <- (fun (fname: string) -> the_control_panel#load_configuration fname);
        
        stp.log_console <-
        ( fun meth x () ->
              match meth with
              | Update -> the_log_console#update x
              | Show -> the_log_console#show
              | _ -> ()
        );
        
        if sim.obj_brw then stp.object_browser Gui ();
        let root = control#root_object in
          root#set_geometry (if Constant.windowingsystem() = Constant.aqua
              then "920x300+20+370"
              else "920x300+20+400");
          the_control_panel#reset ();
          
          if sim.trace_brw
          then mk_trace_browser root ()
)
;;
