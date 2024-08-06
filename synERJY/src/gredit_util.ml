open Ast
open Ly
open Tk
open Graphic_util

(* --------------------------------------------------------------------------
unique () ... returns an unique int
-------------------------------------------------------------------------- *)
module Id =

struct
  let unique_ref = ref 1 (* local data *)
  
  let unique () =
    let n = !unique_ref in
    unique_ref := n + 1; n
end

(* --------------------------------------------------------------------------
info s ... writes string s to the info - window of the active toplevel - window
-------------------------------------------------------------------------- *)
module Info =

struct
  let entry = ref None
  
  let set_entry e = entry := (e: Ttk.entry option)
  let get_entry () = !entry
  
  let clear () =
    match !entry with
    | None -> ()
    | Some e -> (e: Ttk.entry)#delete ()
  
  let ps s =
    match !entry with
    | None -> ()
    | Some e -> (e: Ttk.entry)#delete ();
        e#insert s
  
  let err s =
    let _ = Util_tk.Dialog.messageBox ~default:"ok" "ok" "error" s in
    ()
end

(*| --------------------------------------------------------------------------
Context

module for handling the canvasses and tabs. A context is an integer key
- actual: actual context
- ttab: holds the information about state, tab path, and canvas
(note tabs are only locally known in ge_control)
- all: key - value list between context and the ttab information
-------------------------------------------------------------------------- *)

module Context =

struct
  
  type ttab = { state: tstate; canvas: Tk.canvas }
  
  let actual = ref (1 : int)
  let all = ref ([] : (int * ttab) list)
  
  let make_fct = ref (fun (st: tstate) -> ())
  let remove_tab_fct = ref (fun (ctxt: int) -> ())
  
  let add ctxt st cv =
    all := (ctxt,{ state = st; canvas = cv })::!all
  
  let exists ctxt = List.exists (fun (i, _) -> i = ctxt) !all
  
  let to_canvas ctxt =
    try (List.assoc ctxt !all).canvas
    with Not_found ->
        raise (Error ("Context.to_canvas for: "^string_of_int ctxt))
  
  let to_state ctxt =
    try (List.assoc ctxt !all).state
    with Not_found ->
        raise (Error ("Context.to_state for: "^string_of_int ctxt))
  
  let cv () = to_canvas !actual
  
  let make st =
    !make_fct st
  
  let remove ctxt =
    !remove_tab_fct ctxt;
    all := List.filter (fun (i, _) -> i <> ctxt) !all;
    if !actual = ctxt then ( (* the selected context has been destroyed *)
      actual := 1
    )
  
  let reset () =
    actual := 1;
    all := []
  
  let set ctxt = actual := ctxt
  let all_canvas () = List.map (fun (_, tab) -> tab.canvas) !all
  let all_contexts () = List.map (fun (i, _) -> i) !all
  let actual() = !actual
	
	let status = new observers
  
end

(*| --------------------------------------------------------------------------
Handle

module to organise the behaviour of handles.

Handles are Tk objects with the tag "handle".
-------------------------------------------------------------------------- *)

module Handle =

struct
  
  (* --------------------------------------------------------------------------
  mk_hdl x y ... makes handle (expresses selection of a go) at point (x, y)
  -------------------------------------------------------------------------- *)
  let mk_chdl x y color =
    ignore ((Context.cv())#create_rectangle (x - 4) (y - 2) (x + 4) (y + 2)
          [Option.Fill(color); Option.Tags(["handle"])])
  
  let make x y = mk_chdl x y "red"
  
  (* --------------------------------------------------------------------------
  mk_handles go ... attaches handles to a go to express selection, returns
  the pair (go,[handle;...])
  -------------------------------------------------------------------------- *)
  let mk_small_handles x y r =
    let x' = x - r
    and y' = y - r
    and x'' = x + r
    and y'' = y + r in
    make x' y'; make x' y''; make x'' y'; make x'' y''
  
  let make_for_go =
    function
    | State(st) ->
        let x = st.st_x and y = st.st_y
        and x' = st.st_x' and y' = st.st_y' in
        let x'' = (x + x') / 2 and y'' = (y + y') / 2 in
        make x y; make x y'; make x' y; make x' y'; make x'' y;
        make x'' y'; make x y''; make x' y''
    | Trans(t) ->
        let (fromx, fromy) = connect2pt t.tr_frco t.tr_from
        and (tox, toy) = connect2pt t.tr_toco t.tr_to in
        let midx = fromx + t.tr_midx
        and midy = fromy + t.tr_midy in
        let txtx = midx + t.tr_txtx
        and txty = midy + t.tr_txty in
        make fromx fromy;
        make midx midy;
        make tox toy;
        mk_chdl txtx txty "yellow"
    | Init(i)
    | Exit(i) -> mk_small_handles i.ie_x i.ie_y i.ie_r
    | Cond(c) -> mk_small_handles c.co_x c.co_y c.co_r
  
  (* --------------------------------------------------------------------------
  rm_handles (go, handlel) ... removes the handles from a go
  -------------------------------------------------------------------------- *)
  let remove () =
    List.iter (fun cv -> cv#delete "handle") (Context.all_canvas())
  
end

(*| --------------------------------------------------------------------------
selected_go ... refers to the list of selected items plus their handles
copied_go ... refers to the list of copied GO (copy / paste)
temp_handles... refers to temporary handles, may be removed anytime
all_go ... refers to the list of all graphic objects already created
all_canvas ... refers to triple id - canvas - toplevel of all canvas - widgets.
canvas ... refers to the id ( : int ) of the selected canvas
cv2tk c ... return the real TK - canvas - name corresponding to c
cv() ... return the real TK - canvas - name corresponding to !canvas
-------------------------------------------------------------------------- *)

module Selection =

struct
  
  let gos = ref ([] : tgraphic list)
  
  let get() =
    !gos
  
  let set_empty () =
    gos := []
  
  let reset() =
    Handle.remove();
    gos := []
  
  let redisplay_handles () =
    Handle.remove();
    List.iter Handle.make_for_go !gos
  
  let set gol =
    gos := gol;
    redisplay_handles ()
  
  let add gol =
    gos := gol @ !gos;
    redisplay_handles ()
  
  let print() =
    List.iter (fun go -> P.pi (go2id go); P.ps " ") !gos
  
  let is_in tkid =
    [] <>
    List.filter
      ( fun go -> (Context.actual() = go2parent go) && (List.mem tkid (go2tkids go))
      ) !gos
  
end

(* --------------------------------------------------------------------------
mk_copy ~idfn ~select from_list
... return the list of copied go's from from_list;
the go's id is changed according to idfn: int -> int
-------------------------------------------------------------------------- *)

module Copy =

struct
  
  let gos = ref ([] : tgraphic list)
  
  let reset () = gos:= []
  let set gol = gos:= gol
  let get () = !gos
  let exists () = !gos <> []
  
  let is_statelike go =
    match go with
    | State _
    | Init _
    | Exit _
    | Cond _ -> true
    | Trans _ -> false
  
  let cp_statelike copyid copyl go =
    match go with
    | State s -> (State { s with st_id = copyid s.st_id; st_tk = None }):: copyl
    | Init i -> (Init { i with ie_id = copyid i.ie_id; ie_tk = None }):: copyl
    | Exit i -> (Exit { i with ie_id = copyid i.ie_id; ie_tk = None }):: copyl
    | Cond c -> (Cond { c with co_id = copyid c.co_id; co_tk = None }):: copyl
    | Trans _ -> raise (Failure "cp_statelike")
  (* copy state-like go's only *)
  
  let cp_trans sl copyid copyl go =
    match go with
    | Trans t ->
        let find s = try List.find (fun x -> x = s) sl
          with Not_found ->
              raise (Failure "cp_trans: state not found") in
        (Trans { t with tr_id = copyid t.tr_id; tr_from = find t.tr_from ;
            tr_to = find t.tr_to; tr_tk = None
          }):: copyl
    | State _ -> copyl    (* state-like go are not copied *)
    | Init _ -> copyl
    | Exit _ -> copyl
    | Cond _ -> copyl
  
  let make ?(copyid = fun x -> x) gos =
    (* first: copy state,init,exit,cond & make assoc-list [(old,new);...] *)
    (* then:  copy transitions and replace old from/to by the new ones    *)
    let sl = List.filter is_statelike gos in
    let sl = List.fold_left (cp_statelike copyid) [] sl in
    let tl = List.fold_left (cp_trans sl copyid) [] gos in
    sl@ tl
  
  let offset x y =
    let x', y' =
      List.fold_left
        ( fun (x, y) go ->
              match go with
              | State(s) ->
                  min x s.st_x,
                  min y s.st_y
              | _ -> x, y
        ) (max_int, max_int) !gos in
    if x' = max_int && y' = max_int then 10, 10 else x - x', y - x'
  
  let create_new () =
    gos := make ~copyid: (fun _ -> Id.unique()) !gos
  
end

(* --------------------------------------------------------------------------
StateRefine - used by observers to determine whether a state is
to be refine
used as well to store for undo / redo
-------------------------------------------------------------------------- *)
module StateRefine =

struct
  
  let refine_fct = ref (fun (st: tstate) -> ())
  let undo_fct = ref (fun (st: tstate) -> ())
  let show_fct = ref (fun (st: tstate) -> ())
  
  let do_it st () = !refine_fct st
  let undo st () = !undo_fct st
  let show st () = !show_fct st
  
end

module GO =

struct
  
  let all_go = ref ([] : tgraphic list)
  let previous = ref ([] : tgraphic list)
  
  let set gol = all_go := gol
  let get () = !all_go
  
  let rec id2go id =
    try List.find (fun go -> go2id go = id) !all_go
    with Not_found ->
        raise (Failure "GO.id2go: go does not exist")
  
  let tkid2go tkid =
    try List.find
          ( fun go -> Context.actual() = go2parent go
                && List.mem tkid (go2tkids go)
          ) !all_go
    with Not_found ->
        raise (Failure "GOtkid2go: go does not exist")
  
  let cp () = Copy.make !all_go
  
    (* --------------------------------------------------------------------------
  add_to_all_go go ...
  rem_go_from_all_go go ...
  -------------------------------------------------------------------------- *)
  let add go =
    all_go := go:: !all_go
  
  let remove go =
    all_go := List.filter (fun x -> x <> go) !all_go

end

module UndoStack =

struct
  
  type tundokind = Change | Version
  
  let undo_stack = Stack.create()
  let redo_stack = Stack.create()
  let nb_undo_redo = ref 0
  
  let reset () =
    Stack.clear undo_stack;
    Stack.clear redo_stack;
    nb_undo_redo := 0
  
  let no_undo_redo () = !nb_undo_redo = 0
  
  let push () =
    Stack.clear redo_stack;
    nb_undo_redo := !nb_undo_redo + 1;
    Stack.push (Change, GO.cp()) undo_stack
  
  let push_version () =
    Stack.clear redo_stack;
    Stack.push (Version, GO.cp()) undo_stack
  
  let undo () =
    if not (Stack.is_empty undo_stack) then (
      nb_undo_redo := !nb_undo_redo - 1;
      let ch,gol = Stack.pop undo_stack in
      Stack.push (ch,GO.cp()) redo_stack;
      GO.set gol;
    )
  
  let rec to_last_version () =
    try match Stack.pop undo_stack with
        | (Version, gos) ->
            GO.set gos;
            Stack.clear redo_stack
        | _ -> to_last_version ()
    with Stack.Empty -> ()
  
  let redo () =
    try nb_undo_redo := !nb_undo_redo + 1;
        let ch,gol = Stack.pop redo_stack in
        Stack.push (ch, GO.cp()) undo_stack;
        GO.set gol;
    with Stack.Empty -> ()
    
end

module Cursor =

struct
  (* --------------------------------------------------------------------------
  make name ... changes the appearance of the cursor
  -------------------------------------------------------------------------- *)
  let make c n =
    let n' = if (n = "default") then "top_left_arrow" else n in
    match c with
    | None -> (Context.cv())#configure [Option.Cursor(n')]
    | Some c -> c#configure [Option.Cursor(n')]
  
  (* --------------------------------------------------------------------------
  get go handle_no ... cursor - shape for resize operations
  -------------------------------------------------------------------------- *)
  let get go handle_no =
    if (is_state go)
    then ( match handle_no with
       1 -> "top_left_corner"
      | 2 -> "top_side"
      | 3 -> "top_right_corner"
      | 4 -> "right_side"
      | 5 -> "bottom_right_corner"
      | 6 -> "bottom_side"
      | 7 -> "bottom_left_corner"
      | 8 -> "left_side"
      | 10 -> "xterm"
      | _ -> "fleur"
    )
    else if (is_trans go)
    then ( match handle_no with
       1 -> "based_arrow_down"
      | 2 -> "exchange"
      | 3 -> "based_arrow_up"
      | 11 -> "pencil"
      | _ -> "fleur"
    )
    else if (is_init go) || (is_exit go) || (is_cond go)
    then ( match handle_no with
       1 -> "top_left_corner"
      | 2 -> "top_right_corner"
      | 3 -> "bottom_right_corner"
      | 4 -> "bottom_left_corner"
      | 11 -> "pencil"
      | _ -> "fleur"
    )
    else "exchange"
end

(* --------------------------------------------------------------------------
see the note on design of tools in the documentation!
-------------------------------------------------------------------------- *)
module Tool = struct
  
  let tool_ref = ref (fun (_: int) (_: int) (_: (ttool_memory * tevent)) -> ())
  
  let mem_ref = ref NullMemory
  
  let activate_tool_ref =
    ref (fun (_: ttool) -> (Err.intern "activate_tool_ref": unit))
  
  let detach memory =
    mem_ref := memory
  
  (* --------------------------------------------------------------------------
  select_tool tool ... is called from tcl / tk, if a tool selection button is
  released. It de - colours the previously selected button, colours the
  selected button.
  Via ref - cell ''activate_tool_ref'' it calls the function ''activate_tool''
  of module ''driver''. This indirection is due to Ocaml's module system.
  -------------------------------------------------------------------------- *)
  let selected_tool = ref PointTool (* local data *)
  
  let tool_wdw =
    function
    | PointTool -> "point"
    | StateTool -> "state"
    | AndTool -> "and"
    | InitTool -> "init"
    | ExitTool -> "exit"
    | CondTool -> "cond"
    | TransTool -> "trans"
  
  let select tool () =
    selected_tool := tool;
    !activate_tool_ref tool;
    List.iter
      ( function cv -> Cursor.make (Some cv) "default"
      ) (Context.all_canvas())
end