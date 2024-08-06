(*

1. Abbreviations:
=================

- GE graphics editor
- GO graphical object from the GE - viewpoint
- item graphical object from the TK - viewpoint [term used in the documentation
of the TK canvas - widget]
- tkid identification of an item
- B1 mouse button 1 (left)
- B2 mouse button 2 (middle)

2. Architecture of the GOs:
===========================

The graphics editor GE maintains a list of graphical objects GOs. The list is
accessed via the ref - cell ''all_go''. A GO is
- a state
- a transition
- an inex [this is either an init or an exit]
- a condition

The GE uses TK to display the GOs on the screen. The GE maintains a list of
canvas' to achieve this. This list is accessed via the ref - cell
''all_canvas''. A GO is always either not displayed on the screen
or displayed completely in ONE canvas.

A GO consists of two kinds of data:
- go - related data, e.g. state - name, whether AND - decomposition should be done
horinzontally or vertically, etc.
- display - related data, e.g. the tkid's of the items used to display the GO.

Thus the following type - structure is used to design the GOs:

- tgraphic declares the sum - type for state, transition and inex and cond.
- tstate is a product - type for the state - related data, one field ''st_tk''
of type ''tstatetk option'' refers to the display - related data.
If its value is ''None'', the GO is not displayed at all.
- tstatetk is a product - type for the display - related data. It stores the
canvas used to display the GO and the tkid of the items presenting
the GO [remember, that the tkid's are unique within a canvas].
- ttrans like tstate for transitions
- ttranstk like tstatetk for transitions
- tinex like tstate for init or exit GOs
- tinextk like tstatetk for init or exit GOs
- tcond like tstate for condition GOs
- tcondtk like tstatetk for condition GOs

3. Note on the design of tools
==============================

It is assumed, that all tools are designed this way:
- tools are reactive, they are build like coroutines:
they are activated by an event, process it, and then detach (return
control to the main - loop of tcl / tk). The reaction is resumed when
the next event occurs.
- an event is a value of type event_type (see ast.ml)
- a sequence of events thus triggers a sequence of tool reactions.
- every tool has some memory to propagate information from one reaction
to the next one.
- to every tool a tool - function is associated. Its signature is:
int -> int -> (tool_memory_type * event_type)
- when a tool - function ''tf'' has finished its processing of the actual
event, it calls ''detach (memory_value)''.
- if the next event arrives, the event dispatcher retrieves the coordinates
of the mouse, say ''x'' and ''y'', constructs the event value, say
''event'', and calls the tool - function ''tf x y (memory_value, event)'',
using the memory saved from the last reaction.
- every tool uses pattern matching for a clearly arranged structure:
let tf x y = function
| (memory1, event1) -> ...; ...; detach (memory2)
| (memory2, event2) -> ...; ...; detach ...
The memory type is a sum type. Thus its constructor together with the
event can be easily used to select the right reaction
- implementation notes: due to the module - system of Ocaml, the
following problem arises: tcl / tk has to trigger the CHANGE between tools.
It calls ''!activate_tool_ref tool'' to achieve this. The ref - cell
''activate_tool_ref'' is never changed, but must be of ref type,
because Ocaml disallows cyclic use relations crossing module boundaries.

tool_ref ... refers to the tool - function
mem_ref ... refers to the memory of the tool between reactions

activate_tool_ref ... refers the the function, that activates a (newly)
selected tool. It always refers after startup to the
function ''activate_tool'' from module ''driver''.

4. Human - computer interface
===========================

The user executes commands w.t.h. of tools. A tool is activated by selecting
its button. The following tools are supported:
- PointTool ... to select a GO or a group of GOs, to move or resize GOs
- StateTool ... to create states
- AndTool ... to create AND - separator within a state
- TransTool ... to create transitions
- InexTool ... to create init - and exit - states
- CondTool ... to create condition - states

4.1 The StateTool, TransTool, InexTool and CondTool
===================================================

The StateTool, TransTool, InexTool and CondTool are simple to use. When they
are active, pressing and releasing B1 will create a new instance of the
corresponding GO. This includes the text items associated with a GO, if any.
The text items are initialized with a default string.

4.2 The PointTool
=================

The PointTool is used for SELECTION:

- to select a GO or a group of GOs. This is done either by clicking B1 close
to the shape of a GO, which is then selected. Clicking B1 far enough from any
GO will de - select previously select GOs. A group of GO is selected by
pressing B1, moving it to the right resp. bottom and then releasing B1.
This specifies a rectangle. All GO which are contained completely within
this rectangle are selected.
Any selected GO is augmented by (red) handles.

- to select an AND - separator of a state for moving it. If B1 is clicked close
to an AND - separator, this (part of a GO) is selected and augmented by
handles.

- to select text - items of states, transitions or conditions for moving them.
If B1 is clicked close to an text - items, this (part of a GO) is selected and
augmented by handles.

The PointTool is used for MOVE / RESIZE:

- to move or resize selected GOs [i.e. prior to these actions a selection
has to be performed using B1]. This action is performed by first pressing
B1, then moving the mouse, and then releasing B1.

A move is executed if:
- more than 1 GO is selected
- only one GO is selected AND B1 is pressed far enough from any handle of the
GO.
- an AND - separator or a text - item was selected.

A resize is executed if:
- only one GO is selected AND B1 is pressed close to a handle of the selected
GO.

The distance of the move resp. the amount of the resize is determined by the
move of the mouse between pressing and releasing B1.

The PointTool is used for OPEN TEXTEDITOR / REFINEMENT:
- Double clicking on a text opens the text editor
- Double clicking on a state creates a refinement window

4.3 The AndTool
===============

The AndTool creates AND - separators within states. If B1 is pressed close enough
to the shape of a state, a handle on the left (for horizontal) or on the top
(for vertical) initiates the creation of an AND - separator. If B1 is released,
the AND - separator is created. The first use of the AndTool for a state
determines for later uses whether horizontal or vertical AND - separators are
created.

4.6. Refinement
===================

A state may be and - or refined. In both cases it is possible to display this
refinement

- either in the same canvas as the state to be refined
- or in a separate canvas. This is called SepDisp (separate display).

If the RefineTool is activated, and a state is selected by

- clicking B1, a canvas with the refinement is displayed, if the state
has SepDisp set. If not, the click is ignored.

- clicking B2, the selected state gets SepDisp set. A new canvas is created,
- either empty, if no refinement exist
- with the refinement displayed, if some exists.

Using the tools available, the user may create or change the refinement of
the state w.t.h. of this canvas. The canvas may be closed and re - opened
without loss of data [hopefully :-)].

The shape of a state with existing separate refinemnts is drawn with a
thicker line to remind of the existing, but sometimes not visible refinement.

*)

open Ast
open Ly
open Tk
open Util_tk
open P
open Util
open Graphic_util
open Gredit_util
open Gredit_tk

(* ==========================================================================
TRANSITION - TOOL
========================================================================== *)
let trans_tool x y = function
	
	(* -------------------------------------------------------------------------- *)
	| (_, ActivateTool)
	| (_, Suspend_Tool) -> Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1press) ->                      (* starts TRANSITION CREATION *)
			( try
				let sel = GO.tkid2go ((Context.cv())#find_closest x y) in
				if not( (is_state sel) || (is_init sel) || (is_cond sel) ) then (
					Info.ps "invalid selection - ignored";
					Tool.detach NullMemory
				) else (
					try let selC = mk_connect x y sel in
						let (hx, hy) = connect2pt selC sel in
						Handle.make hx hy;
						Tool.detach (TransCreate(selC, sel, x, y))
					with _ ->
							Info.ps "invalid selection - ignored";
							Tool.detach NullMemory
				)
			with Not_found -> ()
			)
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1rel) -> Tool.detach NullMemory  (* terminates erroneous CREATION *)
	
	(* -------------------------------------------------------------------------- *)
	| (TransCreate(fromC, fromGo, x', y'), B1rel) ->  (* end of TRANSITION CREATION *)
			Handle.remove ();
			if (move_too_near x y x' y') then (
				Info.ps "invalid move - ignored";
				Tool.detach NullMemory
			) else (
				try let toGo = GO.tkid2go ((Context.cv())#find_closest ~halo:"20" x y) in
					if not((is_state toGo) || (is_exit toGo) || (is_cond toGo)) then (
						Info.ps "invalid selection - ignored";
						Tool.detach NullMemory
						
					) else (
						try UndoStack.push();
							let toC = mk_connect x y toGo in
							let t = Graphics.mk_trans fromGo fromC toGo toC (Graphics.mk_trans_text fromGo) in
							Display.display_go (Trans t);
							let tk_trans = some t.tr_tk "bind transition" in
							(Context.cv())#canvas_bind tk_trans.tr_tktxt Constant.doubleB1press
								( fun _ ->
											if !Tool.selected_tool = PointTool
											then TextEdit.transition (x, y) t
								);
							GO.add (Trans t);
							Tool.detach NullMemory
						with _ ->
								Info.ps "invalid creation - ignored";
								Tool.detach NullMemory
					)
				with Not_found -> ()
			)
	
	(* -------------------------------------------------------------------------- *)
	| (_, B2press) ->                                         (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* -------------------------------------------------------------------------- *)
	| (_, _) ->                                             (* invalid activation *)
			Info.ps "transition tool: sequence of user actions invalid - ignored"

(* ==========================================================================
INIT - TOOL
========================================================================== *)
let init_tool x y = function
	
	(* -------------------------------------------------------------------------- *)
	| (_, ActivateTool)
	| (_, Suspend_Tool)
	| (NullMemory, B1press) -> Tool.detach NullMemory (* starts the CREATION of INEX *)
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1rel) ->                   (* terminates the CREATION of INEX *)
			UndoStack.push();
			let init = Graphics.mk_init x y in
			Display.display_go init;
			GO.add init;
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (_, B2press) ->                                         (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* -------------------------------------------------------------------------- *)
	| (_, _) ->                                             (* invalid activation *)
			Info.ps "init tool: sequence of user actions invalid - ignored";
			Tool.detach NullMemory

(* ==========================================================================
Exit - TOOL
========================================================================== *)
let exit_tool x y = function
	
	(* -------------------------------------------------------------------------- *)
	| (_, ActivateTool)
	| (_, Suspend_Tool)
	| (NullMemory, B1press) -> Tool.detach NullMemory (* starts the CREATION of INEX *)
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1rel) ->                    (* terminates the CREATION of INEX *)
			UndoStack.push();
			let exit = Graphics.mk_exit x y in
			Display.display_go exit;
			GO.add exit;
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (_, B2press) ->                                          (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* -------------------------------------------------------------------------- *)
	| (_, _) ->                                              (* invalid activation *)
			Info.ps "init tool: sequence of user actions invalid - ignored";
			Tool.detach NullMemory

(* ==========================================================================
COND - TOOL
========================================================================== *)
let cond_tool x y = function
	
	(* -------------------------------------------------------------------------- *)
	| (_, ActivateTool)
	| (_, Suspend_Tool)
	| (NullMemory, B1press) -> Tool.detach NullMemory    (* starts the CREATION of COND *)
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1rel) ->                    (* terminates the CREATION of COND *)
			UndoStack.push();
			let c = Graphics.mk_cond x y in
			Display.display_go (Cond c);
			let tk_cond = some c.co_tk "bind condition" in
			(Context.cv())#canvas_bind tk_cond.co_tktxt Constant.doubleB1press
				( fun _ ->
							if !Tool.selected_tool = PointTool
							then (Handle.remove ();
								Handle.make (c.co_x + c.co_r) (c.co_y - c.co_r);
								TextEdit.condition (x, y) c
							)
				);
			GO.add (Cond c);
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (_, B2press) ->                                          (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* -------------------------------------------------------------------------- *)
	| (_, _) ->                                              (* invalid activation *)
			Info.ps "cond tool: sequence of user actions invalid - ignored";
			Tool.detach NullMemory

(* ==========================================================================
STATE - TOOL
========================================================================== *)
let state_tool x y = function
	
	(* -------------------------------------------------------------------------- *)
	| (_, ActivateTool) ->
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (_, Suspend_Tool) ->
			Handle.remove ();
			Cursor.make None "default";
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (NullMemory, B1press) ->                    (* starts the CREATION of STATES *)
			ignore ((Context.cv())#create_line
						[| (x + 10); y; x; y; x; (y + 10) |] [Option.Width(2); Option.Tags["handle"]]);
			Cursor.make None "bottom_right_corner";
			Tool.detach (StateCreate(x, y))
	
	(* -------------------------------------------------------------------------- *)
	| (StateCreate(xi, yi), B1rel) ->          (* terminates the CREATION of STATES *)
			Selection.reset();
			UndoStack.push();
			Cursor.make None "default";
			if valid_state_size xi yi x y 25 (AndRefine(0,[])) then (
				let st = Graphics.mk_state xi yi x y "sname" "" in
				Display.display_go (State st);
				GO.add (State st);
				Context.status#has_changed()
			);
			Tool.detach NullMemory
	
	(* -------------------------------------------------------------------------- *)
	| (_, B2press) ->                                          (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* -------------------------------------------------------------------------- *)
	| (_, _) ->                                              (* invalid activation *)
			Info.ps "state tool: sequence of user actions invalid - ignored";
			Tool.detach NullMemory

(* ==========================================================================
POINT - TOOL
be careful when modifying. The structure of this function is
( match memXevent with
| TERM ->
| TERM ->
| _ -> ( match memXevent with
| REM_TERM ->
| REM_TERM ->
| _ -> error
)
)
========================================================================== *)
let point_tool x y memXevent =
	( match memXevent with
		
		(* -------------------------------------------------------------------------- *)
		| (_, ActivateTool) ->
				Tool.detach NullMemory
		| (_, Suspend_Tool) ->
				Selection.reset ();
				Handle.remove ();
				Cursor.make None "default"; Tool.detach NullMemory
		
		(* -------------------------------------------------------------------------- *)
		| (PointSelect(xi, yi), B1rel) ->           (* terminates SELECTION *)
				Cursor.make None "default";
				Handle.remove();
				let sell = Graphics.mk_sel xi yi x y [] in
				Selection.set sell;
				Info.clear ();
				Selection.print();
				Tool.detach NullMemory
		
		(* -------------------------------------------------------------------------- *)
		| (PointSelect(xi, yi), ShiftB1rel) ->        (* terminates SELECTION EXTENSION *)
				Cursor.make None "default";
				let sell = Graphics.mk_sel xi yi x y [] in
				Selection.set sell;
				Info.clear ();
				Selection.print();
				Tool.detach NullMemory
		
		(* ---------------------------------------------------------------------- *)
		| (_, B2press) ->                          (* XXX context menu selected state *)
				let cv = Context.cv() in
				let seltk = cv#find_closest ~halo:"3" x y in
				if cv#find_overlapping (x - 5)(y - 5)(x + 5)(y + 5) = "" || not(Selection.is_in seltk)
				then ContextMenu.for_canvas x y
				else
					let selgo = GO.tkid2go seltk in
					( match selgo with
						| State st ->
								( match st.st_tk with
									| None -> ()
									| Some stk ->
											if stk.st_tkact = seltk then
												ContextMenu.for_action st seltk x y
											else if stk.st_tkname = seltk then
												ContextMenu.for_text seltk x y
											else
												ContextMenu.for_state st seltk x y
								)
						| Trans t ->
								( match t.tr_tk with
									| None -> ()
									| Some trk ->
											if trk.tr_tktxt = seltk
											then ContextMenu.for_text trk.tr_tktxt x y
											else ContextMenu.for_entity seltk x y
								)
						| Cond c ->
								( match c.co_tk with
									| None -> ()
									| Some cok ->
											if cok.co_tktxt = seltk
											then ContextMenu.for_text seltk x y
											else ContextMenu.for_entity seltk x y
								)
						| Init _ ->
								ContextMenu.for_entity seltk x y
						| Exit _ ->
								ContextMenu.for_entity seltk x y
					)
		
		(* ---------------------------------------------------------------------- *)
		| (TransTextMove(xi, yi), _) -> (* terminates SPECIAL MOVE of trans-TEXT *)
				Cursor.make None "default"; (* does not depend on mouse button/modifier *)
				( match Selection.get() with
					| [go] ->
							let dx = x - xi and dy = y - yi in
							( match go with
								| Trans(t) ->
										if not(dx = 0 || dy = 0) then (
											UndoStack.push ();
											t.tr_txtx <- t.tr_txtx + dx;
											t.tr_txty <- t.tr_txty + dy;
											Handle.remove();
											Display.display_go go;
											Selection.set [go];
											Tool.detach NullMemory
										) else (
											Tool.detach NullMemory
										)
								| _ -> Info.ps "invalid text move - ignored"
							)
					| _ -> Info.ps "invalid text move - ignored";
							Tool.detach NullMemory
				)
		
		(* ---------------------------------------------------------------------- *)
		| (AndMove(xi, yi, a, andl), _) ->              (* terminates SPECIAL MOVE of AND *)
				( match Selection.get() with     (* does not depend on mouse button/modifier *)
						[State(st) as go] ->
							let dx = x - xi and dy = y - yi in
							if dx = 0 && dy = 0
							then ( (* do nothing *)
							) else (
								let kind = Display.and_refine_kind st.st_refine in
								let andl = Sort.list (<) (a + (if kind = 0 then dy else dx):: andl) in
								if andl_valid (
										if kind = 0
										then st.st_y' - st.st_separ - st.st_y
										else st.st_x' - st.st_x) andl
								then (
									UndoStack.push ();
									st.st_refine <- (AndRefine(kind, andl));
									Handle.remove();
									Display.display_go go;
									Selection.set [go];
								)
								else (
									Info.ps "invalid use of and-tool - ignored"
								)
							)
					| _ -> Info.err "Error in moving an AND"
				);
				Tool.detach NullMemory
		
		(* ---------------------------------------------------------------------- *)
		| (PointMove(xi, yi), _) ->                                  (* terminates MOVE *)
				let dx = x - xi               (* does not depend on mouse button/modifier *)
				and dy = y - yi in
				Cursor.make None "default";
				if dx = 0 && dy = 0
				then ( (* nothing to do *)
				) else (
					try
						UndoStack.push ();
						let sell = Selection.get() in
						List.iter (Graphics.move dx dy) sell;
						List.iter Display.display_go sell;
						List.iter Display.display_go (Graphics.dependent_trans sell);
						Handle.remove();
						Selection.redisplay_handles()
					with _ ->
							Info.ps "invalid move - ignored"
				);
				Tool.detach NullMemory
		
		(* ---------------------------------------------------------------------- *)
		| (PointResize(xi, yi, kind), _) ->                         (* terminates RESIZE *)
				let dx = x - xi            (* does not depend on mouse button/modifier *)
				and dy = y - yi
				and go = List.hd (Selection.get()) in
				Cursor.make None "default";
				if dx = 0 && dy = 0
				then ( (* nothing to do *)
				) else (
					try if not (dx = 0 || dy = 0) then UndoStack.push ();
						Graphics.resize kind x y dx dy go;
						Display.display_go go;
						List.iter (Display.display_go) (Graphics.dependent_trans [go]);
						Handle.remove();
						Handle.make_for_go go;
						Selection.set [go]
					with _ ->
							Info.ps "invalid resize - ignored"
				);
				Tool.detach NullMemory
		
		(* catch all to compute the condition for move/resize, then re-match -------- *)
		| _ ->
				let ovlp = (Context.cv())#find_overlapping (x - 10)(y - 10)(x + 10)(y + 10) in
				let selEx =
					if ovlp = ""
					then false
					else
						let closest = (Context.cv())#find_closest ~halo:"20" ~start:"handle" x y in
						Selection.is_in closest
				in
				( match memXevent with
					(* ---------------------------------------------------------------------- *)
					| (NullMemory, e) when (e = B1press || e = ShiftB1press) && not selEx ->
					(* starts the SELECTION *)
							ignore (
									(Context.cv())#create_line
										[| (x + 10); y; x; y; x; (y + 10) |] [Option.Width(1); Option.Tags["handle"]]
								);
							Cursor.make None "sizing";
							Tool.detach (PointSelect(x, y))
					
					(* ---------------------------------------------------------------------- *)
					| (NullMemory, B1press) when selEx ->
					(* starts RESIZE or MOVE *)
							( try
								match Selection.get() with
								| [] ->
										Tool.detach NullMemory
								| [go] ->
										let h' = at_handle x y go in
										Cursor.make None (Cursor.get go h');
										if h' = 0
										then
											if (at_an_and x y go) then
												let (a, al) = Graphics.update_and x y go in
												Tool.detach (AndMove(x, y, a, al))
											else if (at_trans_text x y go) then
												Tool.detach (TransTextMove(x, y))
											else
												Tool.detach (PointMove(x, y))
										else
											Tool.detach (PointResize(x, y, h'))
								| _ ->
										Cursor.make None "fleur";
										Tool.detach (PointMove(x, y))
							with _ ->
									Info.ps "invalid move - ignored";
									Tool.detach NullMemory
							)
					
					(* ---------------------------------------------------------------------- *)
					| (_, _) ->                                   (* invalid activation *)
							Info.ps "point tool: sequence of user actions invalid - ignored";
							Tool.detach NullMemory
				)
	)
(* ==========================================================================
AND - TOOL
========================================================================== *)
let and_tool x y = function
	
	(* ---------------------------------------------------------------------- *)
	| (_, ActivateTool) -> Tool.detach NullMemory
	| (_, Suspend_Tool) -> Tool.detach NullMemory
	
	(* ---------------------------------------------------------------------- *)
	| (NullMemory, B1press) ->                              (* starts creating AND *)
			( try let sel = GO.tkid2go ((Context.cv())#find_closest x y) in
				( match sel with
					| State(st) ->
							if (Display.and_refine st.st_refine) then
								if (Display.and_refine_l st.st_refine) = [] then
									if min (absdiff st.st_x x) (absdiff st.st_x' x) <
									min (absdiff st.st_y y) (absdiff st.st_y' y)
									then ((* draw the lines horizontally *)
										st.st_refine <- (AndRefine(0,[]));
										Handle.make st.st_x y;
										Tool.detach (AndCreate(sel))
									) else ((* draw the lines vertically *)
										st.st_refine <- (AndRefine(1,[]));
										Handle.make x st.st_y;
										Tool.detach (AndCreate(sel))
									)
								else (
									(* there exist some ANDs already *)
									if (Display.and_refine_kind st.st_refine) = 0
									then Handle.make st.st_x y
									else ( Handle.make x st.st_y);
									Tool.detach (AndCreate(sel))
								)
					| _ -> Info.ps "invalid selection - ignored";
							Tool.detach NullMemory
				)
			with Not_found -> ()
			)
	
	(* ---------------------------------------------------------------------- *)
	| (AndCreate(go), B1rel) ->                         (* terminates creating AND *)
			Handle.remove ();
			( match go with
				| State(st) ->
						let kind = Display.and_refine_kind st.st_refine in
						let andl =
							Sort.list (<)
								( ( if kind = 0
										then (y - st.st_y - st.st_separ)
										else (x - st.st_x)
									):: (Display.and_refine_l st.st_refine)
								)
						in
						if andl_valid (
								if kind = 0
								then st.st_y' - st.st_separ - st.st_y
								else st.st_x' - st.st_x
							)
							andl
						then (
							UndoStack.push ();
							st.st_refine <- (AndRefine(kind, andl));
							Display.display_go go
						) else (
							Info.ps "invalid use of and-tool - ignored"
						)
				| _ -> Info.ps "error in and-tool - user actions ignored"
			);
			Tool.detach NullMemory
	
	(* ---------------------------------------------------------------------- *)
	| (_, B2press) ->                                          (* XXX context menu *)
			ContextMenu.for_canvas x y
	
	(* ---------------------------------------------------------------------- *)
	| (_, _) ->                                              (* invalid activation *)
			Info.ps "and tool: sequence of user actions invalid - ignored";
			Tool.detach NullMemory

(* ==========================================================================
END TOOL_FUNCTIONS
========================================================================== *)

(* --------------------------------------------------------------------------
activate_tool tool ... called via ref - cell activate_tool_ref from
''tk_base.ml''
- calls the old tool to suspend it with event Suspend_Tool, and
- calls the new tool to activate it with event ActivateTool
-------------------------------------------------------------------------- *)
let cursor = ref "top_left_arrow"

let activate_tool tool =
	Info.clear ();
	!Tool.tool_ref 0 0 (!Tool.mem_ref, Suspend_Tool);
	let tool_fn =
		match tool with
		| PointTool -> cursor:="top_left_arrow"; point_tool
		| StateTool -> cursor:="tcross"; state_tool
		| AndTool -> cursor:="tcross"; and_tool
		| InitTool -> cursor:="top_left_arrow"; init_tool
		| ExitTool -> cursor:="top_left_arrow"; exit_tool
		| CondTool -> cursor:="top_left_arrow"; cond_tool
		| TransTool -> cursor:="tcross"; trans_tool in
	tool_fn 0 0 (NullMemory, ActivateTool);
	Selection.reset();
	Tool.tool_ref := tool_fn

(* --------------------------------------------------------------------------
startup
-------------------------------------------------------------------------- *)
let parse_cmdline_args () =
	let help = "<file> :<file> is the command-file loaded initially" in
	Arg.parse
		[ ("-f", Arg.String( function s -> se.cmd_file <- s ), help)]
		( function _ -> ps "invalid anonymous argument\n"; pF() )
		"supported options:"

let init_ge () =
	
	parse_cmdline_args ();
	( try Cmd.process_file se.cmd_file with _ -> () );
	
	tk_start ~window_name:"" ~class_name:"" "";
	
	let serc = Filename.concat se.home ".serc" in
	if Sys.file_exists serc then (
		Cmd.process_file serc
	) else (
		let serc = Filename.concat se.se_home ".serc" in
		if Sys.file_exists serc
		then Cmd.process_file serc
		else ()
	);
	
	se.open_directory <- Sys.getcwd();
	se.save_directory <- Sys.getcwd();
	
	Util_tk.mk_environment ();
	
	let (_, se_size, se_weight) = se.se_font
	and (_, ge_size, ge_weight) = se.ge_font in
	Font.set Font.se_font se_size se_weight;
	Font.set Font.ge_font ge_size ge_weight;
	
	Ttk.Style.configure "Toolbutton" [Option.Padding(1)];
	
	Tool.activate_tool_ref := activate_tool;
	ignore(new GraphicEditorWindows.ge_control ());
	
	start_output2wdw (Info.ps) (Info.clear) (fun() -> ());
	Wm.withdraw ".";
	tk_main_loop ();
	stop_output2wdw ();
	activate_tool PointTool;
	Error.print ()

let _ = Callback.register "init_ge" init_ge
