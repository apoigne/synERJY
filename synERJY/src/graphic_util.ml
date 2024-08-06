open Ast

(* --------------------------------------------------------------------------
abs_diff x x' ... returns the abs value of the difference between x and x'
-------------------------------------------------------------------------- *)
let absdiff x x' =
	let d = x - x' in
	if d >= 0
	then d
	else (- d)

(* --------------------------------------------------------------------------
norm ... returns a (kind of) distance between points (x, y) and (x'y')
-------------------------------------------------------------------------- *)
let norm x y x' y' =
	(absdiff x x') + (absdiff y y')

(* --------------------------------------------------------------------------
move_too_near ... true, if point (x, y) too close to point (x'y')
valid_initial_state_size ... true, if coordinates large enough for a state
-------------------------------------------------------------------------- *)
let move_too_near x y x' y' =
	(absdiff x x') + (absdiff y y') < 50

let min_size_for_state =
	function
	| AndRefine(horvert, andl) ->
			if horvert = 0
			then (50, max 10 (List.fold_left max 0 andl))
			else (max 50 (List.fold_left max 0 andl), 10)
	| Graphic _ ->
			(50, 30)
	| Textual _ ->
			(50, 30)

let valid_state_size x y x' y' separ refine =
	let (xmin, ymin) = min_size_for_state refine in
	x + xmin <= x' && y + ymin + separ < y'

(* --------------------------------------------------------------------------
valid x y ... the coordinate pair (x, y) is valid for the canvas - size
-------------------------------------------------------------------------- *)
let valid x y =
	(x >= 0) && (x <= se.ge_width) && (y >= 0) && (y <= se.ge_height)

let valid_coord =
	function
	| State(s) ->
			(valid s.st_x s.st_y) && (valid s.st_x' s.st_y') &
			(valid_state_size s.st_x s.st_y s.st_x' s.st_y' s.st_separ s.st_refine)
	| Trans(t) ->
			true
	| Init(i) ->
			let x = i.ie_x
			and y = i.ie_y
			and r = i.ie_r in
			(valid (x - r) (y - r)) && (valid (x + r) (y + r))
	| Exit(i) ->
			let x = i.ie_x
			and y = i.ie_y
			and r = i.ie_r in
			(valid (x - r) (y - r)) && (valid (x + r) (y + r))
	| Cond(c) ->
			let x = c.co_x
			and y = c.co_y
			and r = c.co_r in
			(valid (x - r) (y - r)) && (valid (x + r) (y + r))

(* --------------------------------------------------------------------------
go2font go ... returns the font
-------------------------------------------------------------------------- *)
let go2font =
	function
	| State(s) -> s.st_font
	| Trans(t) -> t.tr_font
	| Init(i) -> Err.intern "go2font"
	| Exit(i) -> Err.intern "go2font"
	| Cond(c) -> c.co_font

let go_set_font f =
	function
	| State(s) -> s.st_font <- f
	| Trans(t) -> t.tr_font <- f
	| Init(i) -> Err.intern "go2font"
	| Exit(i) -> Err.intern "go2font"
	| Cond(c) -> c.co_font <- f

(* --------------------------------------------------------------------------
go2parent go ... returns the parent - go id for any go
-------------------------------------------------------------------------- *)
let go2parent =
	function
	| State(s) -> s.st_parent
	| Trans(t) -> t.tr_parent
	| Init(i) -> i.ie_parent
	| Exit(i) -> i.ie_parent
	| Cond(c) -> c.co_parent

(* --------------------------------------------------------------------------
go2id go ... returns the GO - id for any go
-------------------------------------------------------------------------- *)
let go2id =
	function
	| State(s) -> s.st_id
	| Trans(t) -> t.tr_id
	| Init(i) -> i.ie_id
	| Exit(i) -> i.ie_id
	| Cond(c) -> c.co_id

(* --------------------------------------------------------------------------
down - casting graphical objects
-------------------------------------------------------------------------- *)
let go2state =
	function
	| State(st) -> st
	| _ -> Err.intern "go2state [downcast]"

let go2trans =
	function
	| Trans(st) -> st
	| _ -> Err.intern "go2trans [downcast]"

let go2init =
	function
	| Init(st) -> st
	| _ -> Err.intern "go2inex [downcast]"

let go2exit =
	function
	| Exit(st) -> st
	| _ -> Err.intern "go2inex [downcast]"

let go2cond =
	function
	| Cond(st) -> st
	| _ -> Err.intern "go2cond [downcast]"

(* --------------------------------------------------------------------------
first_unused_id canvasl
... first int, which is not used as id for a canvas in ''canvasl''
-------------------------------------------------------------------------- *)
let first_unused_id of_canvasl =
	let rec is_used i =
		function
		| [] -> false
		| (x, _):: l ->
				if x = i
				then true
				else is_used i l
	in
	let i = ref 1 in
	while (is_used !i of_canvasl) do i := !i + 1 done;
	!i

(* --------------------------------------------------------------------------
is_frozen go ... true if the go should never be modified, removed, resized
-------------------------------------------------------------------------- *)
let is_frozen =
	function
	| State(st) -> st.st_frozen
	| _ -> false

let get_frozen st parent stl =
	try List.find
			( fun st' -> st'.st_parent = parent && st'.st_frozen && st'.st_name = st.st_name
			) stl
	with Not_found ->
			Err.intern ("get_frozen "^string_of_int st.st_id)

(* --------------------------------------------------------------------------
mk_connect x y st ... computes the connection point of a transition and
state / init / exit / cond. Returns a tconnect - value
connect2pt c go ... returns the point at which connection ''c'' is
connected to a state / init / exit / cond.
-------------------------------------------------------------------------- *)
let mk_connect x y =
	function
	| State(s) ->
			let a1 = absdiff y s.st_y
			and a2 = absdiff x s.st_x'
			and a3 = absdiff y s.st_y'
			and a4 = absdiff x s.st_x in
			if a1 <= a2 && a1 <= a3 && a1 <= a4 then
				if x > s.st_x + 4 && x < s.st_x'- 4
				then { side = 1; step = x - s.st_x }
				else { side = 1; step = 4 }
			else if a2 <= a1 && a2 <= a3 && a2 <= a4 then
				if y > s.st_y + 4 && y < s.st_y'- 4
				then { side = 2; step = y - s.st_y }
				else { side = 2; step = 4 }
			else if a3 <= a1 && a3 <= a2 && a3 <= a4 then
				if x > s.st_x + 4 && x < s.st_x'- 4
				then { side = 3; step = s.st_x'- x }
				else { side = 3; step = 4 }
			else if a4 <= a1 && a4 <= a2 && a4 <= a3 then
				if y > s.st_y + 4 && y < s.st_y'- 4
				then { side = 4; step = s.st_y'- y }
				else { side = 4; step = 4 }
			else Err.intern "pos_at_st"
	| Init(i) -> { side = 0; step = 0 }   (* easy to connect to small go's ... *)
	| Exit(i) -> { side = 0; step = 0 }   (* easy to connect to small go's ... *)
	| Cond(i) -> { side = 0; step = 0 }   (* easy to connect to small go's ... *)
	| _ -> Err.intern "mk_connect"

let connect2pt c =
	function
	| State(st) ->
			let s = c.side
			and t = c.step in
			(if s = 1 then st.st_x + t else
				if s = 2 then st.st_x' else
				if s = 3 then st.st_x'- t else
				if s = 4 then st.st_x else
					Err.intern "connect2pt"),
			(if s = 1 then st.st_y else
				if s = 2 then st.st_y + t else
				if s = 3 then st.st_y' else
				if s = 4 then st.st_y'- t else
					Err.intern "connect2pt")
	| Init(i) -> (i.ie_x, i.ie_y)
	| Exit(i) -> (i.ie_x, i.ie_y)
	| Cond(c) -> (c.co_x, c.co_y)
	| _ -> Err.intern "connect2pt"

(* --------------------------------------------------------------------------
go2tkids go ... returns the tcl / tk - tkids of go
is_state go ... returns true if go is state
is_trans go ... returns true if go is transition
is_inex go ... returns true if go is an init or an exit ''state''
is_cond go ... returns true if go is an conditional ''state''
-------------------------------------------------------------------------- *)
(* auxiliary function - local to this function block ------------------------ *)

let is_refined st =
	match st.st_refine with
	| Graphic _ -> true
	| Textual (Some _) -> true
	| _ -> false

let state2tkids =
	function
	| None -> []
	| Some(s) ->
			( match s.st_tkref with
				| None -> []
				| Some tkid -> [tkid]
			)
			@ [s.st_tkframe; s.st_tksepar; s.st_tkname; s.st_tkact] @ s.st_tkand

let trans2tkids =
	function
	| None -> []
	| Some(t) -> [t.tr_tkline; t.tr_tktxt]

let inex2tkids =
	function
	| None -> []
	| Some(i) -> [i.ie_tkid]

let cond2tkids =
	function
	| None -> []
	| Some(c) -> [c.co_tkid; c.co_tktxt]

(* -------------------------------------------------------------------------- *)
let go2tkids =
	function
	| State(s) -> state2tkids s.st_tk
	| Trans(t) -> trans2tkids t.tr_tk
	| Init(i) -> inex2tkids i.ie_tk
	| Exit(i) -> inex2tkids i.ie_tk
	| Cond(c) -> cond2tkids c.co_tk

(* -------------------------------------------------------------------------- *)
let is_state =
	function
	| State(_) -> true
	| _ -> false

let is_trans =
	function
	| Trans(_) -> true
	| _ -> false

let is_init =
	function
	| Init(_) -> true
	| _ -> false

let is_exit =
	function
	| Exit(_) -> true
	| _ -> false

let is_cond =
	function
	| Cond(_) -> true
	| _ -> false

let filter_states stl =
	List.fold_left
		( fun stl go ->
					if (is_state go)
					then (go2state go):: stl
					else stl
		) [] stl
(* --------------------------------------------------------------------------
andl_valid st andl ... the AND - list ''andl'' is well - formed wrt state ''st''
-------------------------------------------------------------------------- *)
let andl_valid length andl =
	let rec bs_andl_valid =
		function
		| [] -> Err.intern "bs_andl_valid"
		| [a] ->
				if a < 0 || a + 30 > length
				then false
				else true
		| a1:: a2:: al ->
				if a1 < 0 || a1 + 30 > a2
				then false
				else bs_andl_valid (a2:: al)
	in
	bs_andl_valid andl

(* --------------------------------------------------------------------------
go_inside_area parent x y x' y' go ... true if go lies inside the area
-------------------------------------------------------------------------- *)
let state_in_ra parent x y x' y' st =
	(x < st.st_x) && (y < st.st_y) &&
	(x' > st.st_x') && (y' > st.st_y') &&
	parent = st.st_parent

let rec go_inside_area parent x y x' y' =
	function
	| State(s) ->
			state_in_ra parent x y x' y' s
	| Trans(t) as go ->
			let inside = go_inside_area parent x y x' y' in
			(inside t.tr_from) && (inside t.tr_to) &&
			parent = (go2parent go)
	| Init(i) as go ->
			let xi = i.ie_x
			and yi = i.ie_y
			and ri = i.ie_r in
			(x <= xi - ri) && (y <= yi - ri) &&
			(x' >= xi + ri) && (y' >= yi + ri) &&
			parent = (go2parent go)
	| Exit(i) as go ->
			let xi = i.ie_x
			and yi = i.ie_y
			and ri = i.ie_r in
			(x <= xi - ri) && (y <= yi - ri) &&
			(x' >= xi + ri) && (y' >= yi + ri) &&
			parent = (go2parent go)
	| Cond(c) as go ->
			let xi = c.co_x
			and yi = c.co_y
			and ri = c.co_r in
			(x <= xi - ri) && (y <= yi - ri) &&
			(x' >= xi + ri) && (y' >= yi + ri) &&
			parent = (go2parent go)

let is_inhabited st gol =
	let state_in_ra =
		state_in_ra st.st_parent st.st_x st.st_y st.st_x' st.st_y' in
	let stl = filter_states gol in
	List.exists state_in_ra stl

(* --------------------------------------------------------------------------
at_handle x y go ... if (x, y) is at a handle, then return
1 --- 2 --- 3 for a state
| |
8 4
| |
7 --- 6 --- 5

--------> for a transition
1 2 3

1 /-----\ 2 for an init
| | for an exit
| | for a cond
4 \-----/ 3

... otherwise, if (x, y) is
at an AND of a state, return 10
at a text of a cond or transition 11

... 0 otherwise

at_an_and x y go ... true, if mouse is close to an AND
--> this function is similar to ''update_and''
at_trans_text x y go ... true, if mouse is close to an AND
-------------------------------------------------------------------------- *)
let at_small_handle x y r =       (* LOCAL for init, exit and conditions *)
	let norm = (norm x y) in
	let x' = x - r
	and y' = y - r
	and x'' = x + r
	and y'' = y + r in
	if (norm x' y' ) < 10 then 1 else
	if (norm x'' y' ) < 10 then 2 else
	if (norm x'' y'' ) < 10 then 3 else
	if (norm x' y'' ) < 10 then 4 else
		0

let at_handle x y =
	function
	| State(s) ->
			let norm = (norm x y)
			and x = s.st_x
			and y = s.st_y
			and x' = s.st_x'
			and y' = s.st_y'
			and x'' = (s.st_x + s.st_x') / 2
			and y'' = (s.st_y + s.st_y') / 2 in
			if (norm x y ) < 10 then 1 else
			if (norm x'' y ) < 10 then 2 else
			if (norm x' y ) < 10 then 3 else
			if (norm x' y'') < 10 then 4 else
			if (norm x' y' ) < 10 then 5 else
			if (norm x'' y' ) < 10 then 6 else
			if (norm x y' ) < 10 then 7 else
			if (norm x y'') < 10 then 8 else 0
	| Trans(t) ->
			let norm = (norm x y)
			and (fromx, fromy) = connect2pt t.tr_frco t.tr_from
			and (tox, toy) = connect2pt t.tr_toco t.tr_to in
			let midx = fromx + t.tr_midx
			and midy = fromy + t.tr_midy in
			if (norm fromx fromy) < 10 then 1 else
			if (norm midx midy ) < 10 then 2 else
			if (norm tox toy ) < 10 then 3 else 0
	| Init(i) -> at_small_handle i.ie_x i.ie_y i.ie_r
	| Exit(i) -> at_small_handle i.ie_x i.ie_y i.ie_r
	| Cond(c) -> at_small_handle c.co_x c.co_y c.co_r

let at_an_and x y =
	function
	| State(st) ->
			( match st.st_refine with
					AndRefine(horvert, andl) ->
						if horvert = 0 then (
							x >= st.st_x && x <= st.st_x' &&
							List.exists
								(fun a -> (absdiff a (y - st.st_y - st.st_separ))
											< 10)
								andl
						) else (
							y >= st.st_y && y <= st.st_y' &&
							List.exists
								(fun a -> (absdiff a (x - st.st_x)) < 10)
								andl
						)
				| Graphic _ -> false
				| Textual _ -> false
			)
	| _ -> false

let at_trans_text x y =
	function
	| Trans(t) ->
			let (fx, fy) = connect2pt t.tr_frco t.tr_from in
			let tx = fx + t.tr_midx + t.tr_txtx
			and ty = fy + t.tr_midy + t.tr_txty in
			if (norm x y tx ty) < 10
			then true
			else false
	| _ -> false
