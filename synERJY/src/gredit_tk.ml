open Ast
open Ly
open Tk
open Util
open Graphic_util
open Gredit_util

(* --------------------------------------------------------------------------
display_go go ... display a GO on a canvas
display_go_of_cv go ... display a GO on the canvas it belongs to
-------------------------------------------------------------------------- *)
module Display =

struct
  
  let font2str font = let (f, s, w) = font in f ^ (" " ^ (s ^ (" " ^ w)))
  
  let bindings_for_state_fct = ref (fun _ -> ())
  
  (* -------------------------------------------------------------------------- *)
  let cv_of_go =
    function
    | State s -> Context.to_canvas s.st_parent
    | Trans t -> Context.to_canvas t.tr_parent
    | Init i | Exit i -> Context.to_canvas i.ie_parent
    | Cond c -> Context.to_canvas c.co_parent
  
  (* --------------------------------------------------------------------------
  auxiliaries for refining a state in a separate canvas (mk_graphic_refine)
  -------------------------------------------------------------------------- *)
  let freeze_state st =
    match st.st_tk with
    | Some stk ->
        let cv = Context.to_canvas st.st_parent in
          ( match stk.st_tkref with
            | None -> ()
            | Some tkid -> cv#delete tkid
          );
          let tkid = cv#create_rounded_polygon st.st_x st.st_y st.st_x' st.st_y' in
            stk.st_tkref <- Some tkid;
            st.st_actions <- "";
            cv#delete stk.st_tkact;
            let font =
              ( match st.st_font with
                | None -> Font.ge_font
                | Some f -> font2str f
              )
            in
            let separ = Font.height_of_text font st.st_name in
              cv#delete stk.st_tksepar;
              stk.st_tksepar <-
              cv#create_line
                [| st.st_x; st.st_y + separ; st.st_x'; st.st_y + separ
                |] [];
              ( match st.st_refine with
                | Graphic _ -> cv#item_configure tkid [ Option.Fill "gray95" ]
                | Textual _ -> cv#item_configure tkid [ Option.Fill "LightYellow" ]
                | _ -> ()
              );
              cv#lower_all tkid
    | None -> ()
  
  let unfreeze_state st p_st =
    match st.st_tk with
    | Some stk ->
        ( match stk.st_tkref with
          | None -> Info.err "unfreeze_state:1"
          | Some tkid ->
              let cv = Context.to_canvas st.st_parent in
                st.st_actions <- p_st.st_actions;
                cv#delete tkid;
                stk.st_tkref <- None;
                let font =
                  ( match st.st_font with
                    | None -> Font.ge_font
                    | Some f -> font2str f
                  )
                in
                  st.st_actions <- (
                    cv#create_text (st.st_x' - 5) (st.st_y + 5)
                      [ Option.Font font; Option.Anchor "ne"; Option.Text st.st_actions ]);
                  let separ =
                    max (Font.height_of_text font st.st_name)
                      (Font.height_of_text font st.st_actions) in
                    cv#delete stk.st_tksepar;
                    stk.st_tksepar <- (
                      cv#create_line
                        [| st.st_x; st.st_y + separ; st.st_x';
                        st.st_y + separ
                        |] [])
        )
    | None -> Info.err "unfreeze_state:2"
  
  let display_and cv kind x y x' y' incr =
    let x = if kind = 0 then x else x + incr
    and y = if kind = 0 then y + incr else y
    and x' = if kind = 0 then x' else x + incr
    and y' = if kind = 0 then y + incr else y' in
      cv#create_line [| x; y; x'; y' |] [ Option.Fill "black" ]
  
  let and_refine =
    function
    | AndRefine (_, _) -> true
    | _ -> false
  
  let and_refine_l =
    function
    | AndRefine (_, l) -> l
    | _ -> []
  
  let and_refine_kind =
    function
    | AndRefine (k, _) -> k
    | _ -> (- 1)
  
  let display_state (offx, offy) st =
    let cv = Context.to_canvas st.st_parent in
      st.st_x <- st.st_x + offx;
      st.st_x' <- st.st_x' + offx;
      st.st_y <- st.st_y + offy;
      st.st_y' <- st.st_y' + offy;
      let font = match st.st_font with | None -> Font.ge_font | Some f -> font2str f in
        ( match st.st_tk with
          | None ->
              let separ =
                match st.st_refine with
                | Textual _
                | Graphic _ -> Font.height_of_text font st.st_name
                | _ -> max (Font.height_of_text font st.st_name) (Font.height_of_text font st.st_actions) in
              let tkr =
                cv#create_rounded_rectangle st.st_x st.st_y st.st_x' st.st_y'
              and tk1 =
                cv#create_text (st.st_x + 5) (st.st_y + 5) [ Option.Font font; Option.Anchor "nw"; Option.Text st.st_name ]
              and tk2 =
                match st.st_refine with
                | Textual _ ->
                    cv#create_text (st.st_x + 5) (st.st_y + separ + 5) [ Option.Font font; Option.Anchor "nw"; Option.Text st.st_actions ]
                | Graphic _ ->
                    ""
                | _ ->
                    cv#create_text (st.st_x'- 5) (st.st_y + 5) [ Option.Font font; Option.Anchor "ne"; Option.Text st.st_actions ]
              and tks =
                cv#create_line [| st.st_x; st.st_y + separ; st.st_x'; st.st_y + separ |] []
              and tka =
                if and_refine st.st_refine
                then
                  List.map
                    ( display_and cv (and_refine_kind st.st_refine) st.st_x (st.st_y + separ) st.st_x' st.st_y'
                    )	(and_refine_l st.st_refine)
                else []
              and tkref =
                ( match st.st_refine with
                  | Textual _ ->
                      let tkid = cv#create_rounded_polygon st.st_x st.st_y st.st_x' st.st_y' in
                        cv#item_configure tkid [ Option.Fill "LightYellow" ];
                        cv#lower_all tkid;
                        Some tkid
                  | _ -> None
                );
              in
                st.st_tk <- (
                  Some
                  { st_tkframe = tkr;
                    st_tksepar = tks;
                    st_tkname = tk1;
                    st_tkact = tk2;
                    st_tkand = tka;
                    st_tkref = tkref;
                  };
                );
                if not (st.st_frozen || is_refined st)
                then !bindings_for_state_fct st
                else Context.make st;
          | Some stk ->
              cv#coords stk.st_tkframe (cv#coord_rounded_rectangle st.st_x st.st_y st.st_x' st.st_y');
              cv#coords stk.st_tkname [| st.st_x + 5; st.st_y + 5 |];
              cv#item_configure stk.st_tkname [ Option.Text st.st_name; Option.Font font ];
              ( match st.st_refine with
                | Textual None ->
                    st.st_separ <- Font.height_of_text font st.st_name;
                    cv#coords stk.st_tkact [| st.st_x + 5; (st.st_y + st.st_separ) + 5 |];
                    cv#item_configure stk.st_tkact [Option.Text st.st_actions; Option.Anchor "nw"; Option.Font font];
                    ( match stk.st_tkref with
                      | None ->
                          let tkid = cv#create_rounded_polygon st.st_x st.st_y st.st_x' st.st_y' in
                            stk.st_tkref <- Some tkid;
                            cv#item_configure tkid [ Option.Fill "LightYellow" ];
                            cv#lower_all tkid
                      | Some tkid ->
                          cv#coords tkid (cv#coord_rounded_rectangle st.st_x st.st_y st.st_x' st.st_y')
                    )
                | Textual (Some _) ->
                    st.st_separ <- Font.height_of_text font st.st_name;
                    cv#delete stk.st_tkact
                | Graphic _ ->
                    st.st_separ <- Font.height_of_text font st.st_name;
                    cv#delete stk.st_tkact;
                    ( match stk.st_tkref with
                      | None -> ()
                      | Some tkid ->
                          cv#delete tkid;
                          stk.st_tkref <- None
                    )
                | _ ->
                    st.st_separ <- max (Font.height_of_text font st.st_name) (Font.height_of_text font st.st_actions);
                    cv#coords stk.st_tkact [| st.st_x' - 5; st.st_y + 5 |];
                    cv#item_configure stk.st_tkact [ Option.Text st.st_actions; Option.Anchor "ne"; Option.Font font ];
                    ( match stk.st_tkref with
                      | None -> ()
                      | Some tkid ->
                          cv#delete tkid;
                          stk.st_tkref <- None
                    )
              );
              cv#coords stk.st_tksepar
                [| st.st_x; st.st_y + st.st_separ; st.st_x'; st.st_y + st.st_separ |];
              List.iter cv#delete stk.st_tkand;
              if and_refine st.st_refine
              then
                stk.st_tkand <- (
                  List.map
                    (display_and cv (and_refine_kind st.st_refine) st.st_x
                        (st.st_y + st.st_separ) st.st_x' st.st_y')
                    (and_refine_l st.st_refine)
                )
        );
        if is_refined st then freeze_state st else ()
  
  (* -------------------------------------------------------------------------- *)
  let display_trans (offx, offy) t =
    let cv = Context.to_canvas t.tr_parent in
    let font = match t.tr_font with | None -> Font.ge_font | Some f -> font2str f in
    let (fromx, fromy) = connect2pt t.tr_frco t.tr_from
    and (tox, toy) = connect2pt t.tr_toco t.tr_to in
    let midx = fromx + t.tr_midx and midy = fromy + t.tr_midy in
    let txtx = midx + t.tr_txtx
    and txty = midy + t.tr_txty in
      match t.tr_tk with
      | None ->
          let tkid =
            cv#create_line [| fromx; fromy; midx; midy; tox; toy |] [ Option.Smooth; Option.Arrow "last" ]
          and tktx =
            cv#create_text txtx txty [ Option.Font font; Option.Anchor "w"; Option.Text t.tr_txt ]
          in t.tr_tk <- Some { tr_tkline = tkid; tr_tktxt = tktx; }
      | Some trtk ->
          cv#coords trtk.tr_tkline [| fromx; fromy; midx; midy; tox; toy |];
          cv#coords trtk.tr_tktxt [| txtx; txty |];
          cv#item_configure trtk.tr_tktxt [ Option.Text t.tr_txt; Option.Font font ]
  
  (* -------------------------------------------------------------------------- *)
  let display_init (offx, offy) i =
    let cv = Context.to_canvas i.ie_parent in
      i.ie_x <- i.ie_x + offx;
      i.ie_y <- i.ie_y + offy;
      let x = i.ie_x
      and y = i.ie_y
      and r = i.ie_r in
        match i.ie_tk with
        | None ->
            let tkid = cv#create_oval (x - r) (y - r) (x + r) (y + r) [ Option.Fill "green" ]
            in i.ie_tk <- Some { ie_tkid = tkid; }
        | Some itk ->
            cv#coords itk.ie_tkid [| x - r; y - r; x + r; y + r |]
  
  (* -------------------------------------------------------------------------- *)
  let display_exit (offx, offy) i =
    let cv = Context.to_canvas i.ie_parent in
      i.ie_x <- i.ie_x + offx;
      i.ie_y <- i.ie_y + offy;
      let x = i.ie_x
      and y = i.ie_y
      and r = i.ie_r in
        match i.ie_tk with
        | None ->
            let tkid =
              cv#create_oval (x - r) (y - r) (x + r) (y + r) [ Option.Fill "red" ]
            in i.ie_tk <- Some { ie_tkid = tkid; }
        | Some itk ->
            cv#coords itk.ie_tkid [| x - r; y - r; x + r; y + r |]
  
  (* -------------------------------------------------------------------------- *)
  let display_cond (offx, offy) c =
    let cv = Context.to_canvas c.co_parent in
      c.co_x <- c.co_x + offx;
      c.co_y <- c.co_y + offy;
      let x = c.co_x and y = c.co_y and r = c.co_r in
      let font =
        match c.co_font with
        | None -> Font.ge_font
        | Some f -> font2str f
      in
        match c.co_tk with
        | None ->
            let tkid = cv#create_oval (x - r) (y - r) (x + r) (y + r) [ Option.Fill "yellow" ]
            and tktx =
              cv#create_text (x + r) (y - r) [ Option.Font font; Option.Anchor "w"; Option.Text c.co_txt ] in
              c.co_tk <- Some { co_tkid = tkid; co_tktxt = tktx; }
        | Some ctk ->
            cv#coords ctk.co_tkid [| x - r; y - r; x + r; y + r |];
            cv#coords ctk.co_tktxt [| x + r; y - r |];
            cv#item_configure ctk.co_tktxt [ Option.Text c.co_txt; Option.Font font ]
  
  (* -------------------------------------------------------------------------- *)
  let display_go ?(offset = (0, 0)) =
    function
    | State s -> display_state offset s
    | Trans t -> display_trans offset t
    | Init i -> display_init offset i
    | Exit i -> display_exit offset i
    | Cond c -> display_cond offset c
  
  (* -------------------------------------------------------------------------- *)
  let display_go_of_cv go =
    if (go2parent go) = (Context.actual ()) then display_go go else ()
  
  (* -------------------------------------------------------------------------
  redisplay the GO [screen damaged??]
  print debug information about all GOs if requested
  ------------------------------------------------------------------------- *)
  let undo_tkmapping =
    function
    | State s -> s.st_tk <- None
    | Trans t -> t.tr_tk <- None
    | Init i | Exit i -> i.ie_tk <- None
    | Cond c -> c.co_tk <- None
  
  let redisplay ?(canvas = None) () =
    let gos = GO.get () in
      if gos <> []
      then (
        Selection.reset ();
        let redisplayed_go = List.filter valid_coord gos in
          GO.set redisplayed_go;
          (* destroy all objects *)
          List.iter (fun cv -> cv#delete "all") (Context.all_canvas());
          (* compute the refined states that do not have corresponding tab yet *)
          (* create the missing tabs *)
          List.iter
            ( function
              | State st -> Context.make st
              | _ -> ()
            ) redisplayed_go;
          (* compute the context that need to be removed *)
          let _, to_remove =
            List.partition
              ( fun c ->
                    List.exists
                      ( function
                        | State st -> st.st_parent = c
                        | _ -> false
                      ) redisplayed_go
              ) (Context.all_contexts())
          in
            List.iter Context.remove to_remove;
            (* only contexts remain that host a state *)
            List.iter undo_tkmapping gos;
            let gos =
              match canvas with
              | Some cv ->
                  List.filter (fun go -> go2parent go = cv) gos
              | None ->
                  gos
            in
              List.iter display_go gos;
              Context.status#has_changed ();
              Info.ps (
                  if List.length (GO.get ()) = List.length redisplayed_go
                  then "Redisplay successful"
                  else "Redisplay problem - some graphic lost"
                )
      )
  
end

module Graphics =

struct
  
  (* let mk_tkhl_txt txt tag dbg canvas tkid =
  txt#tag_add tkid tag
  (i2s (dbg.originline + 1)^"."^i2s (dbg.origincol - 1))
  (i2s (dbg.cornerline + 1)^"."^i2s dbg.cornercol)
  
  let highlight_txt txt tag dbg = function
  | State(s) ->
  ( match s.st_tk with
  | None -> ( )
  | Some(stk) -> mk_tkhl_txt txt tag dbg s.st_parent stk.st_tkact
  )
  | Trans(t) ->
  ( match t.tr_tk with
  | None -> ( )
  | Some(ttk) -> mk_tkhl_txt txt tag dbg t.tr_parent ttk.tr_tktxt
  )
  | Init _
  | Exit _ -> Err.intern "highlight_txt"
  | Cond(c) ->
  ( match c.co_tk with
  | None -> ( )
  | Some(ctk) -> mk_tkhl_txt txt tag dbg c.co_parent ctk.co_tktxt
  )
  *)
  (* --------------------------------------------------------------------------
  mk_sel x y x' y' stop_l ... determine go's selected by the rectangle
  -------------------------------------------------------------------------- *)
  let mk_sel x y x' y' stop_l =
    if move_too_near x y x' y'
    then
      if (Context.cv ())#find_overlapping (x' - 10) (y' - 10) (x' + 10) (y' + 10) = ""
      then []
      else
        try let tkid = (Context.cv ())#find_closest x y in [GO.tkid2go tkid]
        with Not_found -> []
    else
      let inside = go_inside_area (Context.actual ()) x y x' y' in
        List.filter
          ( fun go -> inside go && not (List.mem go stop_l)
          ) (GO.get ())
  
  (* --------------------------------------------------------------------------
  move dx dy go ... moves a go by adding (dx, dy) to its coordinates
  -------------------------------------------------------------------------- *)
  let move dx dy =
    function
    | State s ->
        if not s.st_frozen
        then
          let x = s.st_x + dx
          and y = s.st_y + dy
          and x' = s.st_x' + dx
          and y' = s.st_y' + dy in
            if (valid x y) && (valid x' y')
            then (s.st_x <- x; s.st_y <- y; s.st_x' <- x'; s.st_y' <- y')
            else Error.raise "invalid move"
        else ()
    | Trans t -> ()
    | Init i | Exit i ->
        let x = i.ie_x + dx
        and y = i.ie_y + dy
        and r = i.ie_r in
          if valid (x - r) (y - r) && valid (x + r) (y + r)
          then (i.ie_x <- x; i.ie_y <- y)
          else Error.raise "invalid move"
    | Cond c ->
        let x = c.co_x + dx
        and y = c.co_y + dy
        and r = c.co_r in
          if valid (x - r) (y - r) && valid (x + r) (y + r)
          then (c.co_x <- x; c.co_y <- y)
          else Error.raise "invalid move"
  
  (* --------------------------------------------------------------------------
  resize kind x y dx dy go ... resizes ''go'' relative to its kind
  -------------------------------------------------------------------------- *)
  let valid_connector c sx sy =
    if (c.side = 1) || (c.side = 3)
    then c.step < sx
    else if (c.side = 2) || (c.side = 4)
    then c.step < sy
    else Error.raise "valid_connector"
  
  let valid_trans go sx sy =
    function
    | Trans t ->
        (( != ) t.tr_from go || valid_connector t.tr_frco sx sy) &&
        (( != ) t.tr_to go || valid_connector t.tr_toco sx sy)
    | _ -> true
  
  (* --------------------------------------------------------------------------
  dependent_trans gol ... returns the transitions's dependent on ''go_l''
  that are not already member of ''go_l''
  -------------------------------------------------------------------------- *)
  let dependent_trans gol =
    let dependent_gol = ref ([] : tgraphic list) in     (* local to dependent_trans *)
    let append_dependent go =
      if (List.mem go !dependent_gol) || (List.mem go gol)
      then ( )
      else ( dependent_gol := go :: !dependent_gol ) in
    let process_go =
      function
      | Trans(t) as go ->
          if (List.mem t.tr_from gol) || (List.mem t.tr_to gol)
          then ( append_dependent go )
          else ( )
      | _ -> ( ) in
      dependent_gol := [];
      List.iter process_go (GO.get());
      !dependent_gol
  
  let resize_radius x y =
    let x' = float x
    and y' = float y in
      truncate (sqrt ((x' *. x') +. (y' *. y')))
  
  let resize kind x y dx dy go =
    match go with
    | State r ->
        let x = r.st_x + (if kind = 1 || kind = 7 || kind = 8 then dx else 0)
        and y = r.st_y + (if kind = 1 || kind = 2 || kind = 3 then dy else 0)
        and x' = r.st_x' + (if kind = 3 || kind = 4 || kind = 5 then dx else 0)
        and y' = r.st_y' + (if kind = 5 || kind = 6 || kind = 7 then dy else 0) in
        let sx = x' - x (* x-size of state *)
        and sy = y' - y (* y-size of state *) in
        (* move too small, or AND outside state, or TRANS outside state *)
          if (valid_state_size x y x' y' r.st_separ r.st_refine &&
            List.for_all (valid_trans go sx sy) (GO.get ()) &&
            not r.st_frozen && valid x y && valid x' y')
          then (r.st_x <- x; r.st_y <- y; r.st_x' <- x'; r.st_y' <- y')
          else Error.raise "resize of a state failed"
    | Trans t ->
        if kind = 2
        then
          let x' = t.tr_midx + dx
          and y' = t.tr_midy + dy
          and t' = t.tr_txtx + (if dx > 0 then (- 10) else 10) in
            (t.tr_midx <- x'; t.tr_midy <- y'; t.tr_txtx <- t')
        else
          ( try
              let tkid = (Context.cv ())#find_closest ~halo: "10" ~start: "handle" x y in
              let go = GO.tkid2go tkid in
                if not (is_state go || is_init go || is_exit go || is_cond go)
                then Error.raise "resize";
                if kind = 1
                then (t.tr_frco <- mk_connect x y go; t.tr_from <- go)
                else if kind = 3
                then (t.tr_toco <- mk_connect x y go; t.tr_to <- go)
                else Error.raise "resize"
          with | Not_found ->
              Error.raise "resize"
          )
    | Init i ->
        let r' = resize_radius (absdiff i.ie_x x) (absdiff i.ie_y y) in
          if valid (i.ie_x - r') (i.ie_y - r') && valid (i.ie_x + r') (i.ie_y + r')
          then i.ie_r <- r'
          else Error.raise "resize of a init failed"
    | Exit i ->
        let r' = resize_radius (absdiff i.ie_x x) (absdiff i.ie_y y) in
          if valid (i.ie_x - r') (i.ie_y - r') && valid (i.ie_x + r') (i.ie_y + r')
          then i.ie_r <- r'
          else Error.raise "resize of a exit failed"
    | Cond c ->
        let r' = resize_radius (absdiff c.co_x x) (absdiff c.co_y y) in
          if valid (c.co_x - r') (c.co_y - r') && valid (c.co_x - r') (c.co_y + r')
          then c.co_r <- r'
          else Error.raise "resize of a cond failed"
  
  (* --------------------------------------------------------------------------
  mk_destroy canvas ... callback, when the window ''canvas'' is destroyed
  -------------------------------------------------------------------------- *)
  
  (* let rec remove_from_all_canvas wdwid =
  function
  | [] -> []
  | (((i, _) as cvd)) :: l ->
  if wdwid = i
  then remove_from_all_canvas wdwid l
  else cvd :: (remove_from_all_canvas wdwid l *)
  
  (* --------------------------------------------------------------------------
  remove go ... remove the go from ''all_go'' and widget
  -------------------------------------------------------------------------- *)
  let rec definitive_remove go =
    UndoStack.push();
    GO.remove go;
    try
        let tkidl = go2tkids go
        and cv = Context.to_canvas (go2parent go) in
          List.iter cv#delete tkidl
    with _ -> Info.err "Internal Error: definitive_remove"
  
  let remove go =
    match go with
    | State st ->
        ( match st.st_refine with
          | Graphic c ->
              let p_st = Context.to_state c in
                if is_inhabited p_st (GO.get ())
                then
                  let yes =
                    Dialog.messageBox ~default: "no" "yesno" "question"
                      ( "Do you really want to undo the refinement.\n \
                        All existing sub - states will be eliminated" ) in
                    if yes = "yes"
                    then (Context.remove c; definitive_remove go)
                    else (Context.remove c; definitive_remove go)
          | _ ->
              definitive_remove go
        )
    | _ -> definitive_remove go
  
  (* --------------------------------------------------------------------------
  ... make a template for a GO:
  - mk_trans
  - mk_inex
  - mk_cond
  - mk_state
  -------------------------------------------------------------------------- *)
  let mk_trans_text fromGo =
    if is_init fromGo
    then "{ mthd(); }"
    else if is_cond fromGo
    then "when (true) { mthd(); }"
    else "1: when (expr)\n   { mthd(); }"
  
  let ensure_coord go =
    if valid_coord go then () else raise (Failure "invalid coordinates")
  
  let mk_trans fromGo fromC toGo toC text =
    let (fromx, fromy) = connect2pt fromC fromGo
    
    and (tox, toy) = connect2pt toC toGo in
    let tr =
      { tr_id = Id.unique ();
        tr_from = fromGo;
        tr_frco = fromC;
        tr_to = toGo;
        tr_toco = toC;
        tr_parent = Context.actual ();
        tr_txt = text;
        tr_font = None;
        tr_midx = (tox - fromx) / 2;
        tr_midy = (toy - fromy) / 2;
        tr_txtx = 20;
        tr_txty = 10;
        tr_tk = None;
      }
    in (ensure_coord (Trans tr); tr)
  
  let mk_init x y =
    let go =
      Init
      { ie_id = Id.unique ();
        ie_x = x;
        ie_y = y;
        ie_r = 8;
        ie_parent = Context.actual ();
        ie_tk = None;
      }
    in (ensure_coord go; go)
  
  let mk_exit x y =
    let go =
      Exit
      { ie_id = Id.unique ();
        ie_x = x;
        ie_y = y;
        ie_r = 8;
        ie_parent = Context.actual ();
        ie_tk = None;
      }
    in (ensure_coord go; go)
  
  let mk_cond x y =
    let c =
      { co_id = Id.unique ();
        co_x = x;
        co_y = y;
        co_r = 12;
        co_parent = Context.actual ();
        co_txt = "(expr)";
        co_font = None;
        co_tk = None;
      }
    in (ensure_coord (Cond c); c)
  
  let mk_state x y x' y' stext atext =
    let id = Id.unique () in
    let st =
      { st_id = id;
        st_name =
          if stext = "sname" then "state" ^ (string_of_int id) else stext;
        st_parent = Context.actual ();
        st_actions =
          if (atext = "") && (not (stext = Constant.life_cycle))
          then if (x' - x) < 200 then "    " else "entry { mthd(); }"
          else atext;
        st_font = None;
        st_locals = "";
        st_andacts = [];
        st_frozen = false;
        st_open = false;
        st_separ = 25;
        st_refine = AndRefine (0, []);
        st_x = x;
        st_y = y;
        st_x' = x';
        st_y' = y';
        st_tk = None;
      }
    and ssep = Font.height_of_text Font.ge_font stext
    and asep = Font.height_of_text Font.ge_font atext
    in (st.st_separ <- max ssep asep; ensure_coord (State st); st)
  
  (* --------------------------------------------------------------------------
  mk_notk go ... set all Tk - ids from ''go'' to None
  mk_cut () ... delete the selection
  copy_sel idfn ... copy the !selected_go inti copied_go using (idfn)
  mk_paste () ... paste gos from ref - cell ''copied_go'' and select them
  -------------------------------------------------------------------------- *)
  let mk_notk =
    function
    | State s -> s.st_tk <- None
    | Init i | Exit i -> i.ie_tk <- None
    | Cond c -> c.co_tk <- None
    | Trans t -> t.tr_tk <- None
  
  let state_with_ands st =
    match st.st_refine with
    | AndRefine (_, al) when ( != ) al [] -> true
    | _ -> false
  
  let mk_clear () =
    let selected = Selection.get () in
      match selected with
      | [ (State st as go) ] when state_with_ands st ->
          st.st_refine <- AndRefine (0, []);
          Selection.reset ();
          Display.display_go go
      | _ ->
          List.iter remove selected;
          List.iter remove (dependent_trans selected);
          Selection.reset ();
          Context.status#has_changed ()
  
  let copy_sel ?(copyid = fun _ -> Id.unique ()) () =
    Copy.set (Copy.make ~copyid: copyid (Selection.get ()))
  
  let mk_cut () =
    copy_sel ();
    mk_clear ()
  
  let set_parent_refine =
    function
    | State s ->
        s.st_parent <- Context.actual ();
        s.st_refine <- (
          if Display.and_refine s.st_refine
          then s.st_refine
          else AndRefine (0, []) )
    | Trans t ->
        t.tr_parent <- Context.actual ()
    | Init i
    | Exit i ->
        i.ie_parent <- Context.actual ()
    | Cond c ->
        c.co_parent <- Context.actual ()
  
  let rm_frozen =
    List.filter (function | State s -> not s.st_frozen | _ -> true)
  
  let mk_paste offset () =
    Selection.reset ();
    let copy = rm_frozen (Copy.get ()) in
      List.iter set_parent_refine copy;
      List.iter GO.add copy;
      List.iter (Display.display_go ~offset: offset) copy;
      Context.status#has_changed ();
      Selection.set copy;
      Copy.create_new ()
  
  (* Achtung: problematisch, was passsiert mit frozen states *)
  
  (* --------------------------------------------------------------------------
  update_and x y state ... remove the AND being moved from the and - list
  
  and decorate the display
  --> this function is similar to ''at_an_and''
  -------------------------------------------------------------------------- *)
  let rec (destruct_andl : (int -> bool) -> int list -> (int * (int list))) =
    fun sel ->
        function
        | [] -> Error.raise "destruct_andl"
        | h :: t ->
            if sel h
            then (h, t)
            else
              let (a, al) = destruct_andl sel t in
                (a, (h :: al))
  
  let rec find_and_displacement sel =
    function
    | [] -> Error.raise "find_and_displacement"
    | h :: t -> if sel h then h else find_and_displacement sel t
  
  let and_refine_data =
    function
    | AndRefine (horvert, andl) -> (horvert, andl)
    | _ -> Error.raise "and_refine_data"
  
  let update_and x y go =
    let st = go2state go in
    let (horvert, andl) = and_refine_data st.st_refine in
    let sel =
      if horvert = 0
      then (fun a -> (absdiff a ((y - st.st_y) - st.st_separ)) < 10)
      else (fun a -> (absdiff a (x - st.st_x)) < 10)
    in
    let d = find_and_displacement sel andl in
      Selection.set [ go ];
      ( if horvert = 0
        then
          let d' = (st.st_y + st.st_separ) + d in
            Handle.make st.st_x d'; Handle.make st.st_x' d'
        else
          let d' = st.st_x + d in
            Handle.make d' st.st_y; Handle.make d' st.st_y');
      destruct_andl sel andl
  
  (* --------------------------------------------------------------------------
  call the tool - function with its memory and the actual event
  activate a new canvas if desired
  -------------------------------------------------------------------------- *)
  let event2str =
    function
    | B1press -> Constant.b1press
    | B1rel -> Constant.b1rel
    | ShiftB1press -> Constant.shiftB1press
    | ShiftB1rel -> Constant.shiftB1rel
    | DoubleB1press -> Constant.doubleB1press
    | DoubleB1rel -> Constant.doubleB1rel
    | ShiftDoubleB1press -> Constant.shiftDoubleB1press
    | ShiftDoubleB1rel -> Constant.shiftDoubleB1rel
    | B2press -> Constant.b2press
    | B2rel -> Constant.b2rel
    | B3press -> Constant.b3press
    | B3rel -> Constant.b3rel
    | ActivateTool -> "ActivateTool"
    | Suspend_Tool -> "Suspend_Tool"
  
  let tk_call_tf p event =
    try
        !Tool.tool_ref (int_of_float (float_of_string p.(1)))
          (int_of_float (float_of_string p.(2))) ((!Tool.mem_ref), event)
    with err ->
        let s = Printexc.to_string err in (Error.push s; Info.err s)
  
  let tk_b1_press p = (Info.clear (); tk_call_tf p B1press)
  let tk_b1_rel p = tk_call_tf p B1rel
  let tk_shb1_press p = (Info.clear (); tk_call_tf p ShiftB1press)
  let tk_shb1_rel p = tk_call_tf p ShiftB1rel
  let tk_b2_press p = (Info.clear (); tk_call_tf p B2press)
  let tk_b2_rel p = tk_call_tf p B2rel
  let tk_b3_press p = (Info.clear (); tk_call_tf p B3press)
  let tk_b3_rel p = tk_call_tf p B3rel
  let tk_db1_press p = (Info.clear (); tk_call_tf p DoubleB1press)
  let tk_db1_rel p = tk_call_tf p DoubleB1rel
  let tk_dsb1_press p = (Info.clear (); tk_call_tf p ShiftDoubleB1press)
  let tk_dsb1_rel p = tk_call_tf p ShiftDoubleB1rel
  
  (* --------------------------------------------------------------------------
  mk_graphic_refine () ... refine a state in a separate canvas
  -------------------------------------------------------------------------- *)
  let not_yet_refined st =
    match st.st_refine with
    | AndRefine (_, []) -> true
    | Textual None -> true
    | _ -> false
  
  (* --------------------------------------------------------------------------
  mk_hl widget_with_goid ... high - light GO on / off
  -------------------------------------------------------------------------- *)
  let mk_hlt = ref 0
  
  (* --------------------------------------------------------------------------
  highlight on_off_bool go ...
  -------------------------------------------------------------------------- *)
  let mk_tkhl on colour ol canvas tkid =
    let colour = if on then colour else "black" in
    let outline = if ol then Option.Outline colour else Option.Fill colour in
      (Context.to_canvas canvas)#item_configure tkid [ Option.Width (if on then 3 else 1); outline]
  
  let highlight on =
    function
    | State s ->
        ( match s.st_tk with
          | None -> ()
          | Some stk -> mk_tkhl on "orange" false s.st_parent stk.st_tkframe
        )
    | Trans t ->
        ( match t.tr_tk with
          | None -> ()
          | Some ttk -> mk_tkhl on "blue" false t.tr_parent ttk.tr_tkline
        )
    | Init i | Exit i ->
        ( match i.ie_tk with
          | None -> ()
          | Some itk -> mk_tkhl on "blue" true i.ie_parent itk.ie_tkid
        )
    | Cond c ->
        ( match c.co_tk with
          | None -> ()
          | Some ctk -> mk_tkhl on "blue" true c.co_parent ctk.co_tkid
        )
  
  let set_highlight iid =
    Info.ps (i2s iid);
    try
        if iid < 0 then raise Not_found else ();
        if !mk_hlt = 0
        then (
          highlight true (GO.id2go iid); mk_hlt := iid
        ) else (
          highlight false (GO.id2go !mk_hlt);
          highlight true (GO.id2go iid);
          mk_hlt := iid
        )
    with _ ->
        Info.ps "no object found"
  
  let unset_highlight () =
    if !mk_hlt <> 0 then highlight false (GO.id2go !mk_hlt) else ()
  
  let mk_hl_from_entry entry () =
    let sid = normalize_text entry#get in
    let iid = s2i sid in set_highlight iid
  
end

(* -------------------------------------------------------------------------- *)
module TextEdit =

struct
  
  class ge_text_window (x, y) start_text update_fun =
  
  object (self)
    
    val top =
      new Toplevel.toplevel "Edit"
        ~about: (Some ("About synERJYcharts", Util_tk.Dialog.about se.ge_version))
        ~geometry: ("+" ^ ((i2s x) ^ ("+" ^ (i2s y)))) ".ge.text_dialog"
    val txt = new Tk.text ".ge.text_dialog.t"
    val bot = new Ttk.frame ".ge.text_dialog.bot"
    val close = new Ttk.button ~text: "No" ".ge.text_dialog.bot.c"
    val ok = new Ttk.button ~text: "Yes" ".ge.text_dialog.bot.ok"
    (*  val entry = new Ttk.entry 50 ".ge.text_dialog.e" *)
    val old_entry = Info.get_entry ()
    
    method accept () =
      let edited_text = txt#get () in
      let norm_text = normalize_text edited_text
      in if update_fun norm_text then self#close () else ()
    
    method close () = Info.set_entry old_entry; top#destroy
    
    initializer (
      top#set_cmd self#close;
      (* if Constant.windowingsystem () = Constant.aqua
      then Constant.show_preferences (fun () -> ignore new preference_panel)
      else ();*)
      let m = top#menu in
      let e = new Tk.menu (top#path ^ ".mbar.edit") in
        m#add_cascade "Edit" e;
        e#add_command ~cllbck: txt#undo ~acc: "Z" "Undo";
        top#bind "<<Undo>>" txt#undo;
        e#add_command ~cllbck: txt#redo ~acc: "Y" "Redo";
        top#bind "<<Redo>>" txt#redo;
        e#add_separator;
        e#add_command ~cllbck: txt#cut ~acc: "Y" "Cut";
        top#bind "<<Cut>>" txt#cut;
        e#add_command ~cllbck: txt#copy ~acc: "C" "Copy";
        top#bind "<<Copy>>" txt#copy;
        e#add_command ~cllbck: txt#paste ~acc: "P" "Paste";
        top#bind "<<Paste>>" txt#paste;
        e#add_command ~cllbck: txt#clear ~underl: (- 1) "Clear";
        e#add_separator;
        e#add_command ~cllbck: self#accept ~acc: "S" "Accept";
        top#bind "<<Save>>" self#accept;
        e#add_command ~cllbck: self#close ~acc: "W" "Close";
        top#bind "<<Close>>" self#close;
        let h = new Tk.menu (top#path ^ ".mbar.help") in
          m#add_cascade "Help" h;
          h#add_command ~cllbck: Util_tk.Dialog.help ~acc: "H" "Help";
          top#bind "<<Help>>" Util_tk.Dialog.help;
          txt#configure [ Option.Width 60; Option.Height 8 ];
          bot#configure [ Option.Relief Option.Groove ];
          let f = Constant.file_join [| se.se_home; "images"; "delete.gif" |] in
          let i = Image.create Image.Photo f in
            close#configure [ Option.Image i; Option.Cmd self#close ];
            let f = Constant.file_join [| se.se_home; "images"; "ok.gif" |] in
            let i = Image.create Image.Photo f in
            (* entry#state Constant.ReadOnly;*)
            (* entry#pack [Bottom; Fillx]; *)
            (*  Info.set_entry (Some entry); *)
            (*  top#bind "<FocusIn>" (fun () -> Info.set_entry (Some entry)); *)
              ok#configure [ Option.Image i; Option.Cmd self#accept ];
              ok#pack [ Pack.Top ];
              close#pack [ Pack.Top ];
              bot#pack [ Pack.Left; Pack.Filly ];
              txt#pack [ Pack.Right; Pack.Fillxy; Pack.Expand ];
              txt#insert ~index: "1.0" start_text;
              txt#focus;
              top#grab
    )
    
  end
  
  (* --------------------------------------------------------------------------
  start_text_edit n go ... prepare text edit for ''go'', ''n'' for states
  ... callbacks of accept / cancel - button effect the update
  ... see the functions above
  -------------------------------------------------------------------------- *)
  let upd_frozen_n old_name new_name wdw =
    function
    | State st ->
        if st.st_frozen && ((st.st_name = old_name) && (st.st_parent = wdw))
        then
          let cv =
            try Context.to_canvas wdw
            with Error "Internal Error of Context.to_canvas" ->
                Err.intern "upd_frozen_n"
          in
            cv#set_id new_name;
            st.st_name <- new_name;
            Display.display_go (State st);
            true
        else
          false
    | _ -> false
  
  let upd_frozen_a name wdw separ new_text =
    function
    | State st ->
        if st.st_frozen && ((st.st_name = name) && (st.st_parent = wdw))
        then (
          st.st_actions <- new_text;
          st.st_separ <- separ;
          Display.display_go (State st);
          true
        ) else
          false
    | _ -> false
  
  let upd_state_name st new_name =
    try
        let _ = Lex_wrapper.parse_string Yacc.prs_id new_name in
        (* upd_frozen_n may have a side-effect & returns true then *)
          if new_name <> st.st_name then UndoStack.push ();
          ( match st.st_refine with
            | Graphic c
            | Textual (Some c) ->
                ignore (List.exists (upd_frozen_n st.st_name new_name c) (GO.get ()))
            | _ -> ()
          );
          st.st_name <- new_name;
          Display.display_go (State st);
          Context.status#has_changed();
          Info.ps "state-name accepted";
          true
    with _ ->
        (Info.ps "editing of state-name failed"; false)
  
  let upd_action_text st new_text =
    try
        let font =
          match st.st_font with
          | None -> Font.ge_font
          | Some f -> Display.font2str f in
        let separ =
          max (Font.height_of_text font st.st_name) (Font.height_of_text font new_text)
        in
          if not (valid_state_size st.st_x st.st_y st.st_x' st.st_y' separ st.st_refine)
          then raise (Failure "too_big")
          else if st.st_frozen && (not (st.st_name = Constant.life_cycle))
          then raise (Failure Constant.life_cycle)
          else ();
          let _ = Lex_wrapper.parse_string Yacc.prs_stateaction new_text in
          (* upd_frozen_a may have a side-effect & returns true then *)
            if new_text <> st.st_actions then UndoStack.push () else ();
            ( match st.st_refine with
              | Graphic cntxt ->
                  ignore (
                      List.exists (upd_frozen_a st.st_name cntxt separ new_text) (GO.get ())
                    )
              | _ -> ()
            );
            st.st_separ <- separ;
            st.st_actions <- new_text;
            Display.display_go (State st);
            Info.ps "action text accepted";
            true
    with
    | Failure "too_big" ->
        (Info.ps "size of text exceeds that of the state"; false)
    | Failure "life_cycle" ->
        (Info.ps "State is frozen."; false)
    | _ ->
        (Info.ps "edit of action text invalid - ignored"; false)
  
  let upd_trans_text tr new_text =
    try
        if is_init tr.tr_from
        then ignore (Lex_wrapper.parse_string Yacc.prs_inittrans new_text)
        else
        if is_cond tr.tr_from
        then ignore (Lex_wrapper.parse_string Yacc.prs_condtrans new_text)
        else ignore (Lex_wrapper.parse_string Yacc.prs_trans new_text);
        if new_text <> tr.tr_txt then UndoStack.push () else ();
        tr.tr_txt <- new_text;
        Display.display_go (Trans tr);
        Info.ps "transition text accepted";
        true
    with _ ->
        (Info.ps "edit of transition text invalid - ignored"; false)
  
  let upd_cond_text co new_text =
    try
        ignore (Lex_wrapper.parse_string Yacc.prs_cond new_text);
        if new_text <> co.co_txt then UndoStack.push () else ();
        co.co_txt <- new_text;
        Display.display_go (Cond co);
        Info.ps "condition expression text accepted";
        true
    with _ ->
        (Info.ps "edit of condition expression text invalid - ignored"; false)
  
  let state_name (x, y) st =
    ignore (new ge_text_window (x, y) st.st_name (upd_state_name st))
  
  let state_actions (x, y) st =
    ignore (new ge_text_window (x, y) st.st_actions (upd_action_text st))
  
  let transition (x, y) tr =
    ignore (new ge_text_window (x, y) tr.tr_txt (upd_trans_text tr))
  
  let condition (x, y) c =
    ignore (new ge_text_window (x, y) c.co_txt (upd_cond_text c))
  
end

module Bindings =

struct
  
  (* -------------------------------------------------------------------------- *)
  let for_state st =
    let cv = Context.to_canvas st.st_parent in
      match st.st_tk with
      | None -> Info.err "Internal error: No tk_id of state"
      | Some tk_state ->
          cv#canvas_bind tk_state.st_tkname Constant.doubleB1press
            ( fun _ ->
                  if !Tool.selected_tool = PointTool
                  then (
                    Selection.reset();
                    TextEdit.state_name (st.st_x - 5, st.st_x - 5) st
                  )
            );
          cv#canvas_bind tk_state.st_tkact Constant.doubleB1press
            ( fun _ ->
                  if !Tool.selected_tool = PointTool
                  then (
                    Selection.reset();
                    TextEdit.state_actions (st.st_x - 5, st.st_x - 5) st
                  )
            );
          cv#canvas_bind tk_state.st_tkframe Constant.doubleB1press
            ( fun _ ->
                  if !Tool.selected_tool = PointTool
                  then (
                    Selection.reset ();
                    if is_refined st
                    then StateRefine.show st ()
                    else StateRefine.do_it st ()
                  )
            );
          cv#canvas_bind tk_state.st_tksepar Constant.doubleB1press
            ( fun _ ->
                  if !Tool.selected_tool = PointTool
                  then (
                    Selection.reset ();
                    if is_refined st
                    then StateRefine.show st ()
                    else StateRefine.do_it st ()
                  )
            )
  
end

module Print =

struct
  (* -------------------------------------------------------------------------- *)
  let print_ps fname canvas_id =
    let cv = Context.to_canvas canvas_id
    and fs = if canvas_id = 1 then "" else "." ^ (i2s canvas_id)
    and pf =
      if se.print_format = "a4"
      then [ "-rotate"; "1" ]
      else if se.print_format = "a5"
      then [ "-pagewidth"; "16.0c" ]
      else if se.print_format = "a6"
      then [ "-pagewidth"; "10.0c" ]
      else [ "-rotate"; "1" ] in
    let fn = fname ^ (fs ^ ".ps")
    and txt =
      cv#create_text 80 10
        [ Option.Anchor "nw"; Option.Fill "gray95";
        Option.Text (if se.ge_file = ".sec" then "synERJYcharts" else se.ge_file);
        Option.Font "-*-Courier-Bold-R-*-*-60-*-*-*-*-*-*-*" ]
    in
      cv#lower_all txt;
      tL ([ cv#path; "postscript"; "-file"; fn ] @ pf);
      cv#delete txt;
      Info.ps "postscript file generated"
  
  let print_on fname () =
    Selection.reset ();
    List.iter (fun ctxt -> print_ps fname ctxt) (Context.all_contexts ())
  
end

(* =========================================================================
graphic editor control panel
========================================================================= *)
(* --------------------------------------------------------------------------
structure for displaying the state hierarchy in the ge control panel

a browser state defines
- status: open / closed
- its children
- its refinement status
-------------------------------------------------------------------------- *)
module StateBrowser =

struct
  
  class state (st : tstate) =
  
  object (self)
    val mutable state = st
    val mutable is_open = false
    val mutable childs = ([] : state list)
    val mutable parent = None
    
    method origin = st
    method set_state = fun st -> state <- (st : tstate)
    method get_state = state
    method set_open = is_open <- true
    method set_closed = is_open <- false
    method is_open = is_open
    method add_child = fun s -> childs <- s :: childs
    method get_childs = childs
    
  end
  
  (* --------------------------------------------------------------------------
  toplevel_state stl st ...
  true if state - GO ''st'' is not inside a state - GO from ''stl''
  -------------------------------------------------------------------------- *)
  let states = ref []
  
  let remove_state st =
    let rec remove_state =
      function
      | [] -> []
      | x :: stl when x.st_id = st.st_id -> stl
      | st' :: stl -> st' :: (remove_state stl)
    in
      states := remove_state !states
  
  let toplevel_state stl st =
    List.for_all
      ( fun s ->
            s.st_id = st.st_id ||
            not (state_in_ra s.st_parent s.st_x s.st_y s.st_x' s.st_y' st)
      ) stl
  
  let rec gen_or sto parent x y x' y' =
    let state_in_ra = state_in_ra parent x y x' y' in
    let states_in_ra = List.filter state_in_ra !states in
    let toplevel_state = toplevel_state states_in_ra in
    let children = List.filter toplevel_state states_in_ra in
      List.iter
        ( fun st ->
              let child = new state st
              in
                (sto#add_child child;
                  remove_state st;
                  gen_andor child st.st_refine)
        ) children
  
  and gen_and sto kind start =
    let st = sto#get_state in
    let gen_or = gen_or sto st.st_parent in
      function
      | [] ->
          if kind = 0
          then gen_or st.st_x start st.st_x' st.st_y'
          else gen_or start st.st_y st.st_x' st.st_y'
      | s :: sl ->
          if kind = 0
          then (
            gen_or st.st_x start st.st_x' (st.st_y + s);
            gen_and sto kind (st.st_y + s) sl
          ) else (
            gen_or start st.st_y (st.st_x + s) st.st_y';
            gen_and sto kind (st.st_x + s) sl
          )
  
  and gen_andor sto =
    function
    | AndRefine (k, sl) ->
        gen_and sto k sto#get_state.st_x sl
    | Graphic cntxt ->
        let fz = get_frozen sto#get_state cntxt !states
        in (sto#set_state fz; gen_andor sto fz.st_refine)
    | Textual _ -> ()
  
  let old_lco = ref None
  
  let rec update_status (sto : state) (old_sto : state) =
    if sto#origin.st_id = old_sto#origin.st_id
    then (
      if old_sto#is_open then sto#set_open else ();
      List.iter (fun sto -> update_status sto old_sto) sto#get_childs
    ) else (
      List.iter (update_status sto) old_sto#get_childs
    )
  
  let gen_hierarchy lco =
    old_lco := Some lco;
    states := filter_states (GO.get ());
    let lco = new state lco#get_state in
      remove_state lco#get_state;
      gen_andor lco lco#get_state.st_refine;
      ( match !old_lco with
        | None -> ()
        | Some old_lco -> update_status lco old_lco
      );
      lco
  
end

module GraphicEditorWindows =

struct
  
  (* -------------------------------------------------------------------------- *)
  class preference_panel =
  
  object (self)
    val top = new Toplevel.toplevel "Preferences" ".ge.prefs"
    val frame1 = new Util_tk.bordered_frame ~left: 10 ~right: 10 ~top: 10 ~bottom: 1 ".ge.prefs.f1"
    val size_lbl = new Ttk.label ~text: "  Graphics size  " ".ge.prefs.f1.lbl"
    val width_lbl = new Ttk.label ~text: "  Width " ".ge.prefs.f1.width"
    val width_box = new Ttk.entry 4 ".ge.prefs.f1.width_e"
    val height_lbl = new Ttk.label ~text: " Height " ".ge.prefs.f1.height"
    val height_box = new Ttk.entry 4 ".ge.prefs.f1.height_e"
    val print_lbl = new Ttk.label ~text: "  Print size  " ".ge.prefs.f1.plbl"
    val a4 = new Ttk.radio_button ~text: " a4 " ".ge.prefs.f1.a4"
    val a5 = new Ttk.radio_button ~text: " a5 " ".ge.prefs.f1.a5"
    val txt_lbl = new Ttk.label ~text: "  Text font  " ".ge.prefs.f1.txt"
    val txt_size_lbl = new Ttk.label ~text: "  Size  " ".ge.prefs.f1.tsize"
    val txt_size_box = new Ttk.combobox ".ge.prefs.f1.tsize_e"
    val txt_weight_lbl = new Ttk.label ~text: "  Weight  " ".ge.prefs.f1.weight"
    val txt_weight_box = new Ttk.combobox ".ge.prefs.f1.weight_e"
    val ge_lbl = new Ttk.label ~text: "  Graphic font  " ".ge.prefs.f1.ge"
    val ge_size_lbl = new Ttk.label ~text: "  Size  " ".ge.prefs.f1.gsize"
    val ge_size_box = new Ttk.combobox ".ge.prefs.f1.gsize_e"
    val ge_weight_lbl = new Ttk.label ~text: "  Weight  " ".ge.prefs.f1.gweight"
    val ge_weight_box = new Ttk.combobox ".ge.prefs.f1.gweight_e"
    val lbl1 = new Ttk.label ".ge.prefs.f1.lbl1"
    val lbl2 = new Ttk.label ".ge.prefs.f1.lbl2"
    val frame2 = new Util_tk.bordered_frame ~top: 10 ~bottom: 10 ~left: 20 ~right: 20 ".ge.prefs.f2"
    val acceptbutton = new Ttk.button ~text: "Save" ".ge.prefs.f2.ok"
    val abortbutton = new Ttk.button ~text: "Dismiss" ".ge.prefs.f2.no"
    
    val mutable format_a4 = true
    
    method clear () =
      width_box#delete ();
      height_box#delete ();
      txt_size_box#delete ();
      txt_weight_box#delete ();
      ge_size_box#delete ();
      ge_weight_box#delete ()
    
    method set () =
      self#clear ();
      width_box#set (i2s se.ge_width);
      height_box#set (i2s se.ge_height);
      if se.print_format = "a4" then a4#invoke else a5#invoke;
      let (f, size, weight) = se.se_font
      in
        txt_size_box#set size;
        txt_weight_box#set weight;
        let (f, size, weight) = se.ge_font in
          ge_size_box#set size; ge_weight_box#set weight
    
    method apply =
      fun _ ->
          se.ge_width <- s2i width_box#get;
          se.ge_height <- s2i height_box#get;
          se.print_format <- if format_a4 then "a4" else "a5";
          let txt_size = txt_size_box#get and txt_weight = txt_weight_box#get
          and ge_size = ge_size_box#get and ge_weight = ge_weight_box#get in
          let (f, _, _) = se.se_font in
            se.se_font <- (f, txt_size, txt_weight);
            se.ge_font <- (f, ge_size, ge_weight);
            self#set ();
            Font.set Font.se_font txt_size txt_weight;
            Font.set Font.ge_font ge_size ge_weight;
            Context.status#has_changed ()
    
    method save () =
      self#apply [||];
      Util_tk.save_preferences ();
      top#destroy
    
    initializer (
      top#set_cmd (fun () -> top#destroy);
      Wm.not_resizable top#path;
      a4#configure [ Option.Cmd (fun () -> format_a4 <- true) ];
      a5#configure [ Option.Cmd (fun () -> format_a4 <- false) ];
      txt_size_lbl#configure [ Option.Width 5 ];
      txt_size_box#state Constant.ReadOnly;
      txt_size_box#set_cmd self#apply;
      txt_size_box#load_items se.font_sizes;
      txt_weight_lbl#configure [ Option.Width 5 ];
      txt_weight_box#state Constant.ReadOnly;
      txt_weight_box#load_items se.font_weights;
      txt_weight_box#set_cmd self#apply;
      ge_size_box#state Constant.ReadOnly;
      ge_size_box#set_cmd self#apply;
      ge_size_box#load_items se.font_sizes;
      ge_weight_box#state Constant.ReadOnly;
      ge_weight_box#set_cmd self#apply;
      ge_weight_box#set_cmd self#apply;
      frame1#configure [ Option.Relief Option.Flat ];
      frame1#pack [ Pack.Top ];
      Grid.grid
        [ txt_lbl#path; txt_size_lbl#path; txt_size_box#path; "-";
        txt_weight_lbl#path; txt_weight_box#path ]
        [ Grid.Sticky "nsew" ];
      Grid.grid
        [ ge_lbl#path; ge_size_lbl#path; ge_size_box#path; "-";
        ge_weight_lbl#path; ge_weight_box#path ]
        [ Grid.Sticky "nsew" ];
      Grid.grid [ lbl1#path; "-"; "-"; "-" ] [ Grid.Sticky "nsew" ];
      Grid.grid
        [ size_lbl#path; width_lbl#path; width_box#path; "-";
        height_lbl#path; height_box#path ]
        [ Grid.Sticky "nsew" ];
      Grid.grid [ lbl2#path; "-"; "-"; "-" ] [ Grid.Sticky "nsew" ];
      Grid.grid [ print_lbl#path; a4#path; a5#path ] [ Grid.Sticky "nsew" ];
      frame2#pack [ Pack.Bottom; Pack.Fillx ];
      frame2#configure [ Option.Relief Option.Flat ];
      acceptbutton#configure [ Option.Cmd self#save ];
      acceptbutton#pack [ Pack.Right ];
      abortbutton#configure [ Option.Cmd (fun () -> top#destroy) ];
      abortbutton#pack [ Pack.Right ];
      self#set ()
    )
    
  end
  
  (* =========================================================================
  Canvas with scrolls
  ========================================================================= *)
  class state_canvas (state : tstate) path =
  
  object (self)
    
    inherit Ttk.composite_widget path
    
    val canvas = new Tk.canvas (path ^ ".canvas")
    
    method canvas = canvas
    method get_state = state
    
    initializer (
      let canvas_with_scrolls =
        new Util_tk.widget_with_scrolls (canvas :> Tk.scrollable_widget) (path ^ ".sec") in
      let width = if se.ge_width > 913 then 913 else se.ge_width
      and height = if se.ge_height > 675 then 675 else se.ge_height in
        canvas#configure
          [ Option.Bd 0; Option.Highlightthickness 0; Option.Height height; Option.Width width;
          Option.Cursor "top_left_arrow"; Option.Scrollregion (0, 0, se.ge_width, se.ge_height)];
        canvas_with_scrolls#pack [ Pack.Fillxy; Pack.Expand ];
        (* bind button events *)
        let bind = bind canvas#path
        and cnvcrds f p = f [| p.(0); canvas#canvasx p.(1); canvas#canvasy p.(2) |] in
          bind Constant.b1press ~pattern: Constant.wxy (cnvcrds Graphics.tk_b1_press);
          bind Constant.b1rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_b1_rel);
          bind Constant.shiftB1press ~pattern: Constant.wxy (cnvcrds Graphics.tk_shb1_press);
          bind Constant.shiftB1rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_shb1_rel);
          bind Constant.ctrlB3press ~pattern: Constant.wxy (cnvcrds Graphics.tk_b3_press);
          bind Constant.ctrlB3rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_b3_rel);
          bind Constant.b2press ~pattern: Constant.wxy (cnvcrds Graphics.tk_b2_press);
          bind Constant.b2rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_b2_rel);
          bind Constant.b3press ~pattern: Constant.wxy (cnvcrds Graphics.tk_b3_press);
          bind Constant.b3rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_b3_rel);
          bind Constant.doubleB1press ~pattern: Constant.wxy (cnvcrds Graphics.tk_db1_press);
          bind Constant.doubleB1rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_db1_rel);
          bind Constant.shiftDoubleB1press ~pattern: Constant.wxy (cnvcrds Graphics.tk_dsb1_press);
          bind Constant.shiftDoubleB1rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_dsb1_rel);
          if (Constant.windowingsystem ()) = Constant.aqua
          then (
            bind Constant.ctrlB1press ~pattern: Constant.wxy (cnvcrds Graphics.tk_b2_press);
            bind Constant.ctrlB1rel ~pattern: Constant.wxy (cnvcrds Graphics.tk_b2_rel)
          )
    )
    
  end
  
  type tmode = | Edit | View
  
  class ge_control ?(mode = Edit) () =
  
  object (self)
    
    val top =
      new Toplevel.toplevel "synERJYcharts"
        ~about: (Some ("About synERJYcharts", Util_tk.Dialog.about se.ge_version)) ".ge"
    
    val panes = new Ttk.paned_window ".ge.panes"
    val left_pane = new Ttk.frame ".ge.panes.left"
    val states = new Ttk.treeview ".ge.panes.left.states"
    val logo = new Ttk.label ".ge.panes.left.sE"
    val right_pane = new Ttk.frame ".ge.panes.right"
    val right_top = new Ttk.frame ".ge.panes.right.top"
    val canvasses = new Ttk.notebook ".ge.panes.right.canvasses"
    val entry = new Ttk.entry 70 ".ge.panes.right.e"
    val tools = new Ttk.frame ".ge.tools"
    val file_lbl = new Ttk.label ~text: "  File  " ".ge.tools.file"
    val file_box = new Ttk.entry 40 ".ge.tools.file_e"
    
    val mutable state_browser = None
    val mutable cntxt2tab = []
    
    method top = top
    
    (* -------------------------------------------------------------------------- *)
    method reset_tk_and_go = (* close all tabs *)
      ( match state_browser with
        | None -> ()
        | Some lc ->
            let id = i2s lc#get_state.st_id
            in if states#exists id then states#delete [ id ] else ()
      );
      List.iter (fun (st_id, _) -> self#remove_tab st_id) cntxt2tab;
      Context.reset ();
      Selection.set_empty ();
      Copy.reset ();
      UndoStack.reset ();
      state_browser <- None;
      cntxt2tab <- [];
      Info.clear ()
    
    (* -------------------------------------------------------------------------- *)
    method make_tab (st : tstate) =
      let path = canvasses#path ^ ("." ^ (i2s st.st_id)) in
      let tab = new state_canvas st path in
        canvasses#add tab#path [ Option.Sticky "nsew"; Option.Text st.st_name ];
        cntxt2tab <- (st.st_id, tab) :: cntxt2tab;
        Context.add st.st_id st tab#canvas;
        Context.set st.st_id;
        st.st_open <- true;
        canvasses#select tab#path;
        Tool.select PointTool ();
    
    (* -------------------------------------------------------------------------- *)
    method close_tab ?(ctxt = None) () =
      let ctxt =
        match ctxt with
        | None ->
            Context.actual ()
        | Some ctxt ->
            ctxt
      in
        try
            let tab = List.assoc ctxt cntxt2tab in
              canvasses#hide tab#path;
              let st = Context.to_state ctxt in
                st.st_open <- false;
        with Not_found ->
            Info.err ("Tab not found: " ^ (i2s ctxt))
    
    (* -------------------------------------------------------------------------- *)
    method remove_tab ctxt =
      try
          let tab = List.assoc ctxt cntxt2tab in
            cntxt2tab <- List.remove_assoc ctxt cntxt2tab;
            canvasses#forget tab#path;
            tab#destroy
      with Not_found ->
          Info.err ("Tab not found: " ^ (i2s ctxt))
    
    (* -------------------------------------------------------------------------- *)
    method mk_state ?(refined_state = None) () =
      let (state_name, state_actions) =
        match refined_state with
        | None -> (Constant.life_cycle, "")
        | Some st' -> ((st'.st_name), (st'.st_actions)) in
      let st = Graphics.mk_state 5 3 (se.ge_width - 5) (se.ge_height - 3) state_name state_actions in
        st.st_frozen <- true;
        st.st_parent <- st.st_id;
        self#make_tab st;
        Selection.reset ();
        List.iter (Graphics.highlight false) (GO.get ());
        Display.display_go (State st);
        st
    
    (* -------------------------------------------------------------------------- *)
    method close =
      if UndoStack.no_undo_redo ()
      then GO.set []
      else
        let yes =
          Dialog.messageBox ~default: "no" "yesno" "question"
            "Do you really want close the window?\nThere are unsaved changes."
        in
          if yes = "yes" then GO.set [] else ()
    
    (* -------------------------------------------------------------------------- *)
    method mk_new () =
      self#close;
      self#reset_tk_and_go;
      self#create()
    
    (* -------------------------------------------------------------------------- *)
    method create () =
      se.ge_file <- ".sec";
      let lc = self#mk_state () in
        GO.add (State lc);
        state_browser <- Some (new StateBrowser.state lc);
        self#update_statebrowser ()
    
    (* -------------------------------------------------------------------------- *)
    method open_file () =
      self#close;
      let file =
        Dialog.getOpenFile ~ext: ".sec" ~parent: top#path
          ~initfile: se.ge_file
          ~filetypes: "{{SyncCharts} {.sec}} {{All Files}  {*}}"
          "load file"
      in
        if not (Sys.file_exists file)
        then Info.ps "synERJYchart cannot be loaded"
        else (
          let incha = open_in_bin file in
          let version : float = input_value incha in
            if version < 2.0
            then (
              ignore
                (Dialog.messageBox ~default: "ok" "ok" "warning"
                    ("We are sorry that synERJYchart that have been created \
                      with version "
                      ^string_of_float version ^" are not \
                      supported any more. Present version is "
                      ^ se.ge_version ^ "."
                    )
                )
            ) else (
              if (
                if version = float_of_string se.ge_version
                then true
                else
                  let answer =
                    Dialog.messageBox ~default: "ok" "okcancel" "question"
                      ("Do you want to open a synERJYchart that has been created with version "
                        ^string_of_float version ^
                        " of the graphic editor? Present version is "
                        ^se.ge_version ^ "."
                      )
                  in answer = "ok")
              then (
                self#reset_tk_and_go;
                Info.clear ();
                Copy.reset ();
                se.ge_width <- (input_value incha : int);
                se.ge_height <- (input_value incha : int);
                let _ = (input_value incha : string)  (* XXX font info *)
                and file_go : tgraphic list = input_value incha in
                let gol = List.filter valid_coord file_go in
                  GO.set gol;
                  let lcgo =
                    try
                        List.find (
                            function
                            | State st ->
                                st.st_name = Constant.life_cycle
                            | _ ->
                                false
                          ) gol
                    with Not_found ->
                        raise (Failure "synERJYchart not loaded: incorrect format")
                  in
                  let lc =
                    match lcgo with
                    | State st ->
                        st
                    | _ ->
                        raise (Failure "Internal error: go not a state")
                  in
                    state_browser <- Some (new StateBrowser.state lc);
                    Display.redisplay ();
                    self#show_state lc [| |];
                    List.iter
                      ( function
                        | State st -> if st.st_open && st.st_name <> Constant.life_cycle then self#show_state st [| |]
                        | _ -> ()
                      ) gol;
                    se.ge_file <- file;
                    file_box#set se.ge_file;
                    Context.status#has_changed ();
                    Info.ps
                      ( if List.length gol = List.length file_go
                        then "successful load"
                        else "problem during load - some graphic lost"
                      )
              ) else (
                
                Info.ps "synERJYchart not loaded"
              )
            );
            close_in incha
        )
    
    (* -------------------------------------------------------------------------- *)
    method save_base fname () =
      let outcha = open_out_bin fname in
        Selection.reset ();
        let all_go = GO.get () in
          Selection.set all_go;
          output_value outcha (float_of_string se.ge_version : float);
          output_value outcha (se.ge_width : int);
          output_value outcha (se.ge_height : int);
          output_value outcha ("ignored" : string);
          output_value outcha (Copy.make all_go : tgraphic list);
          close_out outcha;
          Selection.reset ();
          Copy.reset ();
          UndoStack.reset ();
          se.ge_file <- fname;
          Info.ps "successful save"
    
    method save () =
      if se.ge_file = ".sec"
      then self#save_as ()
      else Error.guard (self#save_base se.ge_file) ()
    
    method save_as () =
      let file =
        Dialog.getSaveFile ~initfile: se.ge_file ~parent: top#path "save file"
      in
        Error.guard (self#save_base file) ()
    
    (* -------------------------------------------------------------------------- *)
    method print () =
      let file = Dialog.getSaveFile ~ext: ".ps" ~parent: ".ge" "print to file" in
      let len3 = (String.length file) - 3 in
      let file =
        try
            if (String.sub file len3 3) = ".ps"
            then String.sub file 0 len3
            else file
        with _ -> file
      in
        Error.guard (Print.print_on file) ()
    
    (* -------------------------------------------------------------------------- *)
    method undo () =
      UndoStack.undo ();
      Error.guard Display.redisplay ()
    
    method redo () =
      UndoStack.redo ();
      Error.guard Display.redisplay ()
    
    method sver () =
      Error.guard UndoStack.push_version ()
    
    method gver () =
      UndoStack.to_last_version ();
      Error.guard Display.redisplay ()
    
    method cut () =
      Error.guard Graphics.mk_cut ()
    
    method copy () =
      Error.guard Graphics.copy_sel ()
    
    method paste () =
      Error.guard (Graphics.mk_paste (10, 10)) ()
    
    method dupl () =
      Error.guard (Graphics.copy_sel ~copyid: (fun id -> id)) ();
      Error.guard (Graphics.mk_paste (10, 10)) ()
    
    method clear () =
      Error.guard Graphics.mk_clear ()
    
    (* -------------------------------------------------------------------------- *)
    method highlight (sto : StateBrowser.state) yes =
      match sto#origin.st_tk with
      | None -> ()
      | Some tkid ->
          if yes
          then states#tag_configure tkid.st_tkframe [ Option.Fg "red" ]
          else states#tag_configure tkid.st_tkframe [ Option.Fg "blue" ]
    
    (* -------------------------------------------------------------------------- *)
    method store_status (sto : StateBrowser.state) =
      let st = sto#origin in
      let id = i2s st.st_id in
        if states#exists id
        then
          if (states#get_option id "open") = "true"
          then sto#set_open
          else sto#set_closed
        else sto#set_closed;
        List.iter self#store_status sto#get_childs
    
    (* -------------------------------------------------------------------------- *)
    method populate pos (sto : StateBrowser.state) =
      let st = sto#origin in
      let id = i2s st.st_id in
      let tkid = states#insert pos "end" [ Option.Id id; Option.Text st.st_name; Option.Tags [ id ] ] in
        states#tag_bind tkid Constant.b1press Constant.wxy (self#show_state st);
        List.iter (self#populate id) sto#get_childs;
        if sto#is_open
        then states#set_options tkid [ Option.Open true ]
        else states#set_options tkid [ Option.Open false ]
    
    (* -------------------------------------------------------------------------- *)
    method show_state st _ =
      Context.set st.st_parent;
      canvasses#select (canvasses#path ^ ("." ^ (i2s st.st_parent)));
      Info.clear ();
      Graphics.set_highlight st.st_id
    
    (* -------------------------------------------------------------------------- *)
    method quit () =
      if UndoStack.no_undo_redo ()
      then (
        Constant.destroy "."
      ) else (
        let yes =
          Dialog.messageBox ~default: "no" "yesno" "question"
            "Do you really want to quit? \nThere are unsaved changes."
        in
          if yes = "yes" then Constant.destroy "." else ()
      )
    
    (* -------------------------------------------------------------------------- *)
    method refine_state st =
      let refine st =
        UndoStack.push ();
        Selection.reset ();
        let ref_st = self#mk_state ~refined_state: (Some st) () in
        (* !!!!!!  the id of the refined state is equal to the context of the state !!!! *)
        let ctxt = ref_st.st_id in
          ( match st.st_refine with
            | AndRefine _ ->
                st.st_refine <- Graphic ctxt
            | Textual _ ->
                st.st_refine <- Textual (Some ctxt);
                ref_st.st_refine <- Textual None
            | _ -> ()
          );
          Display.freeze_state st;
          Context.set ctxt;
          Display.display_go (State ref_st);
          GO.add (State ref_st)
      in
        if (Graphics.not_yet_refined st) && (not st.st_frozen)
        then
          if is_inhabited st (GO.get ())
          then
            let yes =
              Dialog.messageBox ~default: "no" "yesno" "question"
                ( "Do you really want to do a refinement though\n \
                  there are sub - states of the state to be refined.\n \
                  These may be moved to the refinement using cut and paste." )
            in if yes = "yes" then refine st else ()
          else refine st
        else refine st
    
    (* -------------------------------------------------------------------------- *)
    method undo_refine st =
      let undo_refine c st p_st =
        UndoStack.push ();
        self#remove_tab c;
        st.st_refine <-
        ( match p_st.st_refine with
          | Textual (Some _) -> Textual None
          | _ -> AndRefine (0, [])
        );
        Selection.reset ();
        Display.unfreeze_state st p_st
      in
        Info.clear ();
        match st.st_refine with
        | Textual (Some c)
        | Graphic c ->
            let p_st = Context.to_state c in
              if is_inhabited p_st (GO.get ()) then
                let yes =
                  Dialog.messageBox ~default: "no" "yesno" "question"
                    ( "Do you really want to undo the refinement.\n \
                      All existing sub - states will be eliminated" )
                in if yes = "yes" then undo_refine c st p_st else ()
              else
                undo_refine c st p_st
        | _ ->
            Info.err ("Internal error: state '" ^ (st.st_name ^ "' not found "))
    
    (* -------------------------------------------------------------------------- *)
    method show_refined st =
      match st.st_refine with
      | Graphic c ->
          let stl = filter_states (GO.get ()) in
          let fz = get_frozen st c stl in
            self#show_state fz [| |];
            fz.st_open <- true;
      | _ ->
          Info.err "Internal error: refined state not found";
          assert false
    
    (* -------------------------------------------------------------------------- *)
    method update_statebrowser () =
      (* let selection = Selection.get () in
      List.iter Display.display_go selection;
      if selection = []
      then (
      *)
      match state_browser with
      | None ->
          Info.err "Internal error: state 'life_cycle' not found"
      | Some lc ->
          Error.guard Graphics.unset_highlight ();
          ( try self#store_status lc with | _ -> () );
          ( try states#delete [ i2s lc#get_state.st_id ] with | _ -> ());
          try
              let lc' = StateBrowser.gen_hierarchy lc in
                state_browser <- Some lc';
                self#populate "" lc'
          with _ -> Info.err "Internal error: update_statebrowser"
    
    (* -------------------------------------------------------------------------- *)
    method update_tabnames () =
      List.iter
        ( fun (_, tab) ->
              let st = tab#get_state in
                canvasses#tab (canvasses#path ^ ("." ^ (i2s st.st_parent))) [Option.Text(st.st_name)]
        ) cntxt2tab;
    
    (* -------------------------------------------------------------------------- *)
    initializer (
      top#focusmodel "active";
      top#set_cmd (Error.guard self#quit);
      if (Constant.windowingsystem ()) = Constant.aqua
      then Constant.show_preferences (fun () -> ignore new preference_panel)
      else ();
      let m = top#menu in
      let ff = new Tk.menu ".ge.mbar.file" in
      (*
      let tt = new Tk.menu (".ge.mbar.tool") in
      m#add_cascade "Tool" tt;
      tt#add_command ~cllbck: (Tool.select PointTool)
      ~acc:"1" "Point";
      tt#add_command ~cllbck: (Tool.select StateTool)
      ~acc:"2" "State";
      tt#add_command ~cllbck: (Tool.select TransTool)
      ~acc:"3" "Transition";
      tt#add_command ~cllbck: (Tool.select AndTool)
      ~acc:"4" "And";
      tt#add_command ~cllbck: (Tool.select InitTool)
      ~acc:"5" "Init";
      tt#add_command ~cllbck: (Tool.select CondTool)
      ~acc:"6" "Condition";
      tt#add_command ~cllbck: (Tool.select ExitTool)
      ~acc:"7" "Exit";
      top#bind "<Control-KeyPress-6>" (Tool.select ExitTool);
      top#bind "<Control-KeyPress-1>" (Tool.select PointTool);
      top#bind "<Control-KeyPress-2>" (Tool.select StateTool);
      top#bind "<Control-KeyPress-3>" (Tool.select TransTool);
      top#bind "<Control-KeyPress-4>" (Tool.select AndTool);
      top#bind "<Control-KeyPress-6>" (Tool.select InitTool);
      top#bind "<Control-KeyPress-5>" (Tool.select CondTool);
      *)
        m#add_cascade "File" ff;
        if mode = Edit then (
          ff#add_command ~cllbck: (Error.guard self#mk_new) ~acc: "N" "New";
          ff#add_command ~cllbck: (Error.guard self#open_file) ~acc: "O" "Open ..."
        );
        ff#add_command ~cllbck: (Error.guard self#close_tab) ~acc: "W" "Close";
        if mode = Edit then (
          ff#add_separator;
          ff#add_command ~cllbck: (Error.guard self#save) ~acc: "S" "Save";
          ff#add_command ~cllbck: (Error.guard self#save_as) "Save As ...";
          ff#add_separator;
          ff#add_command ~cllbck: (Error.guard self#print) ~acc: "P" "Print";
          top#bind Constant.print self#print;
          top#bind Constant._new (Error.guard self#mk_new);
          top#bind Constant._open (Error.guard self#open_file);
          top#bind Constant.save (Error.guard self#save);
          if (Constant.windowingsystem ()) = Constant.aqua
          then (
            ff#add_separator
          );
          ff#add_command ~cllbck: self#quit ~acc: "Q" "Quit";
          top#bind Constant.quit self#quit
        );
        if mode = Edit
        then (
          let fe = new Tk.menu ".ge.mbar.edit" in
            m#add_cascade "Edit" fe;
            fe#add_command ~cllbck: self#undo ~acc: "Z" "Undo";
            fe#add_command ~cllbck: self#redo ~acc: "Y" "Redo";
            fe#add_command ~cllbck: self#sver "Set version";
            fe#add_command ~cllbck: self#gver "Reset";
            fe#add_separator;
            fe#add_command ~cllbck: self#cut ~underl: 2 ~acc: "X"
              "Cut";
            fe#add_command ~cllbck: self#copy ~acc: "C" "Copy";
            fe#add_command ~cllbck: self#paste ~acc: "V" "Paste";
            fe#add_command ~cllbck: self#dupl ~acc: "D" "Duplicate";
            fe#add_command ~cllbck: self#clear ~underl: (- 1) "Clear";
            top#bind "<<Undo>>" self#undo;
            top#bind "<<Redo>>" self#redo;
            top#bind "<<Cut>>" self#cut;
            top#bind "<<Copy>>" self#copy;
            top#bind "<<Paste>>" self#paste;
            top#bind "<Control-KeyPress-d>" self#dupl
        );
        if Constant.windowingsystem () <> Constant.aqua
        then (
          let o = new Tk.menu ".ge.mbar.options" in
            m#add_cascade "Option" o;
            o#add_command ~cllbck: (fun () -> ignore new preference_panel) "Preferences";
            let h = new Tk.menu ".ge.mbar.help" in
              m#add_cascade "Help" h;
              h#add_command ~cllbck: (Error.guard (Util_tk.Dialog.about se.ge_version)) "About";
              h#add_command ~cllbck: (Error.guard Util_tk.Dialog.help) ~acc: "H" "Help";
              top#bind "<<Help>>" (Error.guard Util_tk.Dialog.help)
        );
        
        if mode = Edit then (
          tools#configure [ Option.Relief Option.Groove ];
          tools#pack [ Pack.Top; Pack.Fillx ];
          let tool_button tool =
            let name = Tool.tool_wdw tool in
            let f = Constant.file_join [| se.se_home; "images"; "t_" ^ (name ^ ".gif") |] in
            let i = Image.create Image.Photo f in
            let tool_button = new Ttk.button ~cllbck: (Error.guard (Tool.select tool)) (".ge.tools." ^ name) in
              tool_button#configure [ Option.Image i; Option.Style "Toolbutton" ];
              tool_button#pack [ Pack.Left ]
          in
            tool_button PointTool;
            tool_button StateTool;
            tool_button TransTool;
            tool_button AndTool;
            tool_button InitTool;
            tool_button CondTool;
            tool_button ExitTool;
            
            let scale_button mode factor =
              let scale = new Ttk.button (".ge.tools." ^ mode) in
              let f = Constant.file_join [| se.se_home; "images"; mode ^ ".gif" |] in
              let i = Image.create Image.Photo f in
                scale#configure [Option.Image i; Option.Style "Toolbutton"; Option.Cmd (fun _ -> (Context.cv ())#scale_all factor) ];
                scale#pack [ Pack.Bottom ];
                scale
            in
            let grow = scale_button "grow" 1.2
            and shrink = scale_button "shrink" (1.0 /. 1.2) in
              grow#pack [ Pack.Right ];
              shrink#pack [ Pack.Right ]
        );
        panes#pack [ Pack.Top; Pack.Fillxy; Pack.Expand ];
        panes#add left_pane#path;
        left_pane#configure [ Option.Relief Option.Ridge ];
        states#pack [ Pack.Top; Pack.Fillxy; Pack.Expand ];
        states#column "#0" [ Option.Width 200 ];
        states#heading "#0" [ Option.Text "States" ];
        (*logo#pack [Top];*)
        let f = Constant.file_join [| se.se_home; "images"; "synERJYcharts.gif" |] in
        let i = Image.create Image.Photo f in
        (*entry#bind Constant.b1press (entry#delete ~first:"0" ~last:"end");*)
        (*top#bind "<FocusIn>" (fun () -> Info.set_entry (Some entry));*)
          logo#configure [ Option.Image i ];
          panes#add right_pane#path;
          right_pane#configure [ Option.Relief Option.Ridge ];
          
          top#bind Constant.notebookTabChanged
            ( fun _ ->
                  let tktab = canvasses#current in
                    try let (cntxt, _) = List.find (fun (i, t) -> t#path = tktab) cntxt2tab in
                          Context.set cntxt
                    with _ ->
                        ()
            );
          
          canvasses#pack [ Pack.Top; Pack.Fillxy; Pack.Expand ];
          canvasses#configure [ Option.Height 705; Option.Width 936; Option.Padding 0 ];
          entry#bind Constant.return (Graphics.mk_hl_from_entry entry);
          Info.set_entry (Some entry);
          entry#pack [ Pack.Bottom; Pack.Fillx ];
          right_top#pack [ Pack.Bottom; Pack.Fillx ];
          file_lbl#pack [ Pack.Left ];
          file_lbl#configure [ Option.Width 8 ];
          file_box#pack [ Pack.Left; Pack.Fillx; Pack.Expand ];
          file_box#state Constant.ReadOnly;
          file_box#set se.ge_file;
          
          Context.make_fct := (
            fun st ->
                if st.st_frozen && not (Context.exists st.st_id) then (
                  self#make_tab st;
                  if st.st_open && st.st_id = st.st_parent
                  then self#close_tab ~ctxt: (Some st.st_id) ()
                )
          );
          Context.remove_tab_fct := self#remove_tab;
          StateRefine.refine_fct := self#refine_state;
          StateRefine.undo_fct := self#undo_refine;
          StateRefine.show_fct := self#show_refined;
          
          Display.bindings_for_state_fct := Bindings.for_state;
          
          Context.status#add_observer self#update_tabnames;
          Context.status#add_observer self#update_statebrowser;
          
          Error.guard self#create ();
          
          top#raise;
          top#focus
    )
    
  end
  
  class ge_viewer = ge_control ~mode: View
  
end

(* =========================================================================
context menus
========================================================================= *)
module ContextMenu =

struct
  
  let change_font_size seltk n () =
    try
        let go = GO.tkid2go seltk in
        let (f, s, w) =
          match go2font go with
          | None ->
              ((Font.get_family Font.ge_font), (Font.get_size Font.ge_font),
                (Font.get_weight Font.ge_font))
          | Some f -> f in
        let s' = i2s ((s2i s) + n) in
          go_set_font (Some (f, s', w)) go;
          List.iter Display.display_go_of_cv (GO.get ())
    with Not_found ->
        Error.raise "size not changed"
  
  let change_font_weight seltk w' () =
    try
        let go = GO.tkid2go seltk in
        let (f, s, w) =
          match go2font go with
          | None ->
              ((Font.get_family Font.ge_font), (Font.get_size Font.ge_font),
                (Font.get_weight Font.ge_font))
          | Some f -> f
        in
          go_set_font (Some (f, s, w')) go;
          List.iter Display.display_go_of_cv (GO.get ())
    with Not_found ->
        Error.raise "weight not changed"
  
  let font_set_default seltk () =
    let go = GO.tkid2go seltk in
      go_set_font None go; List.iter Display.display_go_of_cv (GO.get ())
  
  (* -------------------------------------------------------------------------- *)
  let for_canvas x y = ()
  
  (* let cv = Context.cv() in
  Constant.destroy ".popmenu";
  let pop = new Tk.popup_menu ".popmenu" in
  pop#configure [Tearoff];
  let rootx = Winfo.rootx cv#path
  
  and rooty = Winfo.rooty cv#path in
  if Copy.exists() then (
  pop#add_command
  ~cllbck: (Error.guard (Graphics.mk_paste (Copy.offset x y)))
  ~underl: (- 1) "Paste";
  pop#add_separator;
  );
  pop#add_command ~cllbck: (Tool.select PointTool)
  ~underl: (- 1) "Point tool";
  pop#add_command ~cllbck: (Tool.select StateTool)
  ~underl: (- 1) "State tool";
  pop#add_command ~cllbck: (Tool.select TransTool)
  ~underl: (- 1) "Transition tool";
  pop#add_command ~cllbck: (Tool.select AndTool)
  ~underl: (- 1) "And tool";
  pop#add_command ~cllbck: (Tool.select InitTool)
  ~underl: (- 1) "Init tool";
  pop#add_command ~cllbck: (Tool.select CondTool)
  ~underl: (- 1) "Condition tool";
  pop#add_command ~cllbck: (Tool.select ExitTool)
  ~underl: (- 1) "Exit tool";
  pop#popup (rootx + x + 1) (rooty + y + 1);
  pop#activate 0
  *)
  (* -------------------------------------------------------------------------- *)
  let for_text seltk x y =
    let cv = Context.cv () in
      Constant.destroy ".popmenu";
      let pop = new Tk.popup_menu ".popmenu" in
        pop#configure [ Option.Tearoff ];
        pop#add_command ~cllbck: (change_font_size seltk 1) ~underl: (- 1) "size +";
        pop#add_command ~cllbck: (change_font_size seltk (- 1)) ~underl: (- 1) "size -";
        pop#add_separator;
        pop#add_command ~cllbck: (change_font_weight seltk "normal") ~underl: (- 1) "normal";
        pop#add_command ~cllbck: (change_font_weight seltk "bold") ~underl: (- 1) "bold";
        pop#add_separator;
        pop#add_command ~cllbck: (font_set_default seltk) ~underl: (- 1) "default font";
        let rootx = Winfo.rootx cv#path
        and rooty = Winfo.rooty cv#path in
          pop#popup ((rootx + x) + 1) ((rooty + y) + 1); pop#activate 0
  
  (* -------------------------------------------------------------------------- *)
  let for_action st seltk x y =
    let cv = Context.cv () in
      Constant.destroy ".popmenu";
      let pop = new Tk.popup_menu ".popmenu" in
        pop#configure [ Option.Tearoff ];
        ( match (st.st_refine, st.st_tk) with
          | (Textual _, Some tk) when tk.st_tkact = seltk ->
              pop#add_command
                ~cllbck: (
                  fun () ->
                      ( match st.st_refine with
                        | Textual None -> st.st_refine <- AndRefine (0, [])
                        | Textual (Some c) -> st.st_refine <- Graphic c
                        | _ -> ()
                      );
                      Display.display_go (State st)
                )
                ~underl: (- 1) "Back to state";
              pop#add_separator
          | (AndRefine (0, []), Some tk)
          | (Graphic _, Some tk) when tk.st_tkact = seltk ->
              pop#add_command
                ~cllbck: (
                  fun () ->
                      (match st.st_refine with
                        | AndRefine (0, []) ->
                            st.st_refine <- Textual None
                        | Graphic c -> st.st_refine <- Textual (Some c)
                        | _ -> ());
                      Display.display_go (State st)
                )
                ~underl: (- 1) "To textual state";
              pop#add_separator
          | _ -> ()
        );
        pop#add_command ~cllbck: (change_font_size seltk 1) ~underl: (- 1) "size +";
        pop#add_command ~cllbck: (change_font_size seltk (- 1)) ~underl: (- 1) "size -";
        pop#add_separator;
        pop#add_command ~cllbck: (change_font_weight seltk "normal") ~underl: (- 1) "normal";
        pop#add_command ~cllbck: (change_font_weight seltk "bold") ~underl: (- 1) "bold";
        pop#add_separator;
        pop#add_command ~cllbck: (font_set_default seltk) ~underl: (- 1) "default font";
        let rootx = Winfo.rootx cv#path
        and rooty = Winfo.rooty cv#path in
          pop#popup ((rootx + x) + 1) ((rooty + y) + 1);
          pop#activate 0
  
  (* -------------------------------------------------------------------------- *)
  let for_state st seltk x y =
    let cv = Context.cv () in
      Constant.destroy ".popmenu";
      let pop = new Tk.popup_menu ".popmenu" in
        pop#configure [ Option.Tearoff ];
        let rootx = Winfo.rootx cv#path
        and rooty = Winfo.rooty cv#path in
          pop#add_command ~cllbck: (Error.guard Graphics.mk_cut) ~underl: (- 1) "Cut";
          pop#add_command ~cllbck: (Error.guard Graphics.copy_sel) ~underl: (- 1) "Copy";
          pop#add_command ~cllbck: (Error.guard Graphics.mk_cut) ~underl: (- 1) "Clear";
          ( match st.st_refine with
            | AndRefine (_, [])
            | Textual None ->
                pop#add_separator;
                pop#add_command ~cllbck: (StateRefine.do_it st) ~underl: (- 1) "Refine state"
            | AndRefine (_, _) ->
                Info.ps "Cannot refine a state with parallel components"
            | _ ->
                pop#add_separator;
                pop#add_command ~cllbck: (StateRefine.show st) ~underl: (- 1) "Show refined state";
                pop#add_command ~cllbck: (StateRefine.undo st) ~underl: (- 1) "Undo refinement"
          );
          pop#popup ((rootx + x) + 1) ((rooty + y) + 1);
          pop#activate 0
  
  (* -------------------------------------------------------------------------- *)
  let for_entity seltk x y =
    let cv = Context.cv () in
      Constant.destroy ".popmenu";
      let pop = new Tk.popup_menu ".popmenu" in
        pop#configure [ Option.Tearoff ];
        let rootx = Winfo.rootx cv#path
        and rooty = Winfo.rooty cv#path in
          pop#add_command ~cllbck: (Error.guard Graphics.mk_cut) ~underl: (- 1) "Clear";
          pop#popup ((rootx + x) + 1) ((rooty + y) + 1);
          pop#activate 0
  
end

(* -------------------------------------------------------------------------
callback - function for executing a command file
- process a command file
IN PUCTRL.ML IS A SIMILAR VERSION OF THIS FUNCTION
------------------------------------------------------------------------- *)
module Cmd =

struct
  let rec process_file cmdfile =
    Lex_wrapper.prc_cmdfile process_cmd
      ( fun sl -> (P.pC (); List.iter P.ps sl; P.pF ())
      ) cmdfile
  and process_cmd =
    function
    | CmdEof -> raise End_of_file
    | CmdPrint s -> (P.pC (); P.ps s; P.pF ())
    | CmdRedisplay -> Display.redisplay ()
    | CmdUnit -> ()
    | _ -> ()
end
