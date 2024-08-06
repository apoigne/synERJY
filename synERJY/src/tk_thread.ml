(* ==========================================================================
handle timer calls from Tk
========================================================================== *)
let timer_fun = ref (fun () -> ())
let timer_set = ref false

let create_timer f =
	timer_fun := f;
	if !timer_set
	then ()
	else (
		 timer_set := true;
		create_timer ()
	)

let timer2ocaml2tk () = try !timer_fun () with _ -> ()

let _ = Callback.register "timer2ocaml2tk" timer2ocaml2tk

(* -------------------------------------------------------------------------
means to execute TK commands from threads, which are not the Tcl / Tk thread
1. any thread may put fun of type () -> () into the tk_cmdq queue by
calling add_tk_cmd
2. the Tcl / Tk thread uses a timer handler, which eventually calls
process_tk_cmds to take these commands and execute them

functions to use are:
1. from threads NOT owning Tcl / Tk (producer threads):
add_tk_cmd (f : unit -> unit); execute f in the thread owning Tcl / Tk
2. from the thread owning Tcl / Tk (consumer thread) use a loop like this:
tk_create_timer process_tk_cmds;
while (not !tk_quit) do
tk_one_event ();
done;
tk_destroy "."
It is assumed, that bindings to quit will do tk_quit := true

PRECONDITIONS:
1. native threads
2. tk_one_event is used, and not tk_mainloop
3. tk_create_timer is called to initialize the timer handler
4. Thread.yield () is called by the timer handler to give threads other than
the Tcl / Tk thread the possibility to operate
5. use non - blocking I / O, Unix.system instead of System.command ... ... ...
------------------------------------------------------------------------- *)
let mut_tkq = Mutex.create ()
let tk_cmdq : (unit -> unit) Queue.t = Queue.create ()

let safe_tk_cmdq f =
	Mutex.lock mut_tkq; let r = f tk_cmdq in Mutex.unlock mut_tkq; r

let add_tk_cmd f = safe_tk_cmdq (Queue.add f)

let process_tk_cmds () =
	for i = 1 to (safe_tk_cmdq Queue.length) do
		safe_tk_cmdq Queue.take ()
	done;
	Thread.yield ()
