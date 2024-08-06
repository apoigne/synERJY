type tsc_id = Ly.tsc_id

type tsc_data =
	{ mutable inp_counter : int; (* external inputs *)
		mutable ext_counter : int; (* external outputs *)
		mutable lcl_counter : int; (* local signals *)
		mutable var_counter : int; (* variables *)
		mutable aux_counter : int; (* internal valued signals *)
		mutable mem_counter : int; (* internal valued memories *)
		mutable pre_counter : int; (* internal valued memories *)
		mutable jump_counter : int; (* states *)
		mutable reg_counter : int; (* registers *)
		mutable gamma_counter : int; (* gammas *)
		mutable beta_counter : int; (* betas *)
		mutable tau_counter : int; (* taus *)
		mutable frq_counter : int; (* frequenciess *)
		mutable eta_counter : int; (* etas : not in the control path*)
		mutable delta_counter : int; (* internal inputs/data conditions *)
		mutable cond_counter : int; (* condition in conditional expressions *)
		mutable act_counter : int; (* action triggers *)
		mutable dbg_counter : int; (* debug triggers *)
	}

let sc =
	{ inp_counter = 9;
		ext_counter = 1000000;
		lcl_counter = 2000000;
		var_counter = 3000000;
		aux_counter = 4000000;
		mem_counter = 5000000;
		pre_counter = 6000000;
		jump_counter = 7000000;
		reg_counter = 8000000;
		gamma_counter = 9000000;
		beta_counter = 10000000;
		tau_counter = 11000000;
		frq_counter = 12000000;
		eta_counter = 13000000;
		delta_counter = 14000000;
		cond_counter = 15000000;
		act_counter = 16000000;
		dbg_counter = 17000000;
	}

let reset_sgn_counters () =
	sc.inp_counter <- 9;
	sc.ext_counter <- 1000000;
	sc.lcl_counter <- 2000000;
	sc.var_counter <- 3000000;
	sc.aux_counter <- 4000000;
	sc.mem_counter <- 5000000;
	sc.pre_counter <- 6000000;
	sc.jump_counter <- 7000000;
	sc.reg_counter <- 8000000;
	sc.gamma_counter <- 9000000;
	sc.beta_counter <- 10000000;
	sc.tau_counter <- 11000000;
	sc.frq_counter <- 12000000;
	sc.eta_counter <- 13000000;
	sc.delta_counter <- 14000000;
	sc.cond_counter <- 15000000;
	sc.act_counter <- 16000000;
	sc.dbg_counter <- 17000000

let tick = 1
let alpha = 2
let beta = 3
let tau = 4

let tick' = 5
let alpha' = 6
let beta' = 7
let tau' = 8
let mu' = 9

let next_inp () =
	 if sc.inp_counter < 1000000
	then let c = sc.inp_counter + 1 in sc.inp_counter <- c; c
	else raise (Ly.Error "next_inp")
let next_ext () =
	 if sc.ext_counter < 2000000
	then let c = sc.ext_counter + 1 in sc.ext_counter <- c; c
	else raise (Ly.Error "next_ext")
let next_lcl () =
	 if sc.lcl_counter < 3000000
	then let c = sc.lcl_counter + 1 in sc.lcl_counter <- c; c
	else raise (Ly.Error "next_lcl")
let next_var () =
	 if sc.lcl_counter < 4000000
	then let c = sc.var_counter + 1 in sc.var_counter <- c; c
	else raise (Ly.Error "next_var")
let next_aux () = 
	if sc.aux_counter < 5000000
	then let c = sc.aux_counter + 1 in sc.aux_counter <- c; c
	else raise (Ly.Error "next_aux")
let next_mem () =
	 if sc.mem_counter < 6000000
	then let c = sc.mem_counter + 1 in sc.mem_counter <- c; c
	else raise (Ly.Error "next_mem")
let next_pre () =
	 if sc.pre_counter < 7000000
	then let c = sc.pre_counter + 1 in sc.pre_counter <- c; c
	else raise (Ly.Error "next_pre")
let next_jump () = 
	if sc.jump_counter < 8000000
	then let c = sc.jump_counter + 1 in sc.jump_counter <- c; c
	else raise (Ly.Error "next_jump")
let next_reg () =
	 if sc.reg_counter < 9000000
	then let c = sc.reg_counter + 1 in sc.reg_counter <- c; c
	else raise (Ly.Error "next_reg")
let next_gamma () =
	 if sc.gamma_counter < 10000000
	then let c = sc.gamma_counter + 1 in sc.gamma_counter <- c; c
	else raise (Ly.Error "next_gamma")
let next_beta () =
	 if sc.beta_counter < 11000000
	then let c = sc.beta_counter + 1 in sc.beta_counter <- c; c
	else raise (Ly.Error "next_beta")
let next_tau () =
	 if sc.tau_counter < 12000000
	then let c = sc.tau_counter + 1 in sc.tau_counter <- c; c
	else raise (Ly.Error "next_tau")
let next_frq () =
	 if sc.frq_counter < 13000000
	then let c = sc.frq_counter + 1 in sc.frq_counter <- c; c
	else raise (Ly.Error "next_frq")
let next_eta () =
	 if sc.eta_counter < 14000000
	then let c = sc.eta_counter + 1 in sc.eta_counter <- c; c
	else raise (Ly.Error "next_eta")
let next_delta () =
	 if sc.delta_counter < 15000000
	then let c = sc.delta_counter + 1 in sc.delta_counter <- c; c
	else raise (Ly.Error "next_delta")
let next_cond () =
	 if sc.cond_counter < 16000000
	then let c = sc.cond_counter + 1 in sc.cond_counter <- c; c
	else raise (Ly.Error "next_delta")
let next_act () =
	 if sc.act_counter < 17000000
	then let c = sc.act_counter + 1 in sc.act_counter <- c ; c
	else raise (Ly.Error "next_act")
let next_dbg () = 
	if sc.dbg_counter < 18000000
	then let c = sc.dbg_counter + 1 in sc.dbg_counter <- c ; c
	else raise (Ly.Error "next_dbg")

let is_inp x = 0 < x && x < 1000000
let is_ext x = 1000000 < x && x < 2000000
let is_lcl x = 2000000 < x && x < 3000000
let is_var x = 3000000 < x && x < 4000000
let is_aux x = 4000000 < x && x < 5000000
let is_mem x = 5000000 < x && x < 6000000
let is_pre x = 6000000 < x && x < 7000000
let is_jump x = 7000000 < x && x < 8000000
let is_reg x = 8000000 < x && x < 9000000
let is_gamma x = 9000000 < x && x < 10000000
let is_beta x = 10000000 < x && x < 11000000
let is_tau x = 11000000 < x && x < 12000000
let is_frq x = 12000000 < x && x < 13000000
let is_eta x = 13000000 < x && x < 14000000
let is_delta x = 14000000 < x && x < 15000000
let is_cond x = 15000000 < x && x < 16000000
let is_act x = 16000000 < x && x < 17000000
let is_dbg x = 17000000 < x && x < 18000000

let next_sgn x =
	if x = - 1 then - 1 else
	if is_inp x then next_inp() else
	if is_ext x then next_ext() else
	if is_lcl x then next_lcl() else
	if is_var x then next_var() else
	if is_aux x then next_aux() else
	if is_mem x then next_mem() else
	if is_pre x then next_pre() else
	if is_jump x then next_jump() else
	if is_reg x then next_reg() else
	if is_gamma x then next_gamma() else
	if is_beta x then next_beta() else
	if is_tau x then next_tau() else
	if is_frq x then next_frq() else
	if is_eta x then next_eta() else
	if is_delta x then next_delta() else
	if is_cond x then next_cond() else
	if is_act x then next_act() else
	if is_dbg x then next_dbg() else
		raise (Ly.Error "next_sgn")

let sgn2str x =
	if x = 1 then "Tick" else
	if x = 2 then "Alpha" else
	if x = 3 then "Beta" else
	if x = 4 then "Tau" else
	if x = 5 then "tick" else
	if x = 6 then "alpha" else
	if x = 7 then "beta" else
	if x = 8 then "tau" else
	if x = 9 then "mu" else
	if is_inp x then "I"^ (string_of_int(x - 0)) else
	if is_ext x then "S"^ (string_of_int(x - 1000000)) else
	if is_lcl x then "L"^ (string_of_int(x - 2000000)) else
	if is_var x then "V"^ (string_of_int(x - 3000000)) else
	if is_aux x then "Hv"^ (string_of_int(x - 4000000)) else
	if is_mem x then "M"^ (string_of_int(x - 5000000)) else
	if is_pre x then "P"^ (string_of_int(x - 6000000)) else
	if is_jump x then "J"^ (string_of_int(x - 7000000)) else
	if is_reg x then "R"^ (string_of_int(x - 8000000)) else
	if is_gamma x then "G"^ (string_of_int(x - 9000000)) else
	if is_beta x then "B"^ (string_of_int(x - 10000000)) else
	if is_tau x then "T"^ (string_of_int(x - 11000000)) else
	if is_frq x then "F"^ (string_of_int(x - 12000000)) else
	if is_eta x then "E"^ (string_of_int(x - 13000000)) else
	if is_delta x then "D"^ (string_of_int(x - 14000000)) else
	if is_cond x then "C"^ (string_of_int(x - 15000000)) else
	if is_act x then "A"^ (string_of_int(x - 16000000)) else
	if is_dbg x then "dbg"^(string_of_int(x - 17000000)) else
	if is_inp (- x)then "Iv"^ (string_of_int((- x) - 0)) else
	if is_ext (- x)then "Sv"^ (string_of_int((- x) - 1000000)) else
	if is_lcl (- x)then "Lv"^ (string_of_int((- x) - 2000000)) else
	if is_aux (- x)then "Hv"^ (string_of_int((- x) - 4000000)) else
		raise (Ly.Error ("sgn2str: "^string_of_int x))

let sgn2val s = - s
let is_val s = s < 0
let is_inpval s = is_inp (abs s)
let is_extval s = is_ext (abs s)
let is_lclval s = is_lcl (abs s)
let sgnorval2sgn s = abs s

let sgn2valstr s = sgn2str (- s)

let is_reg_or_mem_or_pre s = is_reg s || is_mem s || is_pre s
