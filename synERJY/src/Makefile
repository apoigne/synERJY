# THIS MAKEFILE REQUIRES GNU-MAKE
#                        ========
#
# 1. this makefile supports linux and darwin
#     (windows and nt installations use another makefile)
# 2. run: make clean
# 3. run: make all              make se, ge, sim
# 4. run: make help             to see rule names

include ../Makefile.env
include $(SE_HOME)/Makefile.$(OSTYPE)

# =============================================================================
CAMLC     := /usr/local/bin/ocamlc -dtypes
CAMLO     := /usr/local/bin/ocamlopt
CAMLT     := /usr/local/bin/ocamlmktop
CAMLD     := /usr/local/bin/ocamldep
CAMLLEX   := /usr/local/bin/ocamllex
CAMLYACC  := /usr/local/bin/ocamlyacc

DEBUG     :=
OPT       := -inline 3
OPT       := -p
OPT       :=

# >> partitioning of the file to MAKE =========================================
ML_BASE := ly.ml sc.ml ast.ml sim_error.ml sim_type.ml \
           p.ml tk.ml err.ml util.ml util_tk.ml \
           graphic_util.ml util_print.ml \
           util_parse.ml yacc.ml lex.ml lex_wrapper.ml

ML_PU   := graphic_parse.ml inherit_check.ml builtin_data.ml typecheck.ml \
           data_depend.ml react2sya.ml react2sc.ml \
           gen_application.ml util_gen.ml \
           util_gen_c.ml gen_rct_code.ml gen_c_code.ml \
           gen_verilog.ml gen_formula.ml gen_NuSMV.ml \
           puctrl.ml putk.ml debug.ml

ML_GE   := gredit_util.ml gredit_tk.ml gredit_tool.ml

ML_SIM  := sim_util.ml sim_parse.ml sim_base.ml \
           sim_target.ml sim_play.ml sim_react.ml sim_tool.ml sim_main.ml \
           gredit_util.ml gredit_tk.ml sim_tk.ml

ML_SA   := ly.ml sc.ml ast.ml sim_error.ml sim_type.ml p.ml err.ml util.ml \
           graphic_util.ml util_print.ml util_parse.ml yacc.ml \
           lex.ml lex_wrapper.ml graphic_parse.ml inherit_check.ml \
           builtin_data.ml typecheck.ml data_depend.ml react2sya.ml \
           react2sc.ml gen_application.ml util_gen.ml util_gen_c.ml \
           gen_rct_code.ml gen_c_code.ml gen_verilog.ml puctrl.ml sa.ml

CMO_BASE := $(subst .ml,.cmo,$(ML_BASE))
CMX_BASE := $(subst .ml,.cmx,$(ML_BASE))
CMO_PU   := $(subst .ml,.cmo,$(ML_PU))
CMX_PU   := $(subst .ml,.cmx,$(ML_PU))
CMO_GE   := $(subst .ml,.cmo,$(ML_GE))
CMO_SIM  := $(subst .ml,.cmo,$(ML_SIM))
CMO_SA   := $(subst .ml,.cmo,$(ML_SA))

#------------------------------------------------------------------------------
now: se

dbg: $(SE_HOME)/bin/se_dbg $(SE_HOME)/bin/ge_dbg $(SE_HOME)/bin/sim_dbg

all: se ge sim

se:     $(SE_HOME)/bin/se_dbg  $(SE_HOME)/bin/se
ge:     $(SE_HOME)/bin/ge_dbg  $(SE_HOME)/bin/ge
sim:    $(SE_HOME)/bin/sim_dbg $(SE_HOME)/bin/sim
opt:    se $(SE_HOME)/bin/se_opt
sa:	$(SE_HOME)/bin/synERJYbatch

newopt:
	rm -f *.cmx
	@make opt

#------------------------------------------------------------------------------
%.cmi : %.mli
	rm -f $*.cmo $*.cmx
	$(CAMLC) $(ILibs) $(DEBUG) -c $<

%.cmo : %.ml
	$(CAMLC) $(ILibs) $(DEBUG) -c $<

%.cmx : %.cmo
	$(CAMLO) $(ILibs) $(DEBUG) $(OPT) -c $*.ml

%.o : %.c
	$(CAMLC) -I $(X11H) -I $(TKH) -I $(TCLH) -I $(MLD) \
                 -I $(SE_HOME)/include -ccopt -c $<

#------------------------------------------------------------------------------
$(SE_HOME)/bin/se_dbg: ocamltk.o $(CMO_BASE) $(CMO_PU)
	LD_LIBRARY_PATH='' \
	$(CAMLT) -custom -o $(SE_HOME)/bin/se_dbg $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

$(SE_HOME)/bin/se: ocamltk.o $(CMO_BASE) $(CMO_PU) main_se.cmo
	LD_LIBRARY_PATH='' \
	$(CAMLC) -custom -o $(SE_HOME)/bin/synERJY $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

$(SE_HOME)/bin/sa: $(CMO_SA) main_sa.cmo
	LD_LIBRARY_PATH='' \
	$(CAMLC) -custom -o $(SE_HOME)/bin/synERJYbatch $(ILibs) $(DEBUG) \
                 $(CMA) $+ -cclib "$(MLA) $(LL)"

#------------------------------------------------------------------------------
$(SE_HOME)/bin/ge_dbg: ocamltk.o $(CMO_BASE) $(CMO_GE)
	LD_LIBRARY_PATH='' \
	$(CAMLT) -custom -o $(SE_HOME)/bin/ge_dbg $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

$(SE_HOME)/bin/ge: $(CMO_BASE) ocamltk.o $(CMO_GE) main_ge.cmo
	LD_LIBRARY_PATH='' \
	$(CAMLC) -custom -o $(SE_HOME)/bin/synERJYcharts $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

#------------------------------------------------------------------------------
$(SE_HOME)/bin/sim_dbg: ocamltk.o $(CMO_BASE) $(CMO_SIM) mlunit.cmo
	LD_LIBRARY_PATH='' \
	$(CAMLT) -custom -o $(SE_HOME)/bin/sim_dbg $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

$(SE_HOME)/bin/sim: ocamltk.o $(CMO_BASE) $(CMO_SIM) main_sim.cmo
	LD_LIBRARY_PATH='' \
	$(CAMLC) -custom -o $(SE_HOME)/bin/synERJYsim $(ILibs) $(DEBUG) \
                 $(CMA) $+ \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(MLA) $(LL)"

#------------------------------------------------------------------------------
$(SE_HOME)/bin/se_opt: ocamltk.o $(CMX_BASE) $(CMX_PU) main_se.cmx
	LD_LIBRARY_PATH='' \
	$(CAMLO) -o $(SE_HOME)/bin/se_opt $(OPT) $(ILibs) $(DEBUG) \
                 $(CMX) ocamltk.o $(CMX_BASE) $(CMX_PU) main_se.cmx \
                 -cclib "$(TKA) $(TCLA) $(X11A) $(LL)"

#------------------------------------------------------------------------------
ocamldoc:
	ocamldoc -html -all-params -d mldoc *.ml *.mli

#------------------------------------------------------------------------------
ly.cmo: ly.cmi
sc.cmo: sc.cmi
puctrl.cmo: lex.cmo yacc.cmo builtin_data.cmo
gredit_tk.cmo: lex.cmo yacc.cmo
lex_wrapper.cmo: lex.cmo yacc.cmo

yacc.cmo: yacc.mly util.cmo util_parse.cmo ly.cmo ast.cmo sim_type.cmo
	rm -f yacc.ml yacc.mli
	$(CAMLYACC) yacc.mly
	$(CAMLC) $(ILibs) -c yacc.mli
	$(CAMLC) $(ILibs) -c yacc.ml

lex.ml: lex.mll ly.cmo yacc.cmo
	rm -f lex.ml; $(CAMLLEX) lex.mll;
	$(CAMLC) $(ILibs) -c lex.ml

#------------------------------------------------------------------------------
$(SE_HOME)/builtin/builtin: $(SE_HOME)/builtin/builtin.cmo
	( cd $(SE_HOME)/builtin; \
	  LD_LIBRARY_PATH='' \
	  $(CAMLC) $(ILibs) $(CMA) builtin.cmo -o builtin \
	); \
	$(SE_HOME)/builtin/builtin > builtin_data.ml

$(SE_HOME)/builtin/builtin.cmo: ly.cmo ast.cmo p.cmo err.cmo util_parse.cmo \
                            util_print.cmo inherit_check.cmo util.cmo \
                            $(SE_HOME)/builtin/builtin.ml
	( cd $(SE_HOME)/builtin; \
	  rm -f builtin.cmi builtin.cmo; \
	  $(CAMLC) $(ILibs) -c builtin.ml; \
	)

builtin_data.ml : $(SE_HOME)/builtin/builtin

#------------------------------------------------------------------------------
Makefile.depend:
	$(CAMLD) $(ILibs) *.ml[ily] *.ml > Makefile.depend

clean:
	rm -f *.cm[iox] *.o *.annot\
              builtin_data.ml yacc.ml yacc.mli lex.ml \
              $(SE_HOME)/builtin/*.cm[iox] $(SE_HOME)/builtin/builtin

help:
	perl -n -e 'print "$$_" if /^[\w]*:/' Makefile

# ---------------- ######### .depend   ########### ----------------------------
include Makefile.depend
