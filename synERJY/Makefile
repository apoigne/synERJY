# THIS MAKEFILE REQUIRES GNU-MAKE
#                        ========
#
# 1. this makefile supports unix variants, at least solaris, linux and darwin
#    (windows and nt installations use another makefile)
# 2. run: make clean
# 3. run: make all              make se, ge, sim + libraries
# 4. run: make help             to see rule names

include Makefile.env
include ./Makefile.$(OSTYPE)

#------------------------------------------------------------------------------
ABS_SE   := $(shell pwd)
SEDISTR := synERJY
SEDISTRDIR := /Users/ap/Sites/public_html/synERJY/Downloads/synERJY

VERSION  := 53
#------------------------------------------------------------------------------
all: libs bins

libs:
	make -C target/$(TARGET)/lib  >/dev/null

bins:
	if [ ! -d bin ] ;then mkdir bin; fi
	@make -C src all

docs:
	@make -C doc all

clean:
	rm -f src/*.cm[iox] src/*.o src/Makefile.depend \
              src/builtin_data.ml src/yacc.ml src/yacc.mli src/lex.ml \
              bin/synERJY  bin/se_dbg bin/se_opt bin/synERJYbatch\
              bin/synERJYcharts  bin/ge_dbg \
              bin/synERJYsim bin/sim_dbg \
              target/*/lib/*.o target/*/lib/*.a target/*/lib/*.lib \
	      builtin/*.cm[io] builtin/builtin
	make -C doc clean
	rm -f */*/lib/Makefile.depend

help:
	perl -n -e 'print "$$_" if /^[\w]*:/' Makefile

#------------------------------------------------------------------------------
regression:
	@make -C src opt
	@(cd $(SETEST);rm -f TEST_SUITE_LOG se.* *.se *.sc *.c *.h *.o)
	@echo regression test is running ...
	@time `(regression all >/dev/null 2>&1)`
	@\gvim '+:so ~/.vimrc' $(SETEST)/TEST_SUITE_LOG +/FAULT

#------------------------------------------------------------------------------
distr: libs $(OSTYPE)_bins
	make -C src opt
	rm -rf $(SEDISTRDIR)
	mkdir $(SEDISTRDIR)
	( cd $(SEDISTRDIR); mkdir bin doc target tmp images include util )
	cp $(SERC) $(SEDISTRDIR)/.serc
	cp bin/synERJYcharts $(SEDISTRDIR)/bin/synERJYcharts
	cp bin/synERJYsim $(SEDISTRDIR)/bin/synERJYsim
	cp bin/se_opt $(SEDISTRDIR)/bin/synERJY # distribute the native code compiler
	cp License.txt Readme* $(SEDISTRDIR)
	cp images/*.gif $(SEDISTRDIR)/images
	cp include/*.h $(SEDISTRDIR)/include
	cp util/*.se $(SEDISTRDIR)/util
	cp -r target/unix $(SEDISTRDIR)/target/unix
	rm -rf $(SEDISTRDIR)/target/unix/libs/src
	cp -r target/examples $(SEDISTRDIR)/target/unix
	cp doc/language_intro/main.pdf $(SEDISTRDIR)/doc/language_intro.pdf
	cp doc/language_ref/main.pdf $(SEDISTRDIR)/doc/language_ref.pdf
	cp doc/user_manual/main.pdf $(SEDISTRDIR)/doc/user_manual.pdf
	cp doc/language_intro/main.pdf doc/language_intro.pdf
	cp doc/language_ref/main.pdf doc/language_ref.pdf
	cp doc/user_manual/main.pdf doc/user_manual.pdf
	chmod -R ugo+rx,go-w,u+w $(SEDISTRDIR);
	( cd $(SEDISTRDIR); cd ..; tar zcf synERJY$(VERSION)_$(OSTYPE).tgz $(SEDISTRDIR) )
	rm -rf $(SEDISTRDIR)
	echo rm -rf $(SEDISTRDIR)
	
export_aqua:
	export OSTYPE=aqua
    
aqua_distr: export_aqua distr
		
