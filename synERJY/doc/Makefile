now: all

clean::
	rm -f */*.log */*.aux */*.idx */*.ilg */*.ind */*.toc \
              */*.dvi

realclean: clean
	rm -f language_intro.pdf
	rm -f language_ref.pdf
	rm -f user_manual.pdf

all: texinput  language_intro.pdf language_ref.pdf user_manual.pdf

texinput:
	bin/mk_exmpl.pl -all

language_intro.pdf: language_intro/*.tex
	( cd language_intro; make run_tex -f ../Makefile )
	rm language_intro/main.ps
	cp language_intro/main.pdf language_intro.pdf

language_ref.pdf: language_ref/*.tex
	( cd language_ref;   make run_tex -f ../Makefile )
	rm language_ref/main.ps
	cp language_ref/main.pdf   language_ref.pdf

user_manual.pdf: user_manual/*.tex
	( cd user_manual;    make run_tex -f ../Makefile )
	rm user_manual/main.ps
	cp user_manual/main.pdf    user_manual.pdf

run_tex:
	latex main.tex; makeindex main.idx; latex main.tex; \
        dvips -f <main.dvi >main.ps; ps2pdf main.ps

help:
	perl -n -e 'print "$$_" if /^[\w]*:/' Makefile
