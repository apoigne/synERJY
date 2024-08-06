# Makefile
##########################################################################

CC      = gcc
CFLAGS  = -ansi
CFLAGS += -I$(SE_HOME)/include
CFLAGS += -Dse__linux
CFLAGS += $(INCDIR)

##########################################################################

AvrCore_Test: se.AvrCore_Test.c
	$(CC) $(CFLAGS) se.AvrCore_Test.c -o se.a.out -L$(SE_HOME)/lib \
              -lse.linux.sim -lse.linux.rt -lnsl -lpthread

se.AvrCore_Test.c se.AvrCore_Test.h: ../AvrCore_Test.se $(SE_HOME)/lib/AvrSim.se
	rm -f se.AvrCore_Test.c se.AvrCore_Test.h
	( cd ..;\
	  se -f "% \
		  load file = $(SE_HOME)/lib/AvrSim.se;\
	          load file = AvrCore_Test.se;\
                 set simulation directory=sim;\
	         make simulation C-code; \
		 quit;" \
	)

clean:
	$(RM) se.* *.o

