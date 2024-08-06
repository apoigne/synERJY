# Makefile
#-----------------------------------------------------------------------------
include $(SE_HOME)/include/Makefile.atmel

# mcu type at90s8515 is tested
MCU=at90s8515

#-----------------------------------------------------------------------------
# test programs
#-----------------------------------------------------------------------------
tests: Avr_at90s8515_Test AvrUart_Test AvrCore_Test
	ls *.elf

Avr_at90s8515_Test:    se.Avr_at90s8515_Test.elf
Avr_at90s8515_Test.up: se.Avr_at90s8515_Test.up

AvrUart_Test:    se.AvrUart_Test.elf
AvrUart_Test.up: se.AvrUart_Test.up

AvrCore_Test:    se.AvrCore_Test.elf
AvrCore_Test.up: se.AvrCore_Test.up

se.Avr_at90s8515_Test.c se.Avr_at90s8515_Test.h: Avr_at90s8515_Test.se $(SE_HOME)/misc/AvrMcu.se $(SE_HOME)/include/avr_mcu.h $(SE_HOME)/misc/Avr_$(MCU).se
	rm -f se.at90s8515_Test.c se.at90s8515_Test.h
	( se -f "%set target os = atmel;\
		  load file = $(SE_HOME)/misc/Avr_$(MCU).se;\
		  load file = $(SE_HOME)/misc/AvrMcu.se;\
	          load file = Avr_at90s8515_Test.se;\
	          make target C-code;\
		  quit;" \
	)

se.AvrUart_Test.c se.AvrUart_Test.h: AvrUart_Test.se $(SE_HOME)/misc/AvrMcu.se $(SE_HOME)/include/avr_mcu.h $(SE_HOME)/misc/Avr_$(MCU).se
	rm -f se.AvrUart_Test.c se.AvrUart_Test.h
	( se -f "%set target os = atmel;\
		  load file = $(SE_HOME)/misc/Avr_$(MCU).se;\
		  load file = $(SE_HOME)/misc/AvrMcu.se;\
	          load file = AvrUart_Test.se;\
	          make target C-code;\
	          quit;"\
	)

se.AvrCore_Test.c se.AvrCore_Test.h: AvrCore_Test.se $(SE_HOME)/misc/AvrMcu.se $(SE_HOME)/include/avr_mcu.h $(SE_HOME)/misc/Avr_$(MCU).se
	rm -f se.AvrCore_Test.c se.AvrCore_Test.h
	( se -f "%set target os = atmel;\
		  load file = $(SE_HOME)/misc/Avr_$(MCU).se;\
		  load file = $(SE_HOME)/misc/AvrMcu.se;\
		  load file = Avr.se;\
	          load file = AvrCore_Test.se;\
	          make target C-code;\
	          quit;"\
	)

help: Makefile.help

clean:
	$(RM) *.bak *.log *.srec *.s *.lst *.map *.rom *.elf *.eep *.o se.*
	$(RM) $(RM) tags

