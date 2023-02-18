SOURCES := $(wildcard *.asm) # Todos los ficheros .asm
OBJECTS := $(subst .asm,.o,$(SOURCES)) # Todos los ficheros .o


magica: $(OBJECTS)
	wlalink -S linkfile "invasion.sms"


%.o:%.asm
	wla-z80 -o $@ $^

info: # Informacion sobre los ficheros a ensamblador
	$(info $(SOURCES)) 
	$(info $(OBJECTS))

cleanall: 
	rm -f $(OBJECTS) # Borrado de ficheros objeto
	rm -f *.sms 	 # Borrado de  ejecutable sms