#
# Very simple Makefile, works in my environment
#

INCLUDES = -I $(ERL_TOP)/erts/emulator/beam -I $(ERL_TOP)/erts/include/x86_64-unknown-linux-gnu 

all:	fd_nif.so

%.so:	%.c
	gcc -fpic -shared -Wall $(INCLUDES) $< -o $@

clean:
	rm fd_nif.so

