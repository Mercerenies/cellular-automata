
.PHONY:	all clean

all:	Main.o

clean:
		rm Control/*.o Control/UI/*.o *.o *.a Main

Main.o:	Main.hs Control/Automaton.hs Control/UI/Cellular.hs libgui.a
		ghc -O -Wall -Werror Main.hs libgui.a $(shell pkg-config --libs gtk+-3.0)

libgui.a:	gui.o
		ar rcs libgui.a gui.o

gui.o: gui.c
		gcc -Wall -c -o gui.o $(shell pkg-config --cflags gtk+-3.0) gui.c
