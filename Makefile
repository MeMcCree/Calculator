CC=gcc
CFLAGS=-O2
PCFLAGS=-lm

calc.exe: main.c
	$(CC) $(CFLAGS) -o $@ $^ $(PCFLAGS)

clean: calc.exe
	rm $^

install: calc.exe
	install ./calc.exe /usr/local/bin/calc

uninstall:
	rm /usr/local/bin/calc