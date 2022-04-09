HC := ghc

FLAGS := -dynamic

SRCS := $(wildcard *.hs)

.PHONY: release clean

release: app.exe

app.exe: $(SRCS)
	ghc $(FLAGS) $(SRCS) -o $@

clean:
	-rm app.exe
	-rm *.o
	-rm *.hi
