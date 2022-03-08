CC := gcc

CFLAGS := -std=c99 -Wall -Werror -pedantic -Wfloat-equal -Wfloat-conversion -I ./inc/ -lm

OBJS := ./out/main.o ./out/interpoltools.o

.PHONY: clean release debug

release: lastrelease | app.exe

debug: CFLAGS += -g3
debug: lastdebug | app.exe

lastrelease: ./out/
	-rm ./out/lastdebug app.exe
	touch ./out/lastrelease

lastdebug: ./out/
	-rm ./out/lastrelease app.exe
	touch ./out/lastdebug

./out/:
	-mkdir out

app.exe: $(OBJS)
	gcc $(CFLAGS) $(OBJS) -o $@

./out/%.o: ./src/%.c ./inc/*.h
	gcc $(CFLAGS) $< -c -o $@

clean:
	-rm app.exe
	-rm -r ./out/
