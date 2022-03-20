EXECUTABLE = flp21-fun

all: $(EXECUTABLE)

$(EXECUTABLE): src/Main.hs src/Types.hs src/ParseInput.hs src/Simplify.hs
	ghc -Wall -o $(EXECUTABLE) src/Main.hs src/Types.hs src/ParseInput.hs src/Simplify.hs

clean:
	rm -f $(EXECUTABLE) src/Main.o src/Main.hi src/Types.o src/Types.hi src/ParseInput.o src/ParseInput.hi src/Simplify.o src/Simplify.hi

zip:
	zip flp-fun-xholko02 Makefile src/Main.hs src/Types.hs src/ParseInput.hs src/Simplify.hs doc/* test/*