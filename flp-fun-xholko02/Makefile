EXECUTABLE = flp21-fun

all: $(EXECUTABLE)

$(EXECUTABLE): Main.hs Types.hs ParseInput.hs Simplify.hs
	ghc -o $(EXECUTABLE) Main.hs Types.hs ParseInput.hs Simplify.hs

clean:
	rm -f $(EXECUTABLE) Main.o Main.hi Types.o Types.hi ParseInput.o ParseInput.hi Simplify.o Simplify.hi