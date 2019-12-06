GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -O
GHCBUILDFLAGS = -isrc -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

EXECUTABLE = mcheck

all: $(EXECUTABLE) 

$(EXECUTABLE): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(GHCBUILDFLAGS) src/Main.hs -o $(EXECUTABLE)

doc: $(SOURCE)
	haddock --html -o doc $(SOURCE)

clean: 
	rm -rf build/* $(EXECUTABLE) doc
