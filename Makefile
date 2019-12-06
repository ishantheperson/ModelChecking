ifeq (andrew.cmu.edu,$(findstring andrew.cmu.edu,$(shell hostname)))
GHC = /afs/andrew.cmu.edu/course/15/354/handin/ibhargav/.ghcup/bin/ghc
else 
GHC = ghc
endif 
BUILDDIR = build 
GHCFLAGS = -O -W
GHCBUILDFLAGS = -isrc -odir	$(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

EXECUTABLE = mcheck

all: $(EXECUTABLE)
	mkdir -p bin 
	cp mcheck bin

$(EXECUTABLE): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(GHCBUILDFLAGS) src/Main.hs -o $(EXECUTABLE)

doc: $(SOURCE)
	haddock --html -o doc $(SOURCE)

clean: 
	rm -rf build/* $(EXECUTABLE) doc
