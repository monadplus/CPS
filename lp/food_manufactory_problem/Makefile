CCC = g++

CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG -DIL_STD

SYSTEM     = x86-64_linux
LIBFORMAT  = static_pic

CPLEXDIR      = /nix/store/zsrmkd42gcmsm5g1a9lycag5blbk32q4-home-manager-path/cplex
CONCERTDIR    = /home/arnau/MIRI/CPS/lp/cplex/concert

CPLEXBINDIR   = $(CPLEXDIR)/bin/$(BINDIST)
CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)
CONCERTLIBDIR = $(CONCERTDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CONCERTINCDIR = $(CONCERTDIR)/include
CPLEXINCDIR   = $(CPLEXDIR)/include
CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -L$(CONCERTLIBDIR) -lconcert -lm -pthread -ldl

all: main main2

main: main.o
	$(CCC) -o $@ $< $(CCFLAGS) $(CCLNFLAGS)
main.o: main.cpp
	$(CCC) -c $< $(CCFLAGS)

main2: main2.o
	$(CCC) -o $@ $< $(CCFLAGS) $(CCLNFLAGS)
main2.o: main2.cpp
	$(CCC) -c $< $(CCFLAGS)

clean:
	rm --force main main.o main2 main2.o
