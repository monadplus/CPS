
all: ex1 ex2

ex1: ex1.o
	$(CCC) $(CCFLAGS) ex1.o -o ex1 $(CCLNFLAGS)
ex1.o: ex1.cpp
	$(CCC) -c $(CCFLAGS) ex1.cpp -o ex1.o

ex2: ex2.o
	$(CCC) $(CCFLAGS) ex2.o -o ex2 $(CCLNFLAGS)
ex2.o: ex2.cpp
	$(CCC) -c $(CCFLAGS) ex2.cpp -o ex2.o

clean:
	rm --force ex1 ex1.o ex2 ex2.o

################################

SYSTEM     = x86-64_linux
LIBFORMAT  = static_pic
CPLEXDIR      = /nix/store/zsrmkd42gcmsm5g1a9lycag5blbk32q4-home-manager-path/cplex
CONCERTDIR    = /home/arnau/MIRI/CPS/lp/cplex/concert

CCC = g++

CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG -DIL_STD

CPLEXBINDIR   = $(CPLEXDIR)/bin/$(BINDIST)
CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)
CONCERTLIBDIR = $(CONCERTDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -L$(CONCERTLIBDIR) -lconcert -lm -pthread -ldl

# -ldl needed for CPLEX version > 12.6


CONCERTINCDIR = $(CONCERTDIR)/include
CPLEXINCDIR   = $(CPLEXDIR)/include

CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR)
