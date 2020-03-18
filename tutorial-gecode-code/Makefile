CXX  = g++ -std=c++11
DIR  = /usr/local
LIBS = -lgecodedriver    -lgecodesearch  \
       -lgecodeminimodel -lgecodeint     \
       -lgecodekernel    -lgecodesupport
PROG_NAME = ite

p: $(PROG_NAME).cpp
	#$(CXX) -I$(DIR)/include -c $(PROG_NAME).cpp # Header files
	#$(CXX) -L$(DIR)/lib -o p $(PROG_NAME).o $(LIBS) # Shared libraries
	# mkDerivation sets NIX_CFLAGS_COMPILE (compiler flags) and NIX_LDFLAGS (headers / shared libraries)
	$(CXX) -c $(PROG_NAME).cpp
	$(CXX) -o p $(PROG_NAME).o $(LIBS)
