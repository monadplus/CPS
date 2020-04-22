CXX  = g++ -std=c++11
DIR  = /usr/local
LIBS = -lgecodedriver    -lgecodesearch  \
       -lgecodeminimodel -lgecodeint     \
       -lgecodekernel    -lgecodesupport

all: graph-coloring

graph-coloring: graph-coloring.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)


clean:
	rm -f graph-coloring *.o
