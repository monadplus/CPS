CXX  = g++ -std=c++11
DIR  = /usr/local
LIBS = -lgecodedriver    -lgecodesearch  \
       -lgecodeminimodel -lgecodeint     \
       -lgecodekernel    -lgecodesupport

all: queens-1 queens-2 queens-3 queens-4 queens-5 queens-6

queens-1: queens-1.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

queens-2: queens-2.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

queens-3: queens-3.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

queens-4: queens-4.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

queens-5: queens-5.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

queens-6: queens-6.cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

clean:
	rm -f queens-1 queens-2 queens-3 queens-4 queens-5 queens-6 *.o
