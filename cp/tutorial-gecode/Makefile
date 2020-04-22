CXX  = g++ -std=c++11
DIR  = /usr/local
LIBS = -lgecodedriver    -lgecodesearch  \
       -lgecodeminimodel -lgecodeint     \
       -lgecodekernel    -lgecodesupport
TEST_EVALUATION = test-evaluation
SEND_MORE_MONEY = send-more-money-handling-exceptions
SEND_MOST_MONEY = send-most-money

all: $(TEST_EVALUATION) $(SEND_MORE_MONEY) $(SEND_MOST_MONEY)

$(TEST_EVALUATION): $(TEST_EVALUATION).cpp
	#$(CXX) -I$(DIR)/include -c $(TEST_EVALUATION).cpp # Header files
	#$(CXX) -L$(DIR)/lib -o p $(TEST_EVALUATION).o $(LIBS) # Shared libraries
	# mkDerivation sets NIX_CFLAGS_COMPILE (compiler flags) and NIX_LDFLAGS (headers / shared libraries)
	$(CXX) -c $(TEST_EVALUATION).cpp
	$(CXX) -o $(TEST_EVALUATION) $(TEST_EVALUATION).o $(LIBS)

$(SEND_MORE_MONEY): $(SEND_MORE_MONEY).cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

$(SEND_MOST_MONEY): $(SEND_MOST_MONEY).cpp
	$(CXX) -c $<
	$(CXX) -o $@ $< $(LIBS)

clean:
	rm -f $(TEST_EVALUATION) $(SEND_MORE_MONEY) $(SEND_MOST_MONEY) *.o
