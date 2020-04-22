#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;

class SendMoreMoney : public Space {

  protected:
    IntVarArray l;

  public:            // *this is called "home space"
    SendMoreMoney() : l(*this, 8, 0, 9) { // home, n, min, max
      IntVar s(l[0]), e(l[1]), n(l[2]), d(l[3]), m(l[4]), o(l[5]), r(l[6]), y(l[7]);

      rel(*this, s != 0); // no leading zero
      rel(*this, m != 0); // no leading zero
      distinct(*this, l); // distinct digits on each char
      // send + more = money
      rel(*this,             1000*s + 100*e + 10*n + d
                           + 1000*m + 100*o + 10*r + e
                == 10000*m + 1000*o + 100*n + 10*e + y
         );

      branch(*this, l, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); // Branch over x with variable selection vars and value selection vals.
      // INT_VAR_SIZE_MIN   Select variable with smallest domain size.
      // INT_VAL_MIN        Select smallest value.
    }

    void print() const {
      std::cout << l << std::endl;
    }

    // Required
    // copy constructor
    SendMoreMoney(SendMoreMoney& s) : Space(s) {
      l.update(*this, s.l);
    }

    // Required
    // capable of returning a fresh copy of the model during search.
    virtual Space* copy() {
      return new SendMoreMoney(*this);
    }
};



int main() {

  try {

    SendMoreMoney* m = new SendMoreMoney;

    DFS<SendMoreMoney> e(m); // Depth-First Search Engine

    delete m; // clones the model m so we can safely remove it

    // Search all solutions
    // returns next element or NULL
    while (SendMoreMoney* s = e.next()) {
      s->print(); delete s;
    }

  } catch (Exception e) {

    std::cerr << "Gecode exception: " << e.what() << std::endl;
    return 1;

  }

  return 0;
}
