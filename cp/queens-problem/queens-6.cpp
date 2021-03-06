#include <cassert>
#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace std;
using namespace Gecode;

class Queens : public Space {
  protected:
    int n;
    IntVarArray c;

  public:
    Queens(int nn): n(nn), c(*this, n, 0, n-1) {

      distinct(*this, c, IPL_DOM);
      distinct(*this, IntArgs::create(n, 0,  1), c, IPL_DOM);
      distinct(*this, IntArgs::create(n, 0,  -1), c, IPL_DOM);

      // Symmetry
      if (n % 2 == 0) rel(*this, c[0] <  n/2);
      if (n % 2 == 1) rel(*this, c[0] <= n/2);
      rel(*this, c[0] <= c[n-1]);

      // huge improvement
      branch(*this, c, INT_VAR_SIZE_MIN(), INT_VAL_MAX());
    }

    Queens(Queens& q) : Space(q) {
      c.update(*this, q.c);
      n = q.n;
    }

    virtual Queens* copy() {
      return new Queens(*this);
    }

    void print() const {
      for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j)
          std::cout << (c[i].val() == j ? 'X' : '.');
        std::cout << std::endl;
      }
    }
};

int main(int argc, char* argv[]) {

  try {

    if (argc != 2) {
      std::cout << "Expecting n (number of queens)" << std::endl;
      return 1;
    }

    int n = atoi(argv[1]);

    Queens* m = new Queens(n);

    DFS<Queens> e(m);

    delete m;

    if (Queens* s = e.next()) {
      s->print();
      delete s;
    }

  } catch (Exception e) {

    cerr << "Gecode exception: " << e.what() << std::endl;
    return 1;

  }

  return 0;
}
