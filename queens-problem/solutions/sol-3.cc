#include <cassert>
#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace std;
using namespace Gecode;

class Queens : public Space {

private:
  int n;
  IntVarArray c;

public:
  Queens(int nn) : n(nn), c(*this, n, 0, n-1) {

    distinct(*this, c);
    distinct(*this, IntArgs::create(n, 0,  1), c);
    distinct(*this, IntArgs::create(n, 0, -1), c);

    branch(*this, c, INT_VAR_NONE(), INT_VAL_MAX());
  }

  Queens(Queens& s) : Space(s) {
    c.update(*this, s.c);
    n = s.n;
  }

  virtual Space* copy() {
    return new Queens(*this);
  }

  void print() const {
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j)
	cout << (c[i].val() == j ? 'X' : '.');
      cout << endl;
    }
  }
};

int main(int argc, char* argv[]) {
  try {
    if (argc != 2) return 1;
    int n = atoi(argv[1]);
    Queens* m = new Queens(n);
    DFS<Queens> e(m);
    delete m;
    if (Queens* s = e.next()) {
      s->print();
      delete s;
    }
  } catch (Exception e) {
    std::cerr << "Gecode exception: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
