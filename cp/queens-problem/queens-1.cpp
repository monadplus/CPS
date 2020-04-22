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
    BoolVarArray q;

  public:
    Queens(int nn): n(nn), q(*this, nn*nn, 0, 1) {

      // All queens placed.
      linear(*this, q, IRT_EQ, n);

      // One queen per row
      for (int i = 0; i < n; ++i) {
        BoolVarArgs v(n);

        for (int j = 0; j < n; ++j) {
          v[j] = queen(i, j);
        }

        linear(*this, v, IRT_LQ, 1);
      }

      // One queen per column
      for (int i = 0; i < n; ++i) {
        BoolVarArgs v(n);

        for (int j = 0; j < n; ++j) {
          v[j] = queen(j, i);
        }

        linear(*this, v, IRT_LQ, 1);
      }

      // At most 1 queen per descending diagonal.

      // i - j = k  --> j = i - k
      //
      // 0 <= i <= n-1
      //
      // 0 <= j <= n-1
      // 0 <= i -k <= n - 1
      // k <= i <= k+n-1
      //
      // Ahora i tiene que satisfacer dos lower, and upper bounds
      // max(0, k) <= i <= min(n-1, k+n-1)
      // min(k) = 1-n
      // max(k) = n-1
      for (int k = 1-n; k <= n-1; ++k) {
        int l = max(0, k);
        int u = min(n-1, k+n-1);
        BoolVarArgs v(u-l+1);
        for (int i = l; i <= u; ++i) {
          int j = i - k;
          v[i-l] = queen(i, j);
        }
        linear(*this, v, IRT_LQ, 1);
      }

      // At most 1 queen per ascending diagonal
      //
      // i + j = k
      //
      // 0 <= i <= n-1
      // 0 <= j <= n-1 -> k-n+1 <= i <= k
      //
      // max(0, k-n+1) <= i <= min(n-1, k)
      // min(k) = 0
      // max(k) = 2n-2
      for (int k = 0; k <= 2*n-2; ++k) {
        int l = max(0, k-n+1);
        int u = min(n-1, k);
        BoolVarArgs v(u-l+1);
        for (int i = l; i <= u; ++i) {
    int j = k - i;
    v[i-l] = queen(i, j);
        }
        linear(*this, v, IRT_LQ, 1);
      }

      // https://www.gecode.org/doc/5.1.0/reference/group__TaskModelIntBranchVar.html
      branch(*this, q, BOOL_VAR_NONE(), BOOL_VAL_MAX());
    }

    BoolVar queen(int i, int j) const {
      return q[i*n+j];
    }

    Queens(Queens& qq) : Space(qq) {
      q.update(*this, qq.q);
      n = qq.n;
    }

    virtual Queens* copy() {
      return new Queens(*this);
    }

    void print() const {
      for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
          std::cout << (queen(i,j).val() ? 'X' : '.');
        }
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
