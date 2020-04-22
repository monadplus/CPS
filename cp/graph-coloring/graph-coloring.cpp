#include <fstream>
#include <vector>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace std;
using namespace Gecode;

class GraphColoring : public Space {

  int n; // nodes

  protected:
    IntVarArray c;

  public:
    GraphColoring(const vector<vector<int>>& g) : n(g.size()), c(*this, n, 1, n) {
      // Adjacent vertex of different colours.
      for (int u = 0; u<n; u++) {
        for (int v : g[u])
          if (u < v) // dont check already checked nodes.
            rel(*this, c[u] != c[v]);
      }

      branch(*this, c, INT_VAR_NONE(), INT_VAL_MIN());
    }

    int max_colour (const GraphColoring& g) const {
      int max_c = 0;

      for (int i = 0; i<g.n; i++) {
        if (g.c[i].val() > max_c)
          max_c = g.c[i].val();
      }

      return max_c;
    }

    virtual void constrain(const Space& _b) {
      const GraphColoring& b = static_cast<const GraphColoring&>(_b);

      int max_c = max_colour(b);

      for (int i = 0; i<n; i++) {
        rel(*this, c[i] < max_c);
      }
    }

    GraphColoring(GraphColoring& s) : Space(s) {
      c.update(*this, s.c);
      n = s.n;
    }

    virtual Space* copy() {
      return new GraphColoring(*this);
    }

    void print() const {
      int max_c = max_colour(*this);

      cout << max_c << endl;

      for (int u = 0; u<n; ++u)
        cout << u+1 << ' ' << c[u].val() << endl;
    }
};

int main(int argc, char* argv[]) {
  try {
    if (argc != 2) return 1;
    ifstream in(argv[1]);
    int n, m;
    in >> n >> m;
    vector<vector<int>> g(n);
    for (int k = 0; k < m; ++k) {
      int u, v;
      in >> u >> v;
      --u; --v;
      g[u].push_back(v);
      g[v].push_back(u);
    }
    GraphColoring* mod = new GraphColoring(g);
    BAB<GraphColoring> e(mod);
    delete mod;
    GraphColoring* sant = e.next();
    GraphColoring* s    = e.next();
    while (s != NULL) {
      delete sant;
      sant = s;
      s = e.next();
    }
    sant->print();
    delete sant;
  }
  catch (Exception e) {
    cerr << "Gecode exception: " << e.what() << endl;
    return 1;
  }
  return 0;
}
