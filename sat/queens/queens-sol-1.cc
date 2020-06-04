#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <assert.h>
#include <stdlib.h>

#define V +

using namespace std;

int n;
int n_vars;
int n_clauses;

ofstream cnf;
ifstream sol;

typedef string literal;
typedef string  clause;

literal operator-(const literal& lit) {
  if (lit[0] == '-') return lit.substr(1);
  else               return "-" + lit;
}


literal x(int i, int j) {
  assert(0 <= i and i < n);
  assert(0 <= j and j < n);
  return to_string(i*n + j + 1) + " ";
}


void add_clause(const clause& c) {
  cnf << c << "0" << endl;
  ++n_clauses;
}


void add_amo(const vector<literal>& z) {
  int N = z.size();
  for (int i1 = 0; i1 < N; ++i1)
    for (int i2 = i1+1; i2 < N; ++i2)
      add_clause(-z[i1] V -z[i2]);
}


void write_CNF() {

  n_vars = n*n;

  // At least one queen for each row.
  for (int i = 0; i < n; ++i) {
    clause c;
    for (int j = 0; j < n; ++j)
      c = c V x(i,j);
    add_clause(c);
  }

  // At least one queen for each column.
  for (int j = 0; j < n; ++j) {
    clause c;
    for (int i = 0; i < n; ++i)
      c = c V x(i,j);
    add_clause(c);
  }

  // At most one queen for each row.
  for (int i = 0; i < n; ++i) {
    vector<literal> z;
    for (int j = 0; j < n; ++j)
      z.push_back(x(i,j));
    add_amo(z);
  }

  // At most one queen for each column.
  for (int j = 0; j < n; ++j) {
    vector<literal> z;
    for (int i = 0; i < n; ++i)
      z.push_back(x(i,j));
    add_amo(z);
  }

  // At most one queen for each ascending diagonal.
  for (int k = 0; k <= 2*n-2; ++k) {
    vector<literal> z;
    for (int i = max(0, k-n+1); i <= min(n-1, k); ++i) {
      int j = k - i;
      z.push_back(x(i,j));
    }
    add_amo(z);
  }

  // At most one queen for each descending diagonal.
  for (int k = 1-n; k <= n-1; ++k) {
    vector<literal> z;
    for (int i = max(0, k); i <= min(n-1, n+k-1); ++i) {
      int j = i - k;
      z.push_back(x(i,j));
    }
    add_amo(z);
  }

  // Symmetry over the middle vertical line: column of row 0 is <= n/2-1 [n even], n/2 [n odd]
  if (n % 2 == 0)
    for (int j = n/2; j < n; ++j)
      add_clause( -x(0,j) );
  if (n % 2 == 1)
    for (int j = n/2+1; j < n; ++j)
      add_clause( -x(0,j) );

  // Symmetry over the middle horizontal line: column of row 0 is <= column of row n-1
  for (int j1 = 0; j1 < n; ++j1)
    for (int j2 = j1+1; j2 < n; ++j2)
      add_clause( -x(0,j2) V -x(n-1,j1) );

  cnf << "p cnf " << n_vars << " " << n_clauses << endl;
}


void get_solution(vector<int>& q) {
  int lit;
  while (sol >> lit)
    if (lit > 0) {
      int i = (lit-1) / n;
      int j = (lit-1) % n;
      q[i] = j;
    }
}


void write_solution(const vector<int>& q) {
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j)
      if (j == q[i]) cout << "X";
      else           cout << ".";
    cout << endl;
  }
}


int main(int argc, char** argv) {
  assert(argc == 2);
  n = atoi(argv[1]);
  assert(n >= 4);

  cnf.open("tmp.rev");
  write_CNF();
  cnf.close();

  system("tac tmp.rev | lingeling | grep -E -v \"^c\" | tail --lines=+2 | cut --delimiter=' ' --field=1 --complement > tmp.out");

  vector<int> q(n);
  sol.open("tmp.out");
  get_solution(q);
  sol.close();

  write_solution(q);
}
