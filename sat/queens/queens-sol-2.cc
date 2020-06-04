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

string int2string(int x) {
  ostringstream ost;
  ost << x;
  return ost.str();
}


string operator-(const string& str) {
  if (str[0] == '-') return str.substr(1); 
  else               return "-" + str;
}


string x(int i, int j) {
  assert(0 <= i and i < n);
  assert(0 <= j and j < n);
  return int2string(i*n + j + 1) + " ";
}


void add_clause(string c) {
  cnf << c << "0" << endl;
  ++n_clauses;
}


string new_variable() { 
  return int2string(++n_vars) + " ";
}


int log2(int x) {
  int p = 1;
  int s = 0;
  while (p < x) {
    p *= 2;
    ++s;
  }
  return s;
}


int bit(int j, int x) {
  for (int k = 0; k < j; ++k)
    x /= 2;
  return x % 2;
}


void add_amo(const vector<string>& x) {
  int N = x.size();
  int M = log2(N);
  vector<string> y(M);
  for (int j = 0; j < M; ++j)
    y[j] = new_variable();

  for (int i = 0; i < N; ++i)
    for (int j = 0; j < M; ++j)
      if (bit(j,i)) add_clause(-x[i] V  y[j]);
      else          add_clause(-x[i] V -y[j]);    
}


void write_CNF() {

  n_vars = n*n;

  // At least one queen for each row.
  for (int i = 0; i < n; ++i) {
    string c;
    for (int j = 0; j < n; ++j)
      c += x(i,j);
    add_clause(c);
  }

  // At least one queen for each column.
  for (int j = 0; j < n; ++j) {
    string c;
    for (int i = 0; i < n; ++i)
      c += x(i,j);
    add_clause(c);
  }

  // At most one queen for each row.
  for (int i = 0; i < n; ++i) {
    vector<string> z;
    for (int j = 0; j < n; ++j)
      z.push_back(x(i,j));
    add_amo(z);
  }

  // At most one queen for each column.
  for (int j = 0; j < n; ++j) {
    vector<string> z;
    for (int i = 0; i < n; ++i)
      z.push_back(x(i,j));
    add_amo(z);
  }

  // At most one queen for each ascending diagonal.
  for (int k = 0; k <= 2*n-2; ++k) {
    vector<string> z;
    for (int i = max(0, k-n+1); i <= min(n-1, k); ++i) {
      int j = k - i;
      z.push_back(x(i,j));
    }
    add_amo(z);
  }

  // At most one queen for each descending diagonal.
  for (int k = 1-n; k <= n-1; ++k) {
    vector<string> z;
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
      if (i < q.size()) q[i] = j;
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
