#include <ilcplex/ilocplex.h>
ILOSTLBEGIN

const int    Veg_Beg = 1;   const int    Veg_End = 2;
const int    Nvg_Beg = 3;   const int    Nvg_End = 5;
const int    Oil_Beg = 1;   const int    Oil_End = 5;
const int  Month_Beg = 1;   const int  Month_End = 6;
const int XMonth_Beg = 0;   const int XMonth_End = 6;

const double stg = 500;

const double _ = 0;
const double price[Month_End + 1][Oil_End + 1] =
  {{_,   _,   _,   _,   _,   _},
   {_, 110, 120, 130, 110, 115},
   {_, 130, 130, 110,  90, 115},
   {_, 110, 140, 130, 100,  95},
   {_, 120, 110, 120, 120, 125},
   {_, 100, 120, 150, 110, 105},
   {_,  90, 100, 140,  80, 135}};

const double h[Oil_End + 1] = {_, 8.8, 6.1, 2.0, 4.2, 5.0};
const double hlb = 3;
const double hub = 6;

const double ulb[Oil_End + 1] = {_,  20,  20,  20,  20,  20};
const double uup[Oil_End + 1] = {_, 200, 200, 250, 250, 250};

const double      prof = 150;
const double      cost = 5;
const double     limit = 1000;
const double threshold = 3;

IloNumVarArray D;  IloNumVar d(int i, int j) { return D[(i - Oil_Beg)*( Month_End -  Month_Beg + 1) + (j -  Month_Beg)]; }
IloNumVarArray B;  IloNumVar b(int i, int j) { return B[(i - Oil_Beg)*( Month_End -  Month_Beg + 1) + (j -  Month_Beg)]; }
IloNumVarArray U;  IloNumVar u(int i, int j) { return U[(i - Oil_Beg)*( Month_End -  Month_Beg + 1) + (j -  Month_Beg)]; }
IloNumVarArray S;  IloNumVar s(int i, int j) { return S[(i - Oil_Beg)*(XMonth_End - XMonth_Beg + 1) + (j - XMonth_Beg)]; }
IloNumVarArray P;  IloNumVar p(int i)        { return P[i - Month_Beg]; }                                                 

int main () {

  IloEnv             env;
  IloModel     model(env);

  D = IloNumVarArray(env, (Oil_End - Oil_Beg + 1)*( Month_End -  Month_Beg + 1), 0, 1, ILOBOOL);
  B = IloNumVarArray(env, (Oil_End - Oil_Beg + 1)*( Month_End -  Month_Beg + 1), 0, IloInfinity);
  U = IloNumVarArray(env, (Oil_End - Oil_Beg + 1)*( Month_End -  Month_Beg + 1), 0, IloInfinity);
  S = IloNumVarArray(env, (Oil_End - Oil_Beg + 1)*(XMonth_End - XMonth_Beg + 1), 0, limit);
  P = IloNumVarArray(env, Month_End -  Month_Beg + 1, 0, IloInfinity);

  for (int o = Oil_Beg; o <= Oil_End; ++o) {
    model.add( s(o, 0) == stg);
    model.add( s(o, 6) == stg);
  }

  for (int o = Oil_Beg; o <= Oil_End; ++o)
    for (int m = Month_Beg; m <= Month_End; ++m)
      model.add(s(o, m-1) + b(o, m) - u(o, m) - s(o, m) == 0);

  for (int m = Month_Beg; m <= Month_End; ++m) {

    IloExpr expr1(env);
    for (int o = Oil_Beg; o <= Oil_End; ++o) expr1 += u(o, m);
    model.add(expr1 == p(m));
    expr1.end();

    IloExpr expr2(env);
    for (int o = Oil_Beg; o <= Oil_End; ++o) expr2 += h[o] * u(o, m);
    model.add(expr2 <= hub*p(m));
    model.add(expr2 >= hlb*p(m));
    expr2.end();

    IloExpr expr3(env);
    for (int o = Veg_Beg; o <= Veg_End; ++o) expr3 += u(o, m);
    model.add( expr3 <= uup[Veg_Beg]);
    expr3.end();

    IloExpr expr4(env);
    for (int o = Nvg_Beg; o <= Nvg_End; ++o) expr4 += u(o, m);
    model.add( expr4 <= uup[Nvg_Beg]);
    expr4.end();
  }

  for (int m = Month_Beg; m <= Month_End; ++m) {
    model.add( d(1, m) <= d(5, m) );
    model.add( d(2, m) <= d(5, m) );
  }

  for (int m = Month_Beg; m <= Month_End; ++m) {
    IloExpr expr6(env);
    for (int o = Oil_Beg; o <= Oil_End; ++o) expr6 += d(o, m);
    model.add( expr6 <= threshold);
    expr6.end();    
  }

  for (int o = Oil_Beg; o <= Oil_End; ++o)
    for (int m = Month_Beg; m <= Month_End; ++m) {
      // d(o, m) = 0 --> u(o, m) <= 0
      // u(o, m) <= uup[o]
      model.add( u(o, m) <= uup[o] * d(o, m) );

      // d(o, m) = 1 --> u(o, m) >= ulb[o]
      // u(o, m) >= 0
      model.add( u(o, m) >= ulb[o] * d(o, m) );
    }

  IloExpr obj(env);

  for (int m = Month_Beg; m <= Month_End; ++m)
    obj += prof * p(m);

  for (int o = Oil_Beg; o <= Oil_End; ++o)
    for (int m = Month_Beg; m <= Month_End; ++m)
      obj -= cost * s(o, m) + price[m][o] * b(o, m);

  model.add(IloMaximize(env, obj));
  obj.end();

  IloCplex cplex(model);
  cplex.solve();
  cout << cplex.getObjValue() << endl;
  env.end();
}
