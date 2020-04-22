#include <ilcplex/ilocplex.h>

ILOSTLBEGIN

typedef IloArray<IloNumVarArray> NumVarMatrix;

#define TYPE_1_N 12
#define TYPE_2_N 10
#define TYPE_3_N 5

#define HOURS 24

/*
 * Note, this version is simplified by not taking into account the possibility to start/close a generator.
 */
int main () {
  IloEnv             env;
  IloModel     model(env);

  NumVarMatrix   mw_1(env, TYPE_1_N);
  NumVarMatrix   mw_2(env, TYPE_2_N);
  NumVarMatrix   mw_3(env, TYPE_3_N);

  // ========== Variables & bounds ==================

  for(int i = 0; i < TYPE_1_N; i++){
    mw_1[i] = IloNumVarArray(env, HOURS);
    for(int j = 0; j < HOURS; j++)
      mw_1[i][j] = IloNumVar(env, 850, 2000, ILOINT);
  }
  for(int i = 0; i < TYPE_2_N; i++){
    mw_2[i] = IloNumVarArray(env, HOURS);
    for(int j = 0; j < HOURS; j++)
      mw_2[i][j] = IloNumVar(env, 1250, 1750, ILOINT);
  }
  for(int i = 0; i < TYPE_3_N; i++){
    mw_3[i] = IloNumVarArray(env, HOURS);
    for(int j = 0; j < HOURS; j++)
      mw_3[i][j] = IloNumVar(env, 1500, 4000, ILOINT);
  }

  // ========== Constraints ==================

  const int UNEXPECTED_INCREASE_LOAD = 1.15;

  for(int i = 0; i < HOURS; i ++){
    IloExpr expr(env);

    for(int j = 0; j < TYPE_1_N; j++) expr += (mw_1[j][i]);
    for(int j = 0; j < TYPE_2_N; j++) expr += (mw_2[j][i]);
    for(int j = 0; j < TYPE_3_N; j++) expr += (mw_3[j][i]);

    if (i < 6)       model.add(expr >= 15000*UNEXPECTED_INCREASE_LOAD);
    else if (i < 9)  model.add(expr >= 30000*UNEXPECTED_INCREASE_LOAD);
    else if (i < 15) model.add(expr >= 25000*UNEXPECTED_INCREASE_LOAD);
    else if (i < 18) model.add(expr >= 40000*UNEXPECTED_INCREASE_LOAD);
    else             model.add(expr >= 27000*UNEXPECTED_INCREASE_LOAD);

    expr.end();
  }

  // ========== Objective Function ==================

  IloExpr objExp(env);
  IloExpr startup_cost(env);

  startup_cost += ( TYPE_1_N * 2000
                  + TYPE_2_N * 1000
                  + TYPE_3_N * 500
                  );


  for(int i = 0; i < TYPE_1_N; i++){
    for(int j = 0; j < HOURS; j++){
      objExp += 1000 + 2*(mw_1[i][j]-850);
    }
  }
  for(int i = 0; i < TYPE_2_N; i++){
    for(int j = 0; j < HOURS; j++){
      objExp += 1250 + 1.3*(mw_2[i][j]-1250);
    }
  }
  for(int i = 0; i < TYPE_3_N; i++){
    for(int j = 0; j < HOURS; j++){
      objExp += 1500 + 3*(mw_3[i][j]-1500);
    }
  }
  model.add(IloMinimize(env, startup_cost + objExp));

  // ========== Main ==================

  try {

    IloCplex cplex(model);

    if (cplex.solve()) {
      cplex.exportModel("model.lp");

      cout << "Min: " << cplex.getObjValue() << " (should be 988540)" << endl;

      cout << "=== Gen type 1 ===" << endl;
      for(int i = 0; i < TYPE_1_N; ++i) {
        IloNumArray v (env);
        cplex.getValues(mw_1[i], v);
        cout << i << "-th generator:" << endl;
        cout << i << "\t" << v << endl;
      }
      cout << "=== Gen type 2 ===" << endl;
      for(int i = 0; i < TYPE_2_N; ++i) {
        IloNumArray v (env);
        cplex.getValues(mw_2[i], v);
        cout << i << "-th generator:" << endl;
        cout << i << "\t" << v << endl;
      }
      cout << "=== Gen type 3 ===" << endl;
      for(int i = 0; i < TYPE_3_N; ++i) {
        IloNumArray v (env);
        cplex.getValues(mw_3[i], v);
        cout << i << "-th generator:" << endl;
        cout << i << "\t" << v << endl;
      }

    } else {
       cout << " No solution found" << endl;
    }
  }
  catch (IloException& ex) {
     cerr << "Error: " << ex << endl;
  }
  catch (...) {
     cerr << "Error" << endl;
  }
  env.end();
  return 0;
}
