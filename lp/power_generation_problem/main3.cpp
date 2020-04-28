#include <ilcplex/ilocplex.h>

ILOSTLBEGIN

typedef IloArray<IloNumVarArray> NumVarMatrix;
typedef IloArray<IloNumArray>    NumMatrix;

typedef enum { t1, t2, t3 } Types;

const IloInt nTypes = 3;
const IloInt nPeriods = 5;

int main() {

  IloEnv env;
  IloModel model(env);

  try {
    NumVarMatrix   n(env, nTypes); // Number of generators of type i at period j.
    NumVarMatrix   s(env, nTypes); // Number of generators of type i started at period j
    NumVarMatrix   x(env, nTypes); // Total output rate of generators of type i at period j

    IloNumArray gen = IloNumArray(env, nTypes, 12, 10, 5); // Upper bound of generators of type i.

		int i; // Types
		int j; // Periods

    for (i = 0; i < nTypes; i++) {
       n[i] = IloNumVarArray(env, nPeriods, 0, gen[i]);
       s[i] = IloNumVarArray(env, nPeriods, 0, gen[i]);
       x[i] = IloNumVarArray(env, nPeriods, 0, IloInfinity);
    }

    IloNumArray D = IloNumArray(env, nPeriods, 15000,30000,25000,40000,27000);  // Minimum output per period.

    // Demand for each period
    for (j = 0; j < nPeriods; j++) {
      IloExpr sum;
      for (i = 0; i < nTypes; i++) {
        sum += x[i][j];
      }
      model.add(sum >= D[j]);
    }

    IloNumArray m = IloNumArray(env, nTypes, 850, 1250, 1500);  // Minimum output per generator.
    IloNumArray M = IloNumArray(env, nTypes, 2000, 1750, 4000);  // Maximum output per generator.

    // Limits the lower and upper bound of the output generated for each type.
    for (i = 0; i < nTypes; i++) {
      for (j = 0; j < nPeriods; j++) {
        model.add(n[i][j]*m[i] <= x[i][j]);
        model.add(x[i][j] <= n[i][j]*M[i]);
      }
    }

    // Sudden increase in load of up to 15%
    for (j = 0; j < nPeriods; j++) {
      IloExpr sum;
      for (i = 0; i < nTypes; i++) {
        sum += M[i]*n[i][j]; // You can't add new generators but you can increase the power up to M
      }
      model.add(sum >= 1.15*D[j]);
    }

    // The number of generators in period j must be equal to the number started generators in period j - the number of generators in period (j-1)
    for (i = 0; i < nTypes; i++) {
      for (j = 0; j < nPeriods; j++) {
        model.add(s[i][j] >= (n[i][j] - n[i][(j-1)%5]));
      }
    }

    // Objective function

    IloExpr cost(env);

    NumMatrix E(env, nTypes); // Cost min per type and period
    NumMatrix C(env, nTypes); // Cost above min per type and period
    IloNumArray F = IloNumArray(env, nTypes, 2000, 1000, 500); //Cost of startup per type.

    IloNumArray c_at_min    = IloNumArray(env, nTypes, 1000, 2600, 3000);
    IloNumArray c_above_min = IloNumArray(env, nTypes, 2.0, 1.3, 3.0);

    IloNumArray P = IloNumArray(env, nPeriods, 6,3,6,3,6); // Hours per period.

    for (i = 0; i < nTypes; ++i) {
      // TODO P*c_above_min[i]
      C[i] = IloNumArray(env, nPeriods, 6*c_above_min[i], 3*c_above_min[i], 6*c_above_min[i], 3*c_above_min[i], 6*c_above_min[i]);
      E[i] = IloNumArray(env, nPeriods, 6*c_at_min[i], 3*c_at_min[i], 6*c_at_min[i], 6*c_at_min[i], 6*c_at_min[i]);
    }

    for (i = 0; i < nTypes; i++) {
      for (j = 0; j < nPeriods; j++) {
        cost += ( C[i][j] * (x[i][j] - (m[i]*n[i][j]))
                + E[i][j] * n[i][j]
                + F[i] * s[i][j]
                );
      }
    }

    model.add(IloMinimize(env, cost));

    // ===========================================
    // ===========================================

    IloCplex cplex(model);

    if (cplex.solve()) {
       cout << "Solution status: " << cplex.getStatus() << endl;
       cout << "Minimum cost = " << cplex.getObjValue() << "(hint: 988540)" << endl;
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
