#include <ilcplex/ilocplex.h>

ILOSTLBEGIN

typedef IloArray<IloNumVarArray> NumVarMatrix;
typedef IloArray<IloNumArray>    NumMatrix;

typedef enum { v1, v2, o1, o2, o3 } Product;
const IloInt nbMonths   = 6;
const IloInt nbProducts = 5;

int main() {

  IloEnv env;
  IloModel model(env);

  try {
    NumVarMatrix   use(env, nbMonths);
    NumVarMatrix   buy(env, nbMonths);
    NumVarMatrix   store(env, nbMonths);
    IloInt i, p;
    for (i = 0; i < nbMonths; i++) {
         use[i]   = IloNumVarArray(env, nbProducts, 0, IloInfinity);
         buy[i]   = IloNumVarArray(env, nbProducts, 0, IloInfinity);
         store[i] = IloNumVarArray(env, nbProducts, 0, 1000); // At most 1000 tons of each oil stored.
    }

    // Equivalent to the sum of used products.
    // It's **not necessary** but useful.
    IloNumVarArray produce(env, nbMonths, 0, IloInfinity);

    // It's **not necessary** but useful.
    NumMatrix cost(env, nbMonths);
    cost[0]=IloNumArray(env, nbProducts, 110.0, 120.0, 130.0, 110.0, 115.0);
    cost[1]=IloNumArray(env, nbProducts, 130.0, 130.0, 110.0,  90.0, 115.0);
    cost[2]=IloNumArray(env, nbProducts, 110.0, 140.0, 130.0, 100.0,  95.0);
    cost[3]=IloNumArray(env, nbProducts, 120.0, 110.0, 120.0, 120.0, 125.0);
    cost[4]=IloNumArray(env, nbProducts, 100.0, 120.0, 150.0, 110.0, 105.0);
    cost[5]=IloNumArray(env, nbProducts,  90.0, 100.0, 140.0,  80.0, 135.0);


    // Used for the obj. function
    IloExpr profit(env);

    // For each type of raw oil we must have 500 tons at the end
    for (p = 0; p < nbProducts; p++) {
        store[nbMonths-1][p].setBounds(500, 500);
    }

    for (i = 0; i < nbMonths; i++) {
        // Equivalence
        model.add(produce[i] == IloSum(use[i]));

        // Produced constraint
        model.add(use[i][v1] + use[i][v2] <= 200);
        model.add(use[i][o1] + use[i][o2] + use[i][o3] <= 250);

        if (i == 0) {
          for(p = 0; p < nbProducts; p++)
            model.add(500 + buy[i][p] >= use[i][p] + store[i][p]);
        } else {
          for(p = 0; p < nbProducts; p++)
            model.add(store[i-1][p] + buy[i][p] >= use[i][p] + store[i][p]);
        }

        // 3 <= comp <= 6
        model.add(3*produce[i]
                     <= 8.8*use[i][v1] + 6.1*use[i][v2] + 2.0*use[i][o1] + 4.2*use[i][o2] + 5.0*use[i][o3]);
        model.add(8.8*use[i][v1] + 6.1*use[i][v2] + 2.0*use[i][o1] + 4.2*use[i][o2] + 5.0*use[i][o3]
                     <= 6*produce[i]);

        // The food may never be made up of more than three oils in any month.
        model.add((use[i][v1] == 0) + (use[i][v2] == 0) + (use[i][o1] == 0) + (use[i][o2] == 0) + (use[i][o3] == 0) >= 2);

        // If an oil is used in a month at least 20 must be used.
        for (p = 0; p < nbProducts; p++)
          model.add(use[i][p] == 0 || use[i][p] >= 20);

        // If either of VEG1 or VEG2 is used in a month then OIL 3 must also be used.
        model.add((use[i][v1] == 0 && use[i][v2] == 0) || use[i][o1] >= 1);

        profit += ( 150 * produce[i] - IloScalProd(cost[i], buy[i]) - 5 * IloSum(store[i]));
    }

    // Objective function
    model.add(IloMaximize(env, profit));

    IloCplex cplex(model);

    if (cplex.solve()) {
       cout << "Solution status: " << cplex.getStatus() << endl;
       cout << " Maximum profit = " << cplex.getObjValue() << endl;
       for (IloInt i = 0; i < nbMonths; i++) {
          IloInt p;
          cout << " Month " << i << " " << endl;
          cout << "  . buy   ";
          for (p = 0; p < nbProducts; p++) {
             cout << round(cplex.getValue(buy[i][p])) << "\t ";
          }
          cout << endl;
          cout << "  . use   ";
          for (p = 0; p < nbProducts; p++) {
             cout << round(cplex.getValue(use[i][p])) << "\t ";
          }
          cout << endl;
          cout << "  . store ";
          for (p = 0; p < nbProducts; p++) {
             cout << round(cplex.getValue(store[i][p])) << "\t ";
          }
          cout << endl;
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
