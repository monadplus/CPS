#include <cassert>
#include <cstdlib>
#include <fstream>
#include <vector>
#include <algorithm>
#include <ilcplex/ilocplex.h>

ILOSTLBEGIN

bool cmp(const pair<int,int>& b1, const pair<int,int>& b2) {
  return (b1.first * b1.second) > (b2.first * b2.second);
}

int main(int argc, char* argv[]) {

  IloEnv env;

  try {
    if (argc != 1) return -1;
    IloInt w, n; // width and total number of boxes
    IloInt l = 0; // max length
    cin >> w >> n;
    cout << w << " " << n << endl;
    vector<pair<int,int>> boxes(n);
    int k = 0;
    while (k < n) {
      int m, width, height;
      cin >> m >> width >> height;
      cout << m << "   " << width << " " << height << endl;
      for (int i = 0; i < m; ++i) {
        boxes[k+i] = pair<int,int>(width,height);
        l += max(width,height);
      }
      k += m;
    }

    // try placing the biggest boxes first
    sort(boxes.begin(), boxes.end(), cmp);

    IloModel model(env);

    IloNumVarArray x_tl  = IloNumVarArray(env, boxes.size(), 0, w-1);
    IloNumVarArray y_tl  = IloNumVarArray(env, boxes.size(), 0, l-1);
    // nb. bottom is no longer required.

    // Symmetry
    x_tl[0].setBounds(0,0);
    y_tl[0].setBounds(0,0);

    IloNumVarArray width  = IloNumVarArray(env, boxes.size(), 0, w);
    IloNumVarArray height = IloNumVarArray(env, boxes.size(), 0, l-1);

    IloNumVar length(env, 0, l-1);

    IloInt i;
    IloInt j;

    for(i = 0; i < boxes.size(); i++) {
      IloInt b_width  = boxes[i].first;
      IloInt b_height = boxes[i].second;

      // length
      model.add(length >= y_tl[i] + height[i] - 1);

      // x bounds
      model.add(x_tl[i] <= w - width[i]);

      // width & height
      if (b_width == b_height) {
        width[i].setBounds(b_width, b_width);
        height[i].setBounds(b_height, b_height);
      }
      else {
        model.add(width[i] == b_width || width[i] == b_height);
        model.add(height[i] == b_width || height[i] == b_height);
        model.add(width[i] != height[i]);
      }

      // overlapping
      for (j = i+1; j < boxes.size(); j++) {
        model.add((x_tl[j]+width[j] <= x_tl[i]) || (x_tl[j] >= x_tl[i]+width[i]) || (y_tl[j]+height[j] <= y_tl[i]) || (y_tl[j] >= y_tl[i]+height[i]));
        //model.add( (x_tl[i]+width[i] <= x_tl[j] || x_tl[i] >= x_tl[j]+width[j])
                      //&&
                        //(y_tl[i]+height[i] <= y_tl[j] || y_tl[i] >= y_tl[j]+height[j])
                 //);
      }
    }

    // Objective Function
    model.add(IloMinimize(env, length));

    IloCplex cplex(model);

    if (cplex.solve()) {
       cout << "Solution status: " << cplex.getStatus() << endl;
       cout << " Minimum length: " << (cplex.getObjValue() + 1) << endl;

       // TODO output
       // TODO output
       // TODO output
    }
    else {
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
