#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <cassert>
#include <set>
#include <map>

using namespace std;

string col_names[] = {
"blue",
"red",
"yellow",
"green",
"violet",
"white",
"orange",
"brown",
"aliceblue",
"antiquewhite",
"aquamarine",
"azure",
"beige",
"bisque",
"blanchedalmond",
"blueviolet",
"burlywood",
"cadetblue",
"chartreuse",
"chocolate",
"coral",
"cornflowerblue",
"cornsilk",
"crimson",
"cyan",
"darkgoldenrod",
"darkgreen",
"darkkhaki",
"darkolivegreen",
"darkorange",
"darkorchid",
"darksalmon",
"darkseagreen",
"darkslateblue",
"darkslategray",
"darkturquoise",
"darkviolet",
"deeppink",
"deepskyblue",
"dimgray",
"dodgerblue",
"firebrick",
"floralwhite",
"forestgreen",
"gainsboro",
"ghostwhite",
"gold",
"goldenrod",
"gray",
"greenyellow",
"honeydew",
"hotpink",
"indianred",
"indigo",
"ivory",
"khaki",
"lavender",
"lavenderblush",
"lawngreen",
"lemonchiffon",
"lightblue",
"lightcyan",
"lightgoldenrod",
"lightgoldenrodyellow",
"lightgray",
"lightpink",
"lightsalmon",
"lightseagreen",
"lightskyblue",
"lightslateblue",
"lightslategray",
"lightyellow",
"limegreen",
"linen",
"magenta",
"maroon",
"mediumaquamarine",
"mediumblue",
"mediumorchid",
"mediumpurple",
"mediumseagreen",
"mediumslateblue",
"mediumspringgreen",
"mediumturquoise",
"mediumvioletred",
"midnightblue",
"mintcream",
"mistyrose",
"moccasin",
"navajowhite",
"navy",
"navyblue",
"oldlace",
"olivedrab",
"orangered",
"orchid",
"palegoldenrod",
"palegreen",
"paleturquoise",
"palevioletred",
"papayawhip",
"peachpuff",
"peru",
"pink",
"plum",
"powderblue",
"purple",
"rosybrown",
"royalblue",
"saddlebrown",
"salmon",
"sandybrown",
"seagreen",
"seashell",
"sienna",
"skyblue",
"slateblue",
"slategray",
"snow",
"springgreen",
"steelblue",
"tan",
"thistle",
"tomato",
"turquoise",
"violetred",
"wheat",
"whitesmoke",
"yellowgreen"
};

int main(int argc, char** argv) {

  // Write help message.
  if (argc == 1 or 
      (argc == 2 and (string(argv[1]) == "-h" or string(argv[1]) == "--help"))) {
    cout << "Checks that solutions to a graph coloring instance are indeed colorings" << endl;
    cout << "(and draws valid colorings in a PostScript file)" << endl;
    cout << "Usage: " << argv[0] << " <file with graph coloring instance> <file with graph coloring solution>" << endl;
    exit(0);
  }

  // Open input streams.
  assert(argc == 3);
  ifstream in_ins(argv[1]);
  assert(in_ins.good());
  ifstream in_sol(argv[2]);
  assert(in_sol.good());

  // Read problem instance.
  int n, m;
  in_ins >> n >> m;

  // Read solution.
  int n_colors;
  in_sol >> n_colors;

  set<int> colors;
  map<int,int> vertex2color;
  for (int i = 0; i < n; ++i) {
    int u, k;
    in_sol >> u >> k;
    assert(1 <= u and u <= n);
    assert(1 <= k and k <= n_colors);
    vertex2color[u] = k;
    colors.insert(k);
  }

  // Check solution is correct.
  if (colors.size() != n_colors) {
    cout << "Error! Minimum number of colors is said to be " << n_colors
	 << " but coloring uses " << colors.size() << " colors" << endl;
    exit(1);
  }

  vector< pair<int,int> > edges(m);
  for (int i = 0; i < m; ++i) {
    int u, v;
    in_ins >> u >> v;
    edges[i] = pair<int,int>(u,v);
    if (vertex2color[u] == vertex2color[v]) {
      cout << "Error! In edge " << u << " --- " << v
	   << " both vertices are painted with color " << vertex2color[v] << endl;
      exit(1);
    }
  }

  cout << "OK!" << endl;

  string filename = string(argv[2]) + ".tmp";
  ofstream out;
  out.open(filename.c_str());
  out << "graph {" << endl;
  for (int i = 0; i < m; ++i)
    out << edges[i].first << " -- " << edges[i].second << endl;
  for (int j = 0; j < n; ++j)
    out << j+1 << " [style = filled, color = " << col_names[ vertex2color[j+1]] << "];" << endl;

  out << "}" << endl;
  out.close();

  // Requires graphviz package (http://www.graphviz.org)
  system(string("neato -Tps -o " + filename + ".ps " + filename + " && evince " + filename + ".ps &").c_str());
}

