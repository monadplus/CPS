Finding a CNF/DNF equivalent formula is EXP

Tseitin Transformation: used in SAT solvers (work in formulas in CNF).
Can be computed in linear time in the size of F

Tseitin transformation results in a CNF but they are **not** equivalent. But it has nice properties:

  - It is equisatisfiable to F
  - Any model of CNF projected to the variables in F gives a model of F
  - Any model of F can be completed to a model of the CNF

Once you have a model for F, to translate it to the CNF, it just as easy as keeping the original meaning. For example, in the example of slide 15/18. e2 = p /\ q, and we assigned p=q=r=1, then e2 = 1.

CNF can solve efficiently SAT solvers:

Resolution rule: mechanism to generate new clauses from previous clauses (reductions). It is easy to see that p V C  \not{p} V D    \equiv   C v D

Res(S) = closure of set of clauses S under resolution = apply resolution recursively.

Properties of closures under resolution:
  - Res(S) only contains logical consequences.
  - if S is unsatisfiable, then \square \in Res(S).
       Means that after n resolutions you end up with an empty clause, F is unsat.

SAT do not use Res() because they are slow.
