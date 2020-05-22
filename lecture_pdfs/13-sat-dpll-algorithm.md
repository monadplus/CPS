## Designing an efficient SAT solver

Refution: proof of unsatisfiability

resolution-based(explained previous slides):
  - straightforward to give refutation.
  - not clear how to get a model (it is possible)
  - consume a lot of memory

DPLL-based:
  - straighforward to obtain model
  - not direct to give refutation.

## DPLL Algorithm

DPLL tries to build assignment M st M |= F

assignments = M  (sequence of literals, example p\bar{q}r is M(p) = 1, M(q) = 0, M(r) = 1). Order in M matters. No literal appears twice. No contradictory literals.

decision literals: q^d (arbitrary assigned, you still need to try the negation). You start setting it to true.

**states**: pairs of M || F (M is a partial assignment, and F is a CNF)
**Transitions systems** (transition rules): move from one state to another. Example of step: M||F ==> M'||F

Start from empty assignment, add assignemnts using the transition rule.

### Rules

Decide:

   M || F ===> M l^d || F                  if  l or \bar{l} occurs in F
                                               l is undefined in M

UnitProp (there is a clause of the form C V l):

  M || F, C v l ===> M l || F, C v l       if M |= \neg C (all literals of C evaluate to false on M)
                                              l is undefined in M

Backtrack:

M l^d N || F, C ===> M \bar{l} || F, C    if  M l^d N |= \neg C
                                              N contains no decision literals

Fail:

M || F, C  ===> fail                      if M |= \neg C
                                             M contains no decision literals


Apply does rules until fail or no more rule can be applied.
