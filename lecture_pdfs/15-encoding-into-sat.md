## At most one constraints (AMO)

x_0 + ... + x_{n-1} \leq 1

**quadratic encoding**:

- Variables are the same
- clauses: for each pair i,j,  \bar{x_i} \/ \bar{x_j}
           you need perm(n,2) = O(n^2) pairs

**logarithmic encoding**:

- Uses extra variables

We want to express this:

x0 + x1 + x2 + x3 <= 1
iteger auxiliary variable y in {0,1,2,3}

x0 = 1 -> y = 0
x1 = 1 -> y = 1
x2 = 1 -> y = 2
x3 = 1 -> y = 3

We can't encode y = {0,1,2,3}. But we can use a binary representation of y = y0,y1

(recall: F -> G  =======> \neg F \/ G)

x0 = 1 -> \neg y0 ^ \neq y1 (y = 0)
x1 = 1 -> \neg y0 ^ y1 (y = 1)
x2 = 1 -> y0 ^ \neg y1 (y = 2)
x3 = 1 -> y0 ^ y1 (y = 3)

- log(n) new variables
- n*log(n) new clauses

**Heule encoding**

- (base case) If n \leq 3, the encoding is the quadratic encoding
- If n > 4, introduce an auxiliary variable y and encode (recursively)

    x0+x1+y =< 1
    x2+..+xn + \bar{y} =< 1

- Requires O(n) new variables, O(n) clauses.

**Other encodings exists**

### Consistency and Arc-Consistency

- The encoding is consistent if whenever M is partial assignment inconsistent wrt C (i.e. can't be extended to a solution of C), unit propagation leads to conflict.

The encoding is arc-consistent if

  - it is consistent and,
  - unit propagation discard arc-inconsistent values (i.e. values without a support)

See example on Consistency and Arc-Consistency.

Unit propagation is very efficient in SAT Solvers.

## Cardinality Constraint

x1 + ... + xn >< k

>< \in {=<, <, =>, >, =}

- AMO are a particular case of cardinality constraint, where k = 1 and >< is (=<)

Let's assume >< is <

**Naive encoding**

- Variables x1,...,xn
- Clauses for all 1 <= i1 < i2 < ... < ik =< n

    \neg{x_i1} \/ \neg{x_i2} \/ ... \/ \neg{x_ik}

- This generate bin{n}{k} clauses!

**Adders encoding**

See slides

**Sorting Networks encoding**

Pass x_1,...,x_n (these are bits) as inputs to a circuit that sorts n bits.

Let y_1,...,y_n be the outputs of this circuit.

Suppose the output is y0,y1,y2,y3 (decreasing sorted). If y2 is true, then y0 and y1 are also true. Then x0 + x1 + x2 + x3 >= 3

Suppose the output is y0,y1,y2,y3 (decreasing sorted). If y1 is false, then y2 and y3 are also false. Then x0 + x1 + x2 + x3 <= 1

Once you have the output of the network, you only need to add the clauses to this output (which is easy to encode).
