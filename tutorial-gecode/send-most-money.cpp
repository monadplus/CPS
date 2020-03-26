#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;

class SendMostMoney : public Space {

  protected:
    IntVarArray l;

  public:
    SendMostMoney() : l(*this, 8, 0, 9) {

      IntVar s(l[0]), e(l[1]), n(l[2]), d(l[3]),
             m(l[4]), o(l[5]), t(l[6]), y(l[7]);

      rel(*this, s != 0);

      rel(*this, m != 0);

      distinct(*this, l);

      rel(*this,             1000*s + 100*e + 10*n + d
                           + 1000*m + 100*o + 10*s + t
                == 10000*m + 1000*o + 100*n + 10*e + y);

      branch(*this, l, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }

    SendMostMoney(SendMostMoney& s) : Space(s) {
      l.update(*this, s.l);
    }

    virtual Space* copy() {
      return new SendMostMoney(*this);
    }

    void print() const {
      IntVar e(l[1]), n(l[2]), m(l[4]), o(l[5]), y(l[7]);

      int money = (10000*m.val()+1000*o.val()+100*n.val()+10*e.val()+y.val());

      std::cout << l << "\tmoney -> " << money << std::endl;
    }

    virtual void constrain(const Space& _b) {
      //                         ^^^^^^^^^ new found solution
      const SendMostMoney& b = static_cast<const SendMostMoney&>(_b);

      IntVar e(l[1]), n(l[2]), m(l[4]), o(l[5]), y(l[7]);

      IntVar b_e(b.l[1]), b_n(b.l[2]), b_m(b.l[4]), b_o(b.l[5]), b_y(b.l[7]);

      int b_money = 10000*b_m.val()+1000*b_o.val()+100*b_n.val()+10*b_e.val()+b_y.val();

      rel(*this, (10000*m + 1000*o + 100*n + 10*e + y) > b_money);
    }
};

int main() {

  SendMostMoney* m = new SendMostMoney;

  // Best solution is the last one
  BAB<SendMostMoney> e(m); // Branch-and-Bound

  delete m;

  while (SendMostMoney* s = e.next()) {
    s->print(); delete s;
  }

  return 0;
}
