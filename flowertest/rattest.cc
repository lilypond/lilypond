#include <iostream.h>
#include "rational.hh"
#include "flower-test.hh"
#include "string.hh"


#define EXP_PRINT(a) #a << ": " << (a) << "\n"

void
rattest ()
{
  Rational r (1,4);
  Rational q(1,2);
  Rational s(6,4);
  Rational i;
  i.set_infinite (1);

  cout << r << " " << s << " " << q << "\n";
  cout << r + q << " " << (s * q + s) << " " << (q / r) << "\n";
  cout << i;
  cout << "inf * r" << i * r << "inf * inf " << i * i << "inf + r" << i + r;
  cout << EXP_PRINT(-i);
  cout << EXP_PRINT(i >? -i) << EXP_PRINT(i >? r);
  cout << EXP_PRINT(i <? r) ;
  Rational one(1);
  cout << EXP_PRINT(one/Rational (4));
  cout << EXP_PRINT(one + one/Rational (4));
  Rational nul (0,1);
  Rational kwart (1,4);
  nul += kwart;
  cout << EXP_PRINT(nul);
  nul -= Rational (2) * kwart;
  cout << EXP_PRINT(nul);

  cout << EXP_PRINT(Rational (1,128)  + Rational (1919,128));

}

ADD_TEST (rattest);
