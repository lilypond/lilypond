// vim:sw=2 makeprg=g++\ -g\ bow.cc\ -o\ bow
#include <iostream.h>
#define PT
// #define STAFFHEIGHT 16.0
#define STAFFHEIGHT 20.0

#define UP 1
#define DOWN (-1)

// mmm
#define STANDALONE

#include <math.h>

typedef void *Paper_def;

bool experimental_features_global_b = true;

#include "misc.cc"

int
main ()
{
  //cout.unsetf(ios::scientific);
  cout.setf(ios::fixed);

  Array<Real> a;
  a.push (3.4);
  a.push (3.9);
  Interval iv;

  iv = quantise_iv (a, 4, 3.5);
  cout << "iv: " << iv.min () << ", " << iv.max () << endl;

  iv = quantise_iv (a, 4, -1.5);
  cout << "iv: " << iv.min () << ", " << iv.max () << endl;

  iv = quantise_iv (a, 4, -7.5);
  cout << "iv: " << iv.min () << ", " << iv.max () << endl;

  iv = quantise_iv (a, 4, 3.4);
  cout << "iv: " << iv.min () << ", " << iv.max () << endl;

  iv = quantise_iv (a, 4, 3.9);
  cout << "iv: " << iv.min () << ", " << iv.max () << endl;

  cout << "\\end" << endl;

  return 0;
}

