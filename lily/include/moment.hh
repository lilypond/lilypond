/*   
  moment.hh -- declare Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MOMENT_HH
#define MOMENT_HH

#include "smobs.hh"
#include "rational.hh"

/**
   Rationals with glue for Guilification;

   FIXME: remove self_scm_ and then remove this class */
class Moment : public Rational
{
  DECLARE_SIMPLE_SMOBS (Moment,);
public:
  Moment () { }
  Moment (int m) : Rational (m) { }
  Moment (int m, int n) : Rational (m,n) { }
  Moment (Rational m) : Rational (m) { }

  /*
    Deliver a copy of THIS as a smobified SCM
   */
  SCM smobbed_copy () const; 
};


Moment * unsmob_moment (SCM);

#if 0
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, / );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, * );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, + );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, - );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, % );
INSTANTIATE_COMPARE (Moment const&, Rational::compare);
#endif

#endif /* MOMENT_HH */

