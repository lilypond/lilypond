/*   
  moment.hh -- declare Moment
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  DECLARE_SIMPLE_SMOBS(Moment,);
public:
  Moment () { }
  Moment (int m) : Rational (m) { }
  Moment (int m, int n) : Rational (m,n) { }
  Moment (Rational m) : Rational (m) { }

  /*
    Deliver a copy of THIS as a smobified SCM
   */
  SCM make_scm () const; 
};


Moment * unsmob_moment (SCM);

IMPLEMENT_ARITHMETIC_OPERATOR (Moment, / );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, + );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, * );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, - );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, % );

INSTANTIATE_COMPARE (Moment const&, Rational::compare);


/**
  A really big time-moment.

  Windhoze-suck-suck-suck-suck-suck-thank-you-cygnus

  I get tired of all these incompatibilities.  Let's just assume that
  INT_MAX is really, really, really big.

  Can't we name this Saint_jut_mom (Sintjuttemis ?)  */
  
/* URG ! WE HAVE TWO RATIONAL INFINITIES! */
const Moment infinity_mom = INT_MAX;

#endif /* MOMENT_HH */

