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
 */
struct Moment : public Rational
{
  Moment () { self_scm_ = SCM_EOL; }
  Moment (int m) : Rational (m) {self_scm_ = SCM_EOL; }
  Moment (int m, int n) : Rational (m,n) {self_scm_ = SCM_EOL; }
  Moment (Rational m) : Rational (m) {self_scm_ = SCM_EOL; }
  ~Moment ();
  
  DECLARE_SMOBS;
};


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

