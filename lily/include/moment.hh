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
   Musical timing (Main-timing, grace-timing) with glue for
   Guilification;
*/
class Moment
{
  DECLARE_SIMPLE_SMOBS (Moment,);
public:
  Moment ();
  Moment (int m);
  Moment (int m, int n);

  Moment (Rational m);

  Moment operator - () const;
  
  void operator += (Moment const &m);
  void operator -= (Moment const &m);  

  void operator *= (Moment const &m);
  void operator /= (Moment const &m);  

  Rational main_part_;
  Rational grace_part_;

  void set_infinite (int k);
  
  operator bool ();
  int den () const;
  int num () const;
  /*
    Deliver a copy of THIS as a smobified SCM
   */
  SCM smobbed_copy () const;
  String str () const;
  static int compare (Moment const&, Moment const&);
};
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, + );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, - );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, / );
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, * );


Moment * unsmob_moment (SCM);
int compare (Moment const&,Moment const&);
INSTANTIATE_COMPARE (Moment const&, Moment::compare);

#if 0
IMPLEMENT_ARITHMETIC_OPERATOR (Moment, % );
#endif

#endif /* MOMENT_HH */

