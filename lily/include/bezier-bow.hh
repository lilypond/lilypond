/*   
  bezier-bow.hh -- declare Bezier_bow
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BEZIER_BOW_HH
#define BEZIER_BOW_HH

#include "bezier.hh"
#include "lily-proto.hh"


/**
  Implement bow specific bezier curve. Calculate bezier curve for bow
  from bow paratime_signatures.  */
class Bezier_bow
{
  Bezier curve_;
  Array<Offset> encompass_;

  void blow_fit ();
  void calc_default (Real h);
  void to_canonic_form ();
  void calc_tangent_controls ();
  Real fit_factor () const;


  Paper_def* paper_l_;
  Direction dir_;
  Real alpha_;
  Offset origin_;
public:
  Real  rc_factor_,
    height_limit_,
    ratio_;


  Bezier_bow (Array<Offset> points, Direction dir);
  void calculate ();
  Bezier get_curve () const;
};


#endif /* BEZIER_BOW_HH */

