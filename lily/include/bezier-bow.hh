/*   
  bezier-bow.hh -- declare Bezier_bow
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BEZIER_BOW_HH
#define BEZIER_BOW_HH

#include "bezier.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

/**
  Implement bow specific bezier curve. Calculate bezier curve for bow
  from bow paratime_signatures.  */
class Bezier_bow
{
public:
  Bezier_bow (Array<Offset> encompass, Direction dir);

  Bezier get_bezier () const;
  Bezier get_default_bezier (Real h_inf, Real r_0) const;
  Real get_default_height (Real h_inf, Real r_0, Real length) const;
  void set_default_bezier (Real h_inf, Real r_0);

  /**
     The canonical bezier.
   */
  Bezier curve_;

protected:
  Array<Offset> encompass_;

private:
  void to_canonical_form ();
  Direction dir_;
  Real alpha_;
  Offset origin_;
};


#endif /* BEZIER_BOW_HH */

