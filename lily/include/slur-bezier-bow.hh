/*
  slur-bezier-bow.hh -- declare Slur_bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SLUR_BEZIER_BOW_HH
#define SLUR_BEZIER_BOW_HH

#include "bezier-bow.hh"

class Slur_bezier_bow
{

protected:
  Array<Offset> encompass_;

private:
  void to_canonical_form ();
  Direction dir_;
  Real alpha_;
  Offset origin_;
  Real h_inf_, r_0_;
public:
  /**
     The canonical bezier.
   */
  Bezier curve_;


  Slur_bezier_bow (Array<Offset> encompass, Direction dir,
		   Real hinf, Real r0);
  Bezier get_bezier () const;

  void minimise_enclosed_area (Real beauty, SCM props);
  Real fit_factor () const;
  void blow_fit ();
  Real enclosed_area_f () const;
private:
  Array<Real> area_x_gradients_array (Real area);
};
 
#endif /* SLUR_BEZIER_BOW_HH */
