/*
  slur-bezier-bow.hh -- declare Slur_bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 2000  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SLUR_BEZIER_BOW_HH
#define SLUR_BEZIER_BOW_HH

#include "bezier-bow.hh"

class Slur_bezier_bow : public Bezier_bow
{
public:
  Slur_bezier_bow (Array<Offset> encompass, Direction dir);
  Array<Real> area_x_gradients_array (Real area);
  void blow_fit ();
  Real enclosed_area_f () const;
  Real fit_factor () const;
  void minimise_enclosed_area (Paper_def* paper_l, Real default_height);
};
 
#endif /* SLUR_BEZIER_BOW_HH */
