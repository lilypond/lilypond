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
  Implement bow specific bezier curve
 */
class Bezier_bow
{
public:
  Bezier_bow (Paper_def* paper_l);

  /**
   Calculate bezier curve for bow from bow paratime_signatures.
   */
  void blow_fit ();
  void calc ();
  Real calc_f (Real height);
  void calc_bezier ();
  bool calc_clipping ();
  void calc_controls ();
  void check_sanity () const;
  void calc_default (Real h);
  void calc_return (Real begin_alpha, Real end_alpha);
  void calc_tangent_controls ();
  bool check_fit_b () const;
  Real check_fit_f () const;
  void set (Array<Offset> points, Direction dir);
  void transform ();
  void transform_back ();

  Array<Offset> encompass_;

  Paper_def* paper_l_;
  Direction dir_;
  Real alpha_;
  Offset origin_;


  Bezier curve_;
  Bezier return_;
};


#endif /* BEZIER_BOW_HH */

