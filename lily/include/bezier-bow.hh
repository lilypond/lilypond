/*   
  bezier-bow.hh -- declare Bezier_bow
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BEZIER_BOW_HH
#define BEZIER_BOW_HH

#include "bezier.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"



Bezier slur_shape (Real width, Real height_limit,
		   Real height_proportion);
Real slur_height (Real width, Real height_limit, Real height_proportion); 
void get_slur_indent_height (Real * indent, Real *height, Real width, Real h_inf, Real r_0);
#endif /* BEZIER_BOW_HH */

