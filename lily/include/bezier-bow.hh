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



Bezier slur_shape (Real width, Real height_limit,
		   Real height_proportion);
Real slur_height (Real width, Real height_limit, Real height_proportion); 


#endif /* BEZIER_BOW_HH */

