/*   
  score-element-callback.hh -- declare  Score_element callbacks
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCORE_ELEMENT_CALLBACK_HH
#define SCORE_ELEMENT_CALLBACK_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

typedef SCM (* Score_element_callback) (Score_element * ,  SCM extra_params);
SCM smobify_callback (Score_element_callback cb);


#endif /* SCORE_ELEMENT_CALLBACK_HH */

