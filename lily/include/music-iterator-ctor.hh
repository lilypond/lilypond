/*   
  score-element-callback.hh -- declare Score_element callbacks
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCORE_ELEMENT_CALLBACK_HH
#define SCORE_ELEMENT_CALLBACK_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

typedef void * (*Cpp_function) (SCM param);
SCM smobify_cpp_function (Cpp_function cb);


#endif /* SCORE_ELEMENT_CALLBACK_HH */

