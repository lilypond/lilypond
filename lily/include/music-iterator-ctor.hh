/*   
  grob-callback.hh -- declare Grob callbacks
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCORE_ELEMENT_CALLBACK_HH
#define SCORE_ELEMENT_CALLBACK_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

typedef void * (*Cxx_function) (SCM param);
Cxx_function unsmob_cxx_function (SCM x);
SCM smobify_cxx_function (Cxx_function cb);


#endif /* SCORE_ELEMENT_CALLBACK_HH */

