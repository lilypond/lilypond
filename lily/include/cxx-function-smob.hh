/*   
     cxx-function-smob.hh --  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CXX_FUNCT_SMOB_HH
#define CXX_FUNCT_SMOB_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

typedef void * (*Cxx_function) (SCM param);
Cxx_function unsmob_cxx_function (SCM x);
SCM smobify_cxx_function (Cxx_function cb);


#endif /* CXX_FUNCT_SMOB_HH */

