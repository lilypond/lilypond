/*   
  output-property.hh -- declare Output_property
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef OUTPUT_PROPERTY_HH
#define OUTPUT_PROPERTY_HH

#include "music.hh"
//#include "protected-scm.hh"

/*
  
props:

  relevant stuff: the predicate, the symbol, the value

 */
class Output_property : public Music
{
public:
  VIRTUAL_COPY_CONS (Music);
  Output_property (SCM, SCM, SCM);
};

#endif /* OUTPUT_PROPERTY_HH */

#error
