/*   
  output-property.hh -- declare Output_property
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef OUTPUT_PROPERTY_HH
#define OUTPUT_PROPERTY_HH

#include "music.hh"
#include "protected-scm.hh"

class Output_property : public Music
{
public:
  Output_property(SCM, SCM, SCM);

  /**
    relevant stuff: the predicate, the symbol, the value
   */
  Protected_scm pred_sym_val_list_;
};

#endif /* OUTPUT_PROPERTY_HH */
