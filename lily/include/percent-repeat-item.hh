/*   
  percent-repeat-item.hh -- declare Percent_repeat_item_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef PERCENT_REPEAT_ITEM_HH
#define PERCENT_REPEAT_ITEM_HH

#include "grob.hh"

class Percent_repeat_item_interface
{
public:
  DECLARE_SCHEME_CALLBACK (beat_slash, (SCM ));  
  DECLARE_SCHEME_CALLBACK (double_percent, (SCM ));
  static Molecule x_percent (Grob*,int,Real,Real );
  static Molecule brew_slash (Grob*);
};

#endif /* PERCENT_REPEAT_ITEM_HH */

