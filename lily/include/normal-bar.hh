/*   
  normal-bar.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#ifndef NORMAL_BAR_HH
#define NORMAL_BAR_HH

#include "bar.hh"

class Normal_bar : public virtual Bar
{
public:
  SCORE_ELEM_CLONE(Normal_bar);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif /* NORMAL_BAR_HH */

