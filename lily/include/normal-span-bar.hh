/*   
  normal-span-bar.hh -- declare Normal_span_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#ifndef NORMAL_SPAN_BAR_HH
#define NORMAL_SPAN_BAR_HH

#include "normal-bar.hh"
#include "span-bar.hh"

class Normal_span_bar : public Span_bar, public Normal_bar
{
public:
  SCORE_ELEM_CLONE(Normal_span_bar);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif /* NORMAL_SPAN_BAR_HH */

