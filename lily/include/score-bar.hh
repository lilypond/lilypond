/*   
  score-bar.hh -- declare Score_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCORE_BAR_HH
#define SCORE_BAR_HH

#include "bar.hh"

/**
  Score_bars are at the start of the line only, and 
  they come before normal bars.
 */
class Score_bar : public virtual Bar {
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Score_bar);
protected:
  void do_pre_processing ();
};

#endif /* SCORE_BAR_HH */

