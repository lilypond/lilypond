/*
  super-element.hh -- declare Super_element

  source file of the LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Super_element_HH
#define Super_element_HH

#include "score-element.hh"
/** The toplevel element. The Paper_score contains this element, and any
  element shoud be a dependency for the super element.

  It is the entry point for the "constraint solver"/ dependency
  tracker.  Every XXXX_processing () call traverses the entire
  dependency graph, and calls the appropriate
  Score_element::do_XXX_processing function on each Score_element it encounters.
  

  FIXME: remove this class, to eliminate multiple inheritance. Merge
  with Line_of_score ?  */
class Super_element : public virtual Score_element {
public:
  void space_processing ();
  void pre_processing();
  void breakable_col_processing();
  void post_processing();
  void output_all ();
};

#endif // Super_element_HH
