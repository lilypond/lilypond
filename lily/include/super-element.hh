/*
  super-element.hh -- declare Super_element

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Super_element_HH
#define Super_element_HH

#include "score-element.hh"
/** The toplevel element. The Paper_score contains this element, and any
  element shoud be a dependency for the super element.
  */
class Super_element : public virtual Score_element {
public:
  Super_element();

  void space_processing ();
  void pre_processing();
  void breakable_col_processing();
  void break_processing();
  void post_processing();
  void output_all ();
  void unlink_all ();

protected:
  
};

#endif // Super_element_HH
