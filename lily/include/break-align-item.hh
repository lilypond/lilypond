/*
  break-align-item.hh -- declare Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_ALIGN_ITEM_HH
#define BREAK_ALIGN_ITEM_HH

#include "axis-align-item.hh"

/// align breakable items (clef, bar, etc.)
class Break_align_item : public Axis_align_item {


protected:
  virtual void before_line_breaking ();
public:
  Break_align_item ();
  VIRTUAL_COPY_CONS(Score_element);
};
#endif // BREAK_ALIGN_ITEM_HH
