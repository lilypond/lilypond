/*
  break-align-item.hh -- declare Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_ALIGN_ITEM_HH
#define BREAK_ALIGN_ITEM_HH

#include "item.hh"

/**
   align breakable items (clef, bar, etc.)

   Properties:

   break-align-symbol -- the index in the spacing table (symbol) of
   the to be aligned item.


   TODO: remove this as a class, and make interface.
 */

class Break_align_item : public Item
{
protected:
  virtual void before_line_breaking ();
public:
  Break_align_item (SCM s);
  VIRTUAL_COPY_CONS(Score_element);
};
#endif // BREAK_ALIGN_ITEM_HH
