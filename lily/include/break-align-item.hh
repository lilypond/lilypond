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

class Break_align_item
{
public:
  static SCM before_line_breaking (SCM);
  static void do_alignment (Score_element*);
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static void add_element (Score_element*me, Score_element*add);
  static Real alignment_callback (Score_element*, Axis);
};
#endif // BREAK_ALIGN_ITEM_HH
