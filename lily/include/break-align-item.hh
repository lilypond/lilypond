/*
  break-align-item.hh -- declare Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_ALIGN_ITEM_HH
#define BREAK_ALIGN_ITEM_HH

#include "item.hh"

class Break_align_interface
{
public:
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  static void do_alignment (Grob*);
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  static void add_element (Grob*me, Grob*add);
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (self_align_callback, (SCM element, SCM axis));
};
#endif // BREAK_ALIGN_ITEM_HH
