/*   
  grace-align-item.hh -- declare Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ALIGN_ITEM_HH
#define GRACE_ALIGN_ITEM_HH


#include "lily-guile.hh"
#include "lily-proto.hh"

/*
  horizontal-space -- amount of space to add after a note (in staff-space)
 */

class Grace_align_item
{
public:
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static SCM before_line_breaking (SCM);
};
#endif /* GRACE_ALIGN_ITEM_HH */

