/*   
  grace-align-item.hh -- declare Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ALIGN_ITEM_HH
#define GRACE_ALIGN_ITEM_HH


#include "lily-guile.hh"
#include "lily-proto.hh"

class Grace_align_item
{
public:
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK(before_line_breaking, (SCM ));
};
#endif /* GRACE_ALIGN_ITEM_HH */

