/*   
  single-malt-grouping-item.hh -- declare Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "lily-proto.hh"

struct Separation_item
{
  static bool has_interface (Grob*);
  static Interval my_width (Grob*) ;
  static void add_item (Grob*,Item*);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

