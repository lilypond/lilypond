/*   
  single-malt-grouping-item.hh -- declare Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "lily-proto.hh"

/** Calc dimensions for the Separating_group_spanner; this has to be
   an item to get dependencies correct.  It can't be an element_group
   since these usually are in a different X_group

   Properties:


   elements -- list of items.
   
   no-spacing-rods -- read from elements: boolean that makes Separation_item ignore
     this item
   
*/
struct Separation_item
{
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static Interval my_width (Score_element*) ;
  static void add_item (Score_element*,Item*);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

