/*   
  single-malt-grouping-item.hh -- declare Single_malt_grouping_item
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "item.hh"

/** Calc dimensions for the Separating_group_spanner; this has to be
   an item to get dependencies correct.  It can't be an element_group
   since these usually are in a different X_group

   It's 1:30 am.  Naming suggestions appreciated.

   Properties:


   elements -- list of items.
   
   no-spacing-rods -- read from elements: boolean that makes Single_malt_grouping_item ignore
     this item
   
*/
class Single_malt_grouping_item : public Item
{
  VIRTUAL_COPY_CONS(Score_element);
public:
  Single_malt_grouping_item (SCM);
  Interval my_width () const;
  void add_item (Item*);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

