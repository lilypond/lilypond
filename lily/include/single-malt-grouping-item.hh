/*   
  single-malt-grouping-item.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "item.hh"

/** Calc dimensions for the Separating_group_spanner; this has to be
   an itme to get dependencies correct.  It can't be an element_group
   since these usually are in a different X_group 

   It's 1 30 am.  Naming suggestions appreciated.  */
class Single_malt_grouping_item : public Item
{
  SCORE_ELEM_CLONE (Single_malt_grouping_item);
  Link_array<Item> item_l_arr_;
public:
  Single_malt_grouping_item ();
  Interval do_width () const;
  void add (Item*);
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual  void do_substitute_dependency (Score_elem*, Score_elem*);

};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

