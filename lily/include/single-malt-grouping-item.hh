/*   
  single-malt-grouping-item.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  VIRTUAL_COPY_CONS(Score_element);
  Link_array<Item> item_l_arr_;
public:
  Single_malt_grouping_item ();
  Interval my_width () const;
  void add_item (Item*);
  
protected:
  virtual  void do_substitute_dependency (Score_element*, Score_element*);
  virtual void do_print () const;
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

