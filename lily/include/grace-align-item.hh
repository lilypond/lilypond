/*   
  grace-align-item.hh -- declare Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ALIGN_ITEM_HH
#define GRACE_ALIGN_ITEM_HH


#include "item.hh"

class Grace_align_item : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  Grace_align_item (SCM);
protected:
  virtual void do_add_processing ();
  virtual void before_line_breaking ();
};
#endif /* GRACE_ALIGN_ITEM_HH */

