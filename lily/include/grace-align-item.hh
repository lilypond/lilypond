/*   
  grace-align-item.hh -- declare Grace_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_ALIGN_ITEM_HH
#define GRACE_ALIGN_ITEM_HH

#include "note-head-side.hh"
#include "axis-align-item.hh"

class Grace_align_item : public Axis_align_item, public Note_head_side
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  Grace_align_item ();
protected:
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing ();
};
#endif /* GRACE_ALIGN_ITEM_HH */

