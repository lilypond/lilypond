/*   
  axis-align-item.hh -- declare Axis_align_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AXIS_ALIGN_ITEM_HH
#define AXIS_ALIGN_ITEM_HH

#include "align-element.hh"
#include "axis-group-item.hh"

class Axis_align_item : public virtual Align_element,
			public virtual Axis_group_item
{
public:
  Axis_align_item ();
protected:
  virtual void do_print ()const;

};
#endif /* AXIS_ALIGN_ITEM_HH */

