/*   
  g-script-column.hh -- declare Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "item.hh"

class Script_column : public Item
{
  /**
     Array of objects that are placed by staffsides
   */
  Link_array<Item> staff_sided_item_l_arr_;
public:
  void add_staff_sided (Item*);
protected:
  virtual void do_pre_processing ();
};


#endif /* Script_COLUMN_HH */


