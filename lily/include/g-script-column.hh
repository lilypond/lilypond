/*   
  g-script-column.hh -- declare G_script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_SCRIPT_COLUMN_HH
#define G_SCRIPT_COLUMN_HH

#include "item.hh"

class G_script_column : public Item
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


#endif /* G_SCRIPT_COLUMN_HH */


