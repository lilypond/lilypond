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
public:
  Script_column();
  void add_staff_sided (Item*);
protected:
  virtual void before_line_breaking ();
};


#endif /* Script_COLUMN_HH */


