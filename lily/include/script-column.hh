/*   
  g-script-column.hh -- declare Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "item.hh"

class Script_column : public Item
{
public:
  Script_column(SCM);
  void add_staff_sided (Item*);

  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);
};


#endif /* Script_COLUMN_HH */


