/*   
  g-script-column.hh -- declare Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

class Script_column
{
public:
  static void add_staff_sided (Score_element*, Item*);
  static SCM before_line_breaking (SCM);
};


#endif /* Script_COLUMN_HH */


