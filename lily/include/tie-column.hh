
/*   
  tie-column.hh -- declare Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TIE_COLUMN_HH
#define TIE_COLUMN_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Tie_column
{
public:
  static void set_interface (Grob*me);
  static bool has_interface (Grob*);
  static void add_tie (Grob*me,Grob*);
  DECLARE_SCHEME_CALLBACK(after_line_breaking, (SCM ));
  static void set_directions (Grob*me);
};

#endif /* TIE_COLUMN_HH */

