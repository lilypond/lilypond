
/*   
  tie-column.hh -- declare Tie_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef TIE_COLUMN_HH
#define TIE_COLUMN_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Tie_column
{
public:
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  static void add_tie (Score_element*me,Score_element*);
  DECLARE_SCHEME_CALLBACK(after_line_breaking, (SCM ));
  static void set_directions (Score_element*me);
};

#endif /* TIE_COLUMN_HH */

