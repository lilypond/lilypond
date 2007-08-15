/*
  g-script-column.hh -- declare Script_column

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

#include "std-vector.hh"

class Script_column
{
public:
  static void add_side_positioned (Grob *, Grob *);
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (row_before_line_breaking, (SCM));
  static bool has_interface (Grob *);
  static void order_grobs (vector<Grob*> grobs);

};

#endif /* Script_COLUMN_HH */

