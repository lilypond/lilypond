/*
  g-script-column.hh -- declare Script_column

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "lily-proto.hh"
#include "grob-interface.hh"
#include "std-vector.hh"

class Script_column
{
public:
  static void add_side_positioned (Grob *, Grob *);
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (row_before_line_breaking, (SCM));
  DECLARE_GROB_INTERFACE();
  static void order_grobs (vector<Grob*> grobs);

};

#endif /* Script_COLUMN_HH */

