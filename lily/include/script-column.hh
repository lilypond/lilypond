/*
  g-script-column.hh -- declare Script_column

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef Script_COLUMN_HH
#define Script_COLUMN_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

class Script_column
{
public:
  static void add_staff_sided (Grob *, Item *);
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  static bool has_interface (Grob *);
};

#endif /* Script_COLUMN_HH */

