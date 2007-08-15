/*
  tie-column.hh -- declare Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TIE_COLUMN_HH
#define TIE_COLUMN_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Tie_column
{
public:
  static bool has_interface (Grob *);
  static void add_tie (Grob *me, Grob *);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  static void set_directions (Grob *me);
  static void new_directions (Grob *me);
};

#endif /* TIE_COLUMN_HH */

