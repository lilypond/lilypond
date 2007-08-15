/*
  dot-column.hh -- declare Dot_column Dot_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "lily-guile.hh"

class Grob;

/**
   Group dots.  This is needed because, the dots have to be aligned per voice
*/
class Dot_column // interface
{
public:
  static int compare (Grob *const &, Grob *const &);
  static void add_head (Grob *dotcol, Grob *rh);

  static bool has_interface (Grob *);
  DECLARE_SCHEME_CALLBACK (side_position, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM));
};
#endif // DOT_COLUMN_HH
