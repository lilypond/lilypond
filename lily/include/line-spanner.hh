/*
  line-spanner.hh -- declare Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef LINE_SPANNER_HH
#define LINE_SPANNER_HH

#include "lily-guile.hh"

class Line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static SCM line_atom (Grob* me, Real dx, Real dy);

private:
  static Offset get_broken_offset (Grob *me, Direction dir);
  static Offset broken_trend_offset (Grob *me, Direction dir);
};

#endif /* LINE_SPANNER_HH */
