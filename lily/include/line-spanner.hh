/*
  line-spanner.hh -- declare Line_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef LINE_SPANNER_HH
#define LINE_SPANNER_HH

#include "lily-guile.hh"

class Line_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  
  static Molecule line_molecule (Grob* me, Real thick, Real dx, Real dy);
  static bool has_interface (Grob*);

private:
  static Offset get_broken_offset (Grob *me, Direction dir);
  static Offset broken_trend_offset (Grob *me, Direction dir);
};

#endif /* LINE_SPANNER_HH */
