/*
  tuplet-bracket.hh -- part of GNU LilyPond

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Tuplet_bracket_HH
#define Tuplet_bracket_HH

#include "lily-guile.hh"

/*

    TODO: quantise, we don't want to collide with staff lines.
 (or should we be above staff?)

  todo: handle breaking elegantly.
*/
class Tuplet_bracket
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);

  static void add_column (Grob*me,Item*);
  static void add_beam (Grob*me,Grob*);
  static Grob *parallel_beam (Grob *me, Link_array<Grob> cols, bool *equally_long);
  static void calc_dy (Grob*,Real *) ;
  static void calc_position_and_height (Grob*,Real*,Real *dy);
  
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));

  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  static Molecule make_bracket (Axis protusion_axis,
				Real dx, Real dy, Real thick, Real lprotrusion,
				Real rprotrusion, Real gap, Real left_widen,
				Real right_widen);
  static Direction get_default_dir (Grob*);
};

#endif // Tuplet_bracket_HH

