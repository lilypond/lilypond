/*
  plet-spanner.hh -- part of GNU LilyPond

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
  static void set_interface (Grob*);  
  static bool has_interface (Grob*);

  static void add_column (Grob*me,Item*);
  static void add_beam (Grob*me,Grob*);

  static void calc_dy (Grob*,Real *) ;
  static void calc_position_and_height (Grob*,Real*,Real *dy);
  
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));

  static Direction get_default_dir (Grob*);
};

#endif // Tuplet_bracket_HH

