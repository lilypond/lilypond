/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef COLLISION_HH
#define COLLISION_HH

#include "std-vector.hh"
#include "grob-interface.hh"
#include "lily-proto.hh"


/**
   Resolve conflicts between various Note_columns (chords).

   TODO

   * multistaff support (see Chlapik: equal noteheads should be on the
   same hpos.)

   * Make interface of this, similar to align-interface.
   */
class Note_collision_interface
{
public:
  static SCM automatic_shift (Grob *, Drul_array<vector<Grob*> >);
  static SCM forced_shift (Grob *);

  static Drul_array<vector<Grob*> > get_clash_groups (Grob *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM smob));
  static void add_column (Grob *me, Grob *ncol);
  DECLARE_GROB_INTERFACE();
};
#endif // COLLISION_HH
