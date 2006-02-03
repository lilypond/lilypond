/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef COLLISION_HH
#define COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "std-vector.hh"

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
  static SCM automatic_shift (Grob *, Drul_array<Link_array__Grob_ >);
  static SCM forced_shift (Grob *);

  static Drul_array<Link_array__Grob_ > get_clash_groups (Grob *me);
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM smob));
  static void add_column (Grob *me, Grob *ncol);
  static bool has_interface (Grob *);
};
#endif // COLLISION_HH
