/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

#include "drul-array.hh"
#include "parray.hh"

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
  static SCM automatic_shift (Grob*, Drul_array< Link_array <Grob>  >);
  static SCM forced_shift (Grob*);
  
  static Drul_array< Link_array <Grob>  > get_clash_groups (Grob *me);
  DECLARE_SCHEME_CALLBACK (force_shift_callback, (SCM element, SCM axis));
  static void do_shifts (Grob*);
  static void add_column (Grob*me,Grob*ncol_l);
  static bool has_interface(Grob*);
};
#endif // COLLISION_HH
