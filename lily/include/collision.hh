/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"


/**
  Resolve conflicts between various Note_columns (chords).
  
  TODO 

  * multistaff support (see Chlapik: equal noteheads should be on the
  same hpos.)  

  * Make interface of this, similar to align-interface.
  
  Properties:

  elements -- (see Axis_group_interface)

  merge-differently-dotted -- merge black noteheads with differing dot count.

  horizontal-shift -- integer that identifies ranking of note-column for horizontal shifting.
  
  force-hshift -- amount of collision_note_width that overides automatic collision settings.
  Read and removed from elements.
  
*/
class Collision			// interface
{
public:
  Score_element*  elt_l_;
  static SCM automatic_shift (Score_element*);
  static SCM forced_shift (Score_element*);
  static Real force_shift_callback (Score_element *, Axis);
  static void do_shifts (Score_element*);

  void add_column (Note_column*ncol_l);
  Collision(Score_element*);
};
#endif // COLLISION_HH
