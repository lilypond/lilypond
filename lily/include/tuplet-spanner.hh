/*
  plet-spanner.hh -- part of GNU LilyPond

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Tuplet_spanner_HH
#define Tuplet_spanner_HH

#include "spanner.hh"

/** supportable plet: triplets, eentweetjes, ottava, etc.

    TODO: quantise, we don't want to collide with staff lines.
    (or should we be above staff?)

  todo: handle breaking elegantly.
*/
class Tuplet_spanner : public Spanner
{
public:
  Tuplet_spanner (SCM);
  static SCM brew_molecule (SCM);
  

  void add_column (Note_column*);
  void add_beam (Beam*);

  void calc_dy (Real *) const;
  void calc_position_and_height (Real*,Real *dy)const;
  
  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  SCM member_brew_molecule () const;
  VIRTUAL_COPY_CONS(Score_element);

  virtual void do_add_processing ();
  virtual Direction get_default_dir () const;
};

#endif // Tuplet_spanner_HH

