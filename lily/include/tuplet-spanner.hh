/*
  plet-spanner.hh -- part of GNU LilyPond

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Tuplet_spanner_HH
#define Tuplet_spanner_HH

#include "pointer.hh"
#include "directional-spanner.hh"

/** supportable plet: triplets, eentweetjes, ottava, etc.

    TODO: quantise, we don't want to collide with staff lines.
    (or should we be above staff?)

  todo: handle breaking elegantly.
*/
class Tuplet_spanner : public Directional_spanner
{
public:
  Tuplet_spanner ();
 
  void add_column (Note_column*);
  void add_beam (Beam*);
  

  String  number_str_;

  bool parallel_beam_b_;
  
protected:
  Link_array<Beam> beam_l_arr_;
  Link_array<Note_column> column_arr_;

  virtual Molecule* do_brew_molecule_p () const;
  VIRTUAL_COPY_CONS(Score_element);
  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual Direction get_default_dir () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
};

#endif // Tuplet_spanner_HH

