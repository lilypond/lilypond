/*
  plet-spanner.hh -- part of GNU LilyPond

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PLET_SPANNER_HH
#define PLET_SPANNER_HH
#include "text-def.hh"
#include "pointer.hh"
#include "directional-spanner.hh"

/** supportable plet: triplets, eentweetjes, ottava, etc.  */

class Plet_spanner : public Directional_spanner
{
public:
  Plet_spanner ();
 
  void add_column (Note_column*);
  void set_beam (Beam*);
  P<Text_def>  tdef_p_;
  bool bracket_visibility_b_;
  bool num_visibility_b_;
  
protected:
  Beam *beam_l_;
  Link_array<Note_column> column_arr_;

  virtual Molecule* brew_molecule_p () const;
  VIRTUAL_COPY_CONS(Score_element);

  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual void set_default_dir ();
  virtual void do_substitute_dependency (Score_element*,Score_element*);
};

#endif // PLET_SPANNER_HH

