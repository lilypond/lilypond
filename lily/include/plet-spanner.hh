/*
  plet-spanner.hh -- part of GNU LilyPond

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PLET_SPANNER_HH
#define PLET_SPANNER_HH

#include "bow.hh"

/** supportable plet: triplets, eentweetjes, ottava, etc.  */

class Plet_spanner : public Bow
{
public:

  Plet_spanner ();
  virtual ~Plet_spanner ();
 
  void set_stem (Direction, Stem*);
 
  Text_def* tdef_p_;
  Drul_array<Stem *> stem_l_drul_;
  int visibility_i_;
 
protected:
  virtual Molecule* brew_molecule_p () const;
 
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Plet_spanner);

  virtual void do_add_processing ();
  virtual void do_post_processing ();
  virtual void set_default_dir ();
  virtual void do_substitute_dependency (Score_element*,Score_element*);
  Plet_spanner (Plet_spanner const&);
};

#endif // PLET_SPANNER_HH

