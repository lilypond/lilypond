/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "crescendo.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "atom.hh"

Crescendo::Crescendo ()
{
  grow_dir_ =0;
  dyn_b_drul_[LEFT] = dyn_b_drul_[RIGHT] =false;
}



Molecule*
Crescendo::do_brew_molecule_p () const
{
  Real absdyn_dim = paper_l ()-> get_var ("crescendo_shorten");
  Real extra_left =  get_broken_left_end_align ();

  if (dyn_b_drul_[LEFT])
    extra_left += absdyn_dim;

  

  Real width = spanner_length()- get_broken_left_end_align ();

  if (dyn_b_drul_[LEFT])
    {
      width -= absdyn_dim;
    }
  if (dyn_b_drul_[RIGHT])
    {
      width -= absdyn_dim;
    }

  if (width < 0)
    {
      warning (_ ("crescendo") + " " + _ ("too small"));
      width = 0;
    }

  Drul_array<bool> broken;
  Direction d = LEFT;
  do {
    Paper_column* s = dynamic_cast<Paper_column*>(spanned_drul_[d]); // UGH
    broken[d] = (!s->musical_b ());
  } while (flip (&d) != LEFT);
  

  bool continued = broken[Direction (-grow_dir_)];
  Real height = paper_l()->get_var ("crescendo_height");
  Real thick = paper_l ()->get_var ("crescendo_thickness");

  const char* hairpin = (grow_dir_ < 0)? "decrescendo" :  "crescendo";
  Atom at  (gh_list (ly_symbol2scm (hairpin),
		     gh_double2scm (thick),
		     gh_double2scm (width),
		     gh_double2scm (height),
		     gh_double2scm (continued ? height/2 : 0.0),
		     SCM_UNDEFINED));
  Molecule * m
    = new Molecule;
  
  m->dim_.x () = Interval (0, width);
  m->dim_.y () = Interval (-2*height, 2*height);
  m->add_atom (&at);
  
  m->translate_axis (extra_left, X_AXIS);
  return m;
}


