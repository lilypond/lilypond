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
#include "score-column.hh"

Crescendo::Crescendo ()
{
  grow_dir_ =0;
  dyn_b_drul_[LEFT] = dyn_b_drul_[RIGHT] =false;
}

Molecule
Crescendo::get_symbol () const
{
  Real w_dim = extent (X_AXIS).length () - get_broken_left_end_align ();
  Real absdyn_dim = paper_l ()-> get_var ("crescendo_shorten");
  if (dyn_b_drul_[LEFT])
    {
      w_dim -= absdyn_dim;
    }
  if (dyn_b_drul_[RIGHT])
    {
      w_dim -= absdyn_dim;
    }

  if (w_dim < 0)
    {
      warning (_ ("crescendo") + " " + _ ("too small"));
      w_dim = 0;
    }

  Drul_array<bool> broken;
  Direction d = LEFT;
  do {
    Score_column* s = dynamic_cast<Score_column*>(spanned_drul_[d]); // UGH
    broken[d] = (!s->musical_b ());
  } while (flip (&d) != LEFT);
  

  bool continued = broken[Direction (-grow_dir_)];
  Real height = paper_l()->get_var ("crescendo_height");
  Real thick = paper_l ()->get_var ("crescendo_thickness");

  return Molecule (lookup_l ()->hairpin (w_dim, height, thick, grow_dir_ < 0, continued));
}

Molecule*
Crescendo::do_brew_molecule_p () const
{
  Molecule* m_p =0;
  Real absdyn_dim = paper_l ()-> get_var ("crescendo_shorten");
  Real x_off_dim =  get_broken_left_end_align ();

  if (dyn_b_drul_[LEFT])
    x_off_dim += absdyn_dim;

  m_p = new Molecule;
  Molecule s (get_symbol ());
  m_p->add_molecule (s);
  m_p->translate_axis (x_off_dim, X_AXIS);
  return m_p;
}


