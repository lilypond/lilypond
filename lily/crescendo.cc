/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  dir_ = DOWN;
  dyn_b_drul_[LEFT] = dyn_b_drul_[RIGHT] =false;
}

Interval
Crescendo::symbol_height () const
{
  return get_symbol ().dim_[Y_AXIS];
}

static Real absdyn_dim = 10 PT;	// ugh

Molecule
Crescendo::get_symbol () const
{
  Real w_dim = extent (X_AXIS).length ();
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
  return Molecule (lookup_l ()->hairpin (w_dim, grow_dir_ < 0, continued));
}

Molecule*
Crescendo::do_brew_molecule_p () const
{
  Molecule* m_p =0;
  Real x_off_dim=0.0;
  if (dyn_b_drul_[LEFT])
    x_off_dim += absdyn_dim;

  m_p = new Molecule;
  Molecule s (get_symbol ());
  m_p->add_molecule (s);
  m_p->translate (Offset (x_off_dim, coordinate_offset_f_));
  return m_p;
}



