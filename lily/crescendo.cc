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

Crescendo::Crescendo()
{
  grow_dir_ =0;
  dir_ = DOWN;
  dyn_b_drul_[LEFT] = dyn_b_drul_[RIGHT] =false;
}

Interval
Crescendo::symbol_height() const
{
  return get_symbol().dim_[Y_AXIS];
}

static Real absdyn_dim = 10 PT;	// ugh

Atom
Crescendo::get_symbol() const
{
  Real w_dim = width().length ();
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
      warning (_ ("crescendo") + " " + _("too small"));
      w_dim = 0;
    }

  Drul_array<bool> broken;
  Direction d = LEFT;
  do {
    Score_column* s = (Score_column* )spanned_drul_[d] ; // UGH
    broken[d] = (!s->musical_b());
  } while (flip(&d) != LEFT);
  

  bool continued = broken[(Direction)-grow_dir_];
  return Atom (lookup_l ()->hairpin (w_dim, grow_dir_ < 0, continued));
}

Molecule*
Crescendo::brew_molecule_p() const
{
  Molecule* m_p =0;
  Real x_off_dim=0.0;
  if (dyn_b_drul_[LEFT])
    x_off_dim += absdyn_dim;

  m_p = new Molecule;
  Atom s (get_symbol());
  m_p->add_atom  (s);
  m_p->translate (Offset (x_off_dim, coordinate_offset_f_));
  return m_p;
}


IMPLEMENT_IS_TYPE_B1(Crescendo,Spanner);
