/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "molecule.hh"
#include "dimen.hh"
#include "crescendo.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "debug.hh"

Crescendo::Crescendo()
{
  grow_dir_ =0;
  dir_ = DOWN ;
  dyn_b_drul_[LEFT] = dyn_b_drul_[RIGHT] =false;
  inside_staff_b_ = false;
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
      warning ("Crescendo too small");
      w_dim = 0;
    }

  return Atom (paper()->lookup_l ()->hairpin (w_dim, grow_dir_ < 0));
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
  m_p->add (Atom (s));
  m_p->translate (Offset (x_off_dim, pos_i_ * paper()->internote_f ()));
  return m_p;
}


IMPLEMENT_IS_TYPE_B1(Crescendo,Spanner);
