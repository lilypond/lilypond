/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bow.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"

Bow::Bow()
{
  pos_i_drul_[LEFT] = pos_i_drul_[RIGHT] = 0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}


Offset
Bow::center() const
{
  int dy =  pos_i_drul_[RIGHT]-pos_i_drul_[LEFT];

  Real w = width().length ();

  return Offset (w/2,dy * paper()->internote_f ());
}


Molecule*
Bow::brew_molecule_p() const
{
  Molecule*output = new Molecule;
  Real w = width().length ();
  
  int dy = pos_i_drul_[RIGHT] - pos_i_drul_[LEFT];
  
  Real nw_f = paper()->note_width ();
  Real nh_f = paper()->internote_f ();

  
  w+= (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]) * nw_f ;
  Real round_w = w;		// slur lookup rounds the slurwidth .
  
  Atom a = paper()->lookup_l ()->slur (dy , round_w, dir_);

  Real error = w-round_w;
    a.translate (Offset ((dx_f_drul_[LEFT] + 0.5)*nw_f + error/2,
		       pos_i_drul_[LEFT] * nh_f));
  output->add (a);
  return output;
}


IMPLEMENT_IS_TYPE_B1(Bow,Directional_spanner);
