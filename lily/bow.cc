/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bow.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"

IMPLEMENT_IS_TYPE_B1(Bow,Directional_spanner);

Bow::Bow ()
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}


Offset
Bow::center () const
{
  Real dy =  dy_f_drul_[RIGHT]-dy_f_drul_[LEFT];

  Real w = width ().length ();

  return Offset (w/2,dy );
}

Molecule*
Bow::brew_molecule_p () const
{
  Molecule*output = new Molecule;
  Real w = width ().length ();
  
  Real dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  
  Real nw_f = paper ()->note_width ();
  
  w+= (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  Real round_w = w;		// slur lookup rounds the slurwidth .
  
  Atom a = paper ()->lookup_l ()->slur (dy_f, round_w, height_f (), dir_);

  Real error = w-round_w;
  a.translate (Offset ( (dx_f_drul_[LEFT] + 0.5*nw_f)
		       + error/2,
		       dy_f_drul_[LEFT]));
  output->add (a);
  return output;
}

Real
Bow::height_f () const
{
  return 0;
}

