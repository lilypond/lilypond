/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
      Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "bow.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "bezier.hh"

IMPLEMENT_IS_TYPE_B1(Bow,Directional_spanner);

Bow::Bow ()
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}

Molecule*
Bow::brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;
  
  Array<Offset> c = get_controls ();
  Atom a = paper ()->lookup_l ()->slur (c);
//  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT] - c[0].y ()));

  mol_p->add (a);

  return mol_p;
}

Offset
Bow::center () const
{
  Real dy = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];

  Real dx = width ().length ();

  return Offset (dx / 2, dy);
}

Interval
Bow::do_width () const    
{
  Interval i = Spanner::do_width ();
  Real dx = i.length();
  return Interval (0, dx);
}

Array<Offset>
Bow::get_controls () const
{
  Bezier_bow b (paper ());
  b.set (get_encompass_offset_arr (), dir_);
  b.calc ();
  Array<Offset> controls;
  controls.set_size (8);
  for (int i = 0; i < 4; i++)
    controls[i] = b.control_[i];
  for (int i = 0; i < 4; i++)
    controls[i + 4] = b.return_[i];
  return controls;
}

Array<Offset>
Bow::get_encompass_offset_arr () const
{
  Real dx = width (). length ();
  dx += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  Real dy = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];

  Array<Offset> notes;
  notes.push (Offset (0,0));
  notes.push (Offset (dx, dy));

  return notes;
}

