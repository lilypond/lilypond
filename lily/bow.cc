/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
      Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bow.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "bezier.hh"
#include "main.hh"



Bow::Bow ()
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
  dash_i_ = 0;
  interstaff_f_ = 0;
  vertical_align_drul_[MIN] = 0;
  vertical_align_drul_[MAX] = -1;
}

Molecule*
Bow::do_brew_molecule_p () const
{
  Real thick = paper_l ()->get_var ("slur_thickness");
  Array<Offset> c = get_controls ();
  Real dy = c[3].y () - c[0].y ();
  Molecule a;

  if (!dash_i_)
    a = lookup_l ()->slur (c);
  else
    a = lookup_l ()->dashed_slur (c, thick, dash_i_);

  if (check_debug && !monitor->silent_b ("Bow"))
    {
      static int i = 1;
      cout << "******" << i++ << "******" << endl;
      cout << "c0.y: " << c[0].y () << endl;
      cout << "c3.y: " << c[3].y () << endl;
      cout << "dy: " << dy << endl;
      cout << "dy_f_l: " << dy_f_drul_[LEFT] << endl;
      cout << "dy_f_r: " << dy_f_drul_[RIGHT] << endl;
      cout << "dy_f: " << dy_f_drul_[RIGHT] - dy_f_drul_[LEFT] << endl;
    }
  a.translate (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));

  Molecule* mol_p = new Molecule;
  mol_p->add_molecule (a);
  mol_p->translate_axis (-interstaff_f_, Y_AXIS);

  return mol_p;
}

Offset
Bow::center () const
{
  Real dy = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];

  Real dx =  extent(X_AXIS).length ();

  return Offset (dx / 2, dy);
}



/*
   Ugh.  Control points are too crude measures.
 */
Interval
Bow::do_height () const
{
  Interval iv;
  Array<Offset> c (get_controls());
  for (int i=0; i < c.size (); i++)
    {
      Real y = c[i][Y_AXIS];
      iv.unite (Interval (y,y));
    }
  iv -= interstaff_f_;
  return iv;
}


Array<Offset>
Bow::get_controls () const
{
  Bezier_bow b (paper_l ());
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
  Offset d (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d.x() += extent (X_AXIS). length ();

  Array<Offset> notes;
  notes.push (Offset (0, 0));
  notes.push (d);

  return notes;
}

