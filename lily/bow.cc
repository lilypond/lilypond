/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
      Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimension-cache.hh"
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
}

Molecule*
Bow::do_brew_molecule_p () const
{
  Real thick = paper_l ()->get_var ("slur_thickness");
  Array<Offset> c = get_controls ();

  Molecule a;

  SCM d =  get_elt_property ("dashed");
  if (d == SCM_UNDEFINED)
    a = lookup_l ()->slur (c, thick);
  else
    a = lookup_l ()->dashed_slur (c, thick, gh_scm2int (d));

  return new Molecule (a); 
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
Bow::dim_callback (Dimension_cache const* c) 
{
  Interval iv;
  Bow * b = dynamic_cast<Bow*> (c->element_l ());
  Array<Offset> p (b->get_controls());
  for (int i=0; i < p.size (); i++)
    {
      Real y = p[i][Y_AXIS];
      iv.unite (Interval (y,y));
    }
  return iv;
}

Drul_array<Interval>
Bow::curve_extent_drul () const
{
  Bezier_bow b (paper_l ());
  b.set (get_encompass_offset_arr (), get_direction ());
  b.calc ();
  return b.curve_extent_drul_;
}

Array<Offset>
Bow::get_controls () const
{
  Bezier_bow b (paper_l ());
  b.set (get_encompass_offset_arr (), get_direction ());
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
  Array<Offset> offset_arr;
  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  offset_arr.push (Offset (spanner_length () + dx_f_drul_[RIGHT],
		      dy_f_drul_[RIGHT]));
		      
  return offset_arr;
}


