/*
  dots.cc -- implement Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"

Dots::Dots ()
{
  dots_i_ =0;
  position_i_ =0;
  resolve_dir_ =CENTER;
}

void
Dots::do_post_processing ()
{
  if (!resolve_dir_)
    resolve_dir_ = UP;
  
  if (!(position_i_ % 2))
    position_i_ += resolve_dir_;

  if (!dots_i_)
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (true, X_AXIS, Y_AXIS);
    }
}

Molecule* 
Dots::do_brew_molecule_p () const
{
  Molecule *out = new Molecule;
  Molecule fill = lookup_l ()->fill (Box (Interval (0,0),
					       Interval (0,0)));
  out->add_molecule (fill);

  Molecule d = lookup_l ()->dots ();

  Real dw = d.dim_[X_AXIS].length ();
  d.translate_axis (-dw, X_AXIS);
  for (int i=dots_i_; i--; )
    {
      d.translate_axis (2*dw,X_AXIS);
      out->add_molecule (d);
    }
  out->translate_axis (staff_line_leading_f () * position_i_ /2., Y_AXIS);
  return out;
}



