/*
  crescendo.cc -- implement Hairpin

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "crescendo.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "paper-column.hh"

MAKE_SCHEME_CALLBACK (Hairpin, brew_molecule, 1);

SCM
Hairpin::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *span = dynamic_cast<Spanner*>(me);

  Real line = me->paper_l ()->get_var ("stafflinethickness");  
  
  Real broken_left =  span->get_broken_left_end_align ();

  SCM s = me->get_grob_property("grow-direction");
  if (!isdir_b (s))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction grow_dir = to_dir (s);
  Real padding = gh_scm2double (me->get_grob_property ("padding"));
  Real width = span->spanner_length ();
  width -= span->get_broken_left_end_align ();

  if (width < 0)
    {
      warning (_ ((grow_dir < 0) ? "decrescendo too small"
		  : "crescendo too small"));
      width = 0;
    }

  Drul_array<bool> broken;
  Drul_array<Real> extra_off;
  Direction d = LEFT;
  do
    {
      Item *b = span->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (!broken [d])
	{

	  Interval e =b->extent (b, X_AXIS);
	  Real r = 0.0;
	  if (!e.empty_b ())
	    r = e[-d] + padding;
	  width += d * r;
	  extra_off[d] = r;
	}
    }
  while (flip (&d) != LEFT);
  
  bool continued = broken[Direction (-grow_dir)];
  Real height = gh_scm2double (me->get_grob_property ("height"));
  Real thick = line * gh_scm2double (me->get_grob_property ("thickness"));
  
  const char* type = (grow_dir < 0) ? "decrescendo" :  "crescendo";
  SCM hairpin = gh_list (ly_symbol2scm (type),
		    gh_double2scm (thick),
		    gh_double2scm (width),
		    gh_double2scm (height),
		    gh_double2scm (continued ? height/2 : 0.0),
		    SCM_UNDEFINED);

  Box b (Interval (0, width), Interval (-2*height, 2*height));
  Molecule mol (b, hairpin);
  mol.translate_axis (broken_left + extra_off[LEFT], X_AXIS);

  return mol.smobbed_copy ();
}


