/*
  hairpin.cc -- implement Hairpin

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "hairpin.hh"
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
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  Real line = me->paper_l ()->get_var ("stafflinethickness");  
  
  SCM s = me->get_grob_property ("grow-direction");
  if (!isdir_b (s))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction grow_dir = to_dir (s);


  /* Ugh, must be same as Text_spanner::brew_molecule.  */  
  Real padding = gh_scm2double (me->get_grob_property ("if-text-padding"));
  Real broken_left =  spanner->get_broken_left_end_align ();
  Real width = spanner->spanner_length ();
  width -= broken_left;

  Drul_array<bool> broken;
  Drul_array<Real> extra_off;
  Direction d = LEFT;
  do
    {
      Item *b = spanner->get_bound (d);
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

  // FIXME: ecs tells us
  width += gh_scm2double (me->get_grob_property ("width-correct"));
  /* /Ugh */
  
  if (width < 0)
    {
      warning (_ ((grow_dir < 0) ? "decrescendo too small"
		  : "crescendo too small"));
      width = 0;
    }

  bool continued = broken[Direction (-grow_dir)];
  Real height = gh_scm2double (me->get_grob_property ("height"));
  Real thick = line * gh_scm2double (me->get_grob_property ("thickness"));

  Real starth, endh;
  if (grow_dir < 0)
    {
      starth = height;
      endh = continued ? height/2 : 0.0;
    }
  else
    {
      starth = continued ? height/2 : 0.0;
      endh = height;
    }
  
  /*
    TODO: junk this and, make a general

    Lookup::line  (XY1, XY2).
  */
  SCM hairpin = gh_list (ly_symbol2scm ("hairpin"),
			 gh_double2scm (thick),
			 gh_double2scm (width),
			 gh_double2scm (starth),
			 gh_double2scm (endh),
			 SCM_UNDEFINED);

  Interval yext = 2* height  * Interval (-1,1);
  Box b (Interval (0, width), yext);
  Molecule mol (b, hairpin);
  mol.translate_axis (broken_left + extra_off[LEFT], X_AXIS);

  return mol.smobbed_copy ();
}



