/*
  hairpin.cc -- implement Hairpin

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "hairpin.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "lookup.hh"

MAKE_SCHEME_CALLBACK (Hairpin, brew_molecule, 1);

SCM
Hairpin::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);

  Real line = me->get_paper ()->get_var ("linethickness");  
  
  SCM s = me->get_grob_property ("grow-direction");
  if (!ly_dir_p (s))
    {
      me->suicide ();
      return SCM_EOL;
    }
  
  Direction grow_dir = to_dir (s);


  /* Ugh, must be same as Text_spanner::brew_molecule.  */

  /*
    Ugh. property name is not general.
   */
  Real padding = gh_scm2double (me->get_grob_property ("if-text-padding"));
 
  Drul_array<bool> broken;
  Drul_array<Item*> bounds ;
  Direction d = LEFT;
  do
    {
      bounds[d] =spanner->get_bound (d);
      broken[d] = bounds[d]->break_status_dir () != CENTER;
    }
  while (flip (&d) != LEFT);

  Grob *common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);
  Drul_array<Real> x_points ;
  
  do
    {
      Item *b = bounds[d];
      x_points[d]  = b->relative_coordinate (common, X_AXIS);
      if (broken [d])
	{
	  if (d == LEFT)
	    x_points[d] = b->extent (common,X_AXIS)[RIGHT] ;
	}
      else
	{
	  if (dynamic_cast<Paper_column*> (b))
	    {
	      /*
		If we're hung on a paper column, that means we're not
		adjacent to a text-dynamic, and we may move closer. We
		make the padding a little smaller, here.
	      */
	      Interval e =b->extent (common, X_AXIS);
	      if (e.empty_b ())
		e = Interval (0,0) + b->relative_coordinate (common, X_AXIS);
	      
	      x_points[d] = e.center () - d  * padding /3; // ugh.
	    }
	  else
	    {
	      Interval e =b->extent (common, X_AXIS);
	      if (!e.empty_b ())
		x_points[d] = e[-d] - d*padding;
	    }
	}
    }
  while (flip (&d) != LEFT);


  Real width = x_points[RIGHT] - x_points[LEFT];

  if (width < 0)
    {
      me->warning (_ ((grow_dir < 0) ? "decrescendo too small"
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

  Molecule mol  = Lookup::line (thick,
				Offset (0, starth),
				Offset (width, endh));
  mol.add_molecule (Lookup::line (thick,
				  Offset (0, -starth),
				  Offset (width, -endh)));
		    
  mol.translate_axis (x_points[LEFT]
		      - bounds[LEFT]->relative_coordinate (common, X_AXIS),
		      X_AXIS);

  return mol.smobbed_copy ();
}



ADD_INTERFACE (Hairpin, "hairpin-interface",
  "hairpin crescendo.",
  "grow-direction thickness height if-text-padding");

