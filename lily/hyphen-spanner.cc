/*
  hyphen-spanner.cc -- implement Hyphen_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1999--2003 Glen Prideaux <glenprideaux@iname.com>

 (adapted from lyric-extender)
*/

#include <math.h>

#include "box.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "hyphen-spanner.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "item.hh"


MAKE_SCHEME_CALLBACK (Hyphen_spanner,brew_molecule,1)
SCM 
Hyphen_spanner::brew_molecule (SCM smob)
{
  Spanner * sp = unsmob_spanner (smob);
  Drul_array<Item*> bounds (sp->get_bound (LEFT),
			    sp->get_bound (RIGHT));
  
  Grob * common = bounds[LEFT]->common_refpoint (bounds[RIGHT], X_AXIS);

  Interval span_points;
  Direction d = LEFT;
  do
    {
      Interval iv = bounds[d]->extent (common, X_AXIS);

      span_points[d] = iv.is_empty ()
	? bounds[d]->relative_coordinate (common, X_AXIS)
	: iv[-d];
    }
  while (flip (&d) != LEFT);
  
  Real lt = sp->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));
  Real th = gh_scm2double (sp->get_grob_property ("thickness")) * lt ;
  Real h = gh_scm2double (sp->get_grob_property ("height"));

  // interval?
  
  Real dp = gh_scm2double (sp->get_grob_property ("dash-period"));
  Real dl = gh_scm2double (sp->get_grob_property ("length"));

  if (dp < dl)
    dp = 1.5 * dl;

  Real l = span_points.length ();
  int n = int (ceil (l/dp - 0.5));
  if (n <= 0)
    n = 1;

  Real space_left = l - dl - (n-1)* dp;

  /*
    If there is not enough space, the hyphen should disappear.
   */
  if (space_left < 0)
    return SCM_EOL;
  
  Box b (Interval (0, dl), Interval (h,h+th));
  Molecule dash_mol (Lookup::round_filled_box (b, 0.8 * lt));

  Molecule total;
  for (int i = 0; i < n; i++)
    {
      Molecule m (dash_mol);
      m.translate_axis (span_points[LEFT] + i * dp + space_left / 2, X_AXIS);
      total.add_molecule (m);
    }

  total.translate_axis ( -sp->relative_coordinate (common, X_AXIS), X_AXIS);
  return total.smobbed_copy ();
}



ADD_INTERFACE (Hyphen_spanner, "lyric-hyphen-interface",
	       "A centred hyphen is a simple line between lyrics used to divide syllables",
	       "thickness height dash-period length");

