/*
  hyphen-spanner.cc -- implement Hyphen_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>

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

  Grob * common = sp;
  Direction d = LEFT;
  do
    {
      common = common->common_refpoint (sp->get_bound (d), X_AXIS);
    }
  while (flip (&d) != LEFT);
  Interval bounds;
  
  do
    {
      Interval iv = sp->get_bound (d)->extent (common, X_AXIS);

      bounds[d] = iv.empty_b ()
	? sp->get_bound (d)->relative_coordinate (common, X_AXIS)
	: iv[-d];
    }
  while (flip (&d) != LEFT);
  
  Real lt = sp->paper_l ()->get_var ("linethickness");
  Real th = gh_scm2double (sp->get_grob_property ("thickness")) * lt ;
  Real h = gh_scm2double (sp->get_grob_property ("height"));

  // interval?
  Real l = gh_scm2double (sp->get_grob_property ("minimum-length"));  
  Real x = gh_scm2double (sp->get_grob_property ("maximum-length"));
  // The hyphen can exist in the word space of the left lyric ...
  SCM space =  sp->get_bound (LEFT)->get_grob_property ("word-space");
  if (gh_number_p (space))
    {
      bounds[LEFT] -=  gh_scm2double (space);
    }

  /*
    we should probably do something more intelligent when bounds is
    empty, but at least this doesn't crash.
  */      
  Real w  = bounds.empty_b () ? 0 : bounds.length ();
  
  /* for length, use a geometric mean of the available space and some minimum
   */
  if (l < w)
    {
      l = sqrt (l*w);
      if (l > x)
	l = x;
    }
  else
    {
      /* OK, we have a problem. Usually this means that we're on the
         first column, and we have a long lyric which extends to near
         the offset for stuff */
      /* This test for being on the first column has been shamelessly
         ripped from spanner.cc */
      Paper_column *sc = dynamic_cast<Paper_column*> (sp->get_bound (LEFT)->column_l ());
      if (sc != NULL &&
	  sc->break_status_dir () == RIGHT)
	{
	  /* We are on the first column, so it's probably harmless to
             get the minimum length back by extending leftwards into
             the space under the clef/key sig/time sig */
	  bounds[LEFT] = bounds[RIGHT] - l;
	}
      else 
	{
	  /* We can't get the length desired. Maybe we should warn. */
	  l = w;
	}
    }
  Box b (Interval (-l/2,l/2), Interval (h,h+th));
  Molecule mol (Lookup::filledbox (b));
  Real ct = bounds.empty_b () ? 0 : bounds.center () ;
  mol.translate_axis (ct -sp->relative_coordinate (common, X_AXIS), X_AXIS);
  return mol.smobbed_copy ();
}
  
void
Hyphen_spanner::set_textitem (Direction d, Grob* b)
{
  elt_l_->set_bound (d, b);
  elt_l_->add_dependency (b);
}

Hyphen_spanner::Hyphen_spanner (Spanner*s)
{
  elt_l_ = s;
}



ADD_INTERFACE (Hyphen_spanner, "lyric-hyphen-interface",
  "A centred hyphen is a simple line between lyrics used to divide
syllables.   The length of the hyphen line should stretch based on the
size of the gap between syllables.",
  "thickness height minimum-length maximum-length word-space");

