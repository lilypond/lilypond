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


MAKE_SCHEME_CALLBACK (Hyphen_spanner,set_spacing_rods,1);
SCM
Hyphen_spanner::set_spacing_rods (SCM smob)
{
  Grob*me = unsmob_grob (smob);

  Rod rod;
  Spanner*sp = dynamic_cast<Spanner*> (me);
  Item * l = sp->get_bound (LEFT);
  Item * r =  sp->get_bound (RIGHT);
  rod.item_l_drul_[LEFT] = l;
  rod.item_l_drul_[RIGHT] =r;
  rod.distance_ =
    gh_scm2double (me->get_grob_property ("minimum-length"))
    + l->extent (l, X_AXIS)[RIGHT]
    - r->extent (r, X_AXIS)[LEFT];

  rod.add_to_cols ();
  return SCM_UNSPECIFIED;
}

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

      bounds[d] = iv.is_empty ()
	? sp->get_bound (d)->relative_coordinate (common, X_AXIS)
	: iv[-d];
    }
  while (flip (&d) != LEFT);
  
  Real lt = sp->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));
  Real th = gh_scm2double (sp->get_grob_property ("thickness")) * lt ;
  Real h = gh_scm2double (sp->get_grob_property ("height"));

  // interval?
  Real x = gh_scm2double (sp->get_grob_property ("maximum-length"));
  SCM space =  sp->get_bound (LEFT)->get_grob_property ("word-space");

  Real word_space  = 1.0;
  if (gh_number_p (space))
    {
      word_space = gh_scm2double (space);
    }

  /*
    We remove word space from the distance so it doesn't look like an extender.
    
   */
  Real l = (gh_scm2double (sp->get_grob_property ("minimum-length"))
    - word_space ) >? word_space;
  
  
  /*
    we should probably do something more intelligent when bounds is
    empty, but at least this doesn't crash.
  */      
  Real w  = bounds.is_empty () ? 0 : bounds.length ();
  
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
      Paper_column *sc = dynamic_cast<Paper_column*> (sp->get_bound (LEFT)->get_column ());
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
  Real ct = bounds.is_empty () ? 0 : bounds.center () ;
  mol.translate_axis (ct -sp->relative_coordinate (common, X_AXIS), X_AXIS);
  return mol.smobbed_copy ();
}
  
void
Hyphen_spanner::set_textitem (Direction d, Grob* b)
{
  elt_->set_bound (d, b);
  elt_->add_dependency (b);
}

Hyphen_spanner::Hyphen_spanner (Spanner*s)
{
  elt_ = s;
}



ADD_INTERFACE (Hyphen_spanner, "lyric-hyphen-interface",
	       "A centred hyphen is a simple line between lyrics used to divide syllables",
	       "thickness height minimum-length maximum-length word-space");

