/*
  hyphen-spanner.cc -- implement Hyphen_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>

  (adapted from extender-spanner)
*/

/*
  TODO: too complicated implementation.  Why the dx_drul?.
 */

#include <math.h>
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "hyphen-spanner.hh"
#include "dimension-cache.hh"

Hyphen_spanner::Hyphen_spanner ()
  : Spanner ()
{
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;

  dim_cache_[Y_AXIS]->set_callback (Dimension_cache::point_dimension_callback);
}

Molecule 
Hyphen_spanner::do_brew_molecule () const
{
  Molecule  mol;

  Real w = spanner_length ();

  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);

  Real th = paper_l ()->get_var ("hyphen_thickness");
  Real h = paper_l ()->get_var ("hyphen_height");

  // UGH. First try: just make the hyphen take 1/3 of the available space  
  // for length, use a geometric mean of the available space and some minimum
  Real l = paper_l ()->get_var ("hyphen_minimum_length");
  if(l < w)
    l = sqrt(l*w);
  Molecule a = lookup_l ()->filledbox ( Box (Interval ((w-l)/2,(w+l)/2), Interval (h,h+th)));
  a.translate (Offset (dx_f_drul_[LEFT], 0));

  mol.add_molecule (a);

  return mol;
}


void
Hyphen_spanner::do_post_processing ()
{
  // UGH
  Real gap = paper_l ()->get_var ("interline");

  Direction d = LEFT;
  do
    {
      Item* t = spanned_drul_[d]
	? spanned_drul_[d] : spanned_drul_[(Direction)-d];
      if (d == LEFT)
        dx_f_drul_[d] += t->extent (X_AXIS).length ();
      else
	dx_f_drul_[d] -= d * gap / 2;
    }
  while (flip(&d) != LEFT);
}

  
void
Hyphen_spanner::set_textitem (Direction d, Item* textitem_l)
{
  set_bounds (d, textitem_l);
  add_dependency (textitem_l);
}


