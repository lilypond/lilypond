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

Hyphen_spanner::Hyphen_spanner ()
  : Directional_spanner ()
{
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}

// UGH - is this even used?
Offset
Hyphen_spanner::center () const
{
  Real dx = extent (X_AXIS).length ();

  return Offset (dx / 2, 0);
}

Molecule*
Hyphen_spanner::do_brew_molecule_p () const
{
  Molecule* mol_p = new Molecule;

  Real w = extent (X_AXIS).length ();

  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);

  Real th = paper_l ()->get_realvar (hyphen_thickness_scm_sym);
  Real h = paper_l ()->get_realvar (hyphen_height_scm_sym);

  // UGH. First try: just make the hyphen take 1/3 of the available space  
  // for length, use a geometric mean of the available space and some minimum
  Real l = paper_l ()->get_realvar (hyphen_minimum_length_scm_sym);
  if(l < w)
    l = sqrt(l*w);
  Molecule a = lookup_l ()->filledbox ( Box (Interval ((w-l)/2,(w+l)/2), Interval (h,h+th)));
  a.translate (Offset (dx_f_drul_[LEFT], 0));

  mol_p->add_molecule (a);

  return mol_p;
}

Interval
Hyphen_spanner::do_height () const
{
  return Interval (0,0);
}

void
Hyphen_spanner::do_post_processing ()
{
  // UGH
  Real gap = paper_l ()->get_realvar (interline_scm_sym);

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

