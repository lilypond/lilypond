/*
  lyric-extender.cc -- implement Lyric_extender
  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  TODO: too complicated implementation.  Why the dx_drul?.
 */

#
#include "dimension-cache.hh"
#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "extender-spanner.hh"

Lyric_extender::Lyric_extender (SCM s)
  : Spanner (s)

{
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
  set_extent_callback (Score_element::point_dimension_callback, Y_AXIS);
}




MAKE_SCHEME_SCORE_ELEMENT_CALLBACKS(Lyric_extender)
Molecule 
Lyric_extender::do_brew_molecule () const
{
  Molecule  mol;

  Real w = spanner_length ();
  
  w += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  Real h = paper_l ()->get_var ("extender_height");
  Molecule a = lookup_l ()->filledbox ( Box (Interval (0,w), Interval (0,h)));
  a.translate (Offset (dx_f_drul_[LEFT], 0));

  mol.add_molecule (a);

  return mol;
}



void
Lyric_extender::after_line_breaking ()
{
  // UGH
  Real gap = paper_l ()->get_var ("interline");

  Direction d = LEFT;
  do
    {
      Item* t = get_bound (d)
	? get_bound (d) : get_bound ((Direction)-d);
      if (d == LEFT)
        dx_f_drul_[d] += t->extent (X_AXIS).length ();
      else
	dx_f_drul_[d] -= d * gap / 2;
    }
  while (flip(&d) != LEFT);
}

  
void
Lyric_extender::set_textitem (Direction d, Item* textitem_l)
{
  set_bound (d, textitem_l);
  add_dependency (textitem_l);
}


