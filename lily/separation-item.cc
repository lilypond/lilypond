/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "separation-item.hh"

#include "accidental-placement.hh"
#include "axis-group-interface.hh"
#include "lookup.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "skyline-pair.hh"
#include "stencil.hh"
#include "warn.hh"

void
Separation_item::add_item (Grob *s, Item *i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"), i);
}

void
Separation_item::add_conditional_item (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"), e);
}

Real
Separation_item::set_distance (Item *l, Item *r, Real padding)
{
  Drul_array<Skyline_pair*> lines (Skyline_pair::unsmob (l->get_property ("horizontal-skylines")),
				   Skyline_pair::unsmob (r->get_property ("horizontal-skylines")));
  Skyline right = conditional_skyline (r, l);
  right.merge ((*lines[RIGHT])[LEFT]);
  
  Real dist = padding + (*lines[LEFT])[RIGHT].distance (right);
  if (dist > 0)
    {
      Rod rod;

      rod.item_drul_ = Drul_array<Item*> (l, r);

      rod.distance_ = dist;
      rod.add_to_cols ();
    }

  return max (dist, 0.0);
}

bool
Separation_item::is_empty (Grob *me)
{
  Skyline_pair *sky = Skyline_pair::unsmob (me->get_property ("horizontal-skylines"));
  return (!sky || sky->is_empty ());
}

/*
  Return the width of ME given that we are considering the object on
  the LEFT.
*/
Skyline
Separation_item::conditional_skyline (Grob *me, Grob *left)
{
  vector<Box> bs = boxes (me, left);
  return Skyline (bs, 0.1, Y_AXIS, LEFT);
}


MAKE_SCHEME_CALLBACK (Separation_item, calc_skylines,1);
SCM
Separation_item::calc_skylines (SCM smob)
{
  Item *me = unsmob_item (smob);
  vector<Box> bs = boxes (me, 0);
  /* todo: the horizon_padding is somewhat arbitrary */
  return Skyline_pair (bs, 0.1, Y_AXIS).smobbed_copy ();
}

/* if left is non-NULL, get the boxes corresponding to the
   conditional-elements (conditioned on the grob LEFT). This
   sounds more general than it is: conditional-elements are
   always accidentals attached to a tied note.
*/
vector<Box>
Separation_item::boxes (Grob *me, Grob *left)
{
  Item *item = dynamic_cast<Item *> (me);

  int very_large = INT_MAX;
  Paper_column *pc = item->get_column ();
  vector<Box> out;
  extract_grob_set (me, left ? "conditional-elements" : "elements", read_only_elts);
  vector<Grob*> elts;

  if (left)
    elts = Accidental_placement::get_relevant_accidentals (read_only_elts, left);
  else
    {
      elts = read_only_elts;

      /* This is a special-case for NoteColumn: we want to include arpeggio in its
	 skyline (so spacing takes it into account) but we don't want to include it
	 in the NoteColumn's extent because some spanners (eg. Hairpin) bound themselves
	 on the NoteColumn and we don't want them to include arpeggios in their bounds.
      */
      if (Grob *a = Note_column::arpeggio (me)) {
	elts.push_back (a);
      }
    }

  Grob *ycommon = common_refpoint_of_array (elts, me, Y_AXIS);
  
  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
	continue;

      /* ugh. We want to exclude groups of grobs (so that we insert each grob
	 individually into the skyline instead of adding a single box that
	 bounds all of them). However, we can't exclude an axis-group that
	 adds to its childrens' stencil. Currently, this is just TrillPitchGroup;
	 hence the check for note-head-interface. */
      if (Axis_group_interface::has_interface (il)
	  && !Note_head::has_interface (il))
	continue;

      Interval y (il->pure_height (ycommon, 0, very_large));
      Interval x (il->extent (pc, X_AXIS));

      Interval extra_width = robust_scm2interval (elts[i]->get_property ("extra-spacing-width"),
						  Interval (-0.1, 0.1));
      Interval extra_height = robust_scm2interval (elts[i]->get_property ("extra-spacing-height"),
						   Interval (-0.1, 0.1));

      x[LEFT] += extra_width[LEFT];
      x[RIGHT] += extra_width[RIGHT];
      y[DOWN] += extra_height[DOWN];
      y[UP] += extra_height[UP];
 
      if (!x.is_empty () && !y.is_empty ())
	out.push_back (Box (x, y));
    }

  return out;      
}

MAKE_SCHEME_CALLBACK (Separation_item, print, 1)
SCM
Separation_item::print (SCM smob)
{
  if (!debug_skylines)
    return SCM_BOOL_F;

  Grob *me = unsmob_grob (smob);
  Stencil ret;
  if (Skyline_pair *s = Skyline_pair::unsmob (me->get_property ("horizontal-skylines")))
    {
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[LEFT].to_points (Y_AXIS)).in_color (255, 255, 0));
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[RIGHT].to_points (Y_AXIS)).in_color (0, 255, 255));
    }
  return ret.smobbed_copy ();
}

ADD_INTERFACE (Separation_item,
	       "Item that computes widths to generate spacing rods.",

	       /* properties */
	       "X-extent "
	       "conditional-elements "
	       "elements "
	       "padding "
	       "horizontal-skylines "
	       );
