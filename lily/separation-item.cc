/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

using std::vector;

void
Separation_item::add_item (Grob *s, Item *i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"), i);
}

void
Separation_item::add_conditional_item (Grob *me, Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"),
                                     e);
}

Real
Separation_item::set_distance (Item *l, Item *r, Real padding)
{
  Drul_array<Skyline_pair> lines (
    from_scm<Skyline_pair> (get_property (l, "horizontal-skylines")),
    from_scm<Skyline_pair> (get_property (r, "horizontal-skylines")));
  Skyline right = conditional_skyline (r, l);
  right.merge (lines[RIGHT][LEFT]);

  Real dist = padding + lines[LEFT][RIGHT].distance (right);
  if (dist > 0)
    {
      Rod rod;

      rod.item_drul_ = Drul_array<Item *> (l, r);

      rod.distance_ = dist;
      rod.add_to_cols ();
    }

  return std::max (dist, 0.0);
}

bool
Separation_item::is_empty (Grob *me)
{
  const Skyline_pair &sky
    = from_scm<Skyline_pair> (get_property (me, "horizontal-skylines"));
  return (sky.is_empty ());
}

/*
  Return the width of ME given that we are considering the object on
  the LEFT.
*/
Skyline
Separation_item::conditional_skyline (Grob *me, Grob *left)
{
  vector<Box> const &bs = boxes (me, left);
  return Skyline (bs, Y_AXIS, LEFT);
}

MAKE_SCHEME_CALLBACK (Separation_item, calc_skylines,
                      "ly:separation-item::calc-skylines", 1);
SCM
Separation_item::calc_skylines (SCM smob)
{
  Item *me = unsmob<Item> (smob);
  vector<Box> const &bs = boxes (me, 0);
  Skyline_pair sp (bs, Y_AXIS);
  /*
    TODO: We need to decide if padding is 'intrinsic'
    to a skyline or if it is something that is only added on in
    distance calculations.  Here, we make it intrinsic, which copies
    the behavior from the old code but no longer corresponds to how
    vertical skylines are handled (where padding is not built into
    the skyline).
  */
  Real vp
    = from_scm<double> (get_property (me, "skyline-vertical-padding"), 0.0);
  sp[LEFT] = sp[LEFT].padded (vp);
  sp[RIGHT] = sp[RIGHT].padded (vp);
  return to_scm (sp);
}

/*
   If left is non-NULL, get the boxes corresponding to the
   conditional-elements (conditioned on the grob LEFT).
   Conditional elements are, for now, arpeggios and accidental
   placements.  Based on the left grob, the accidentals will
   be printed or not, so we filter using
   Accidental_placement::get_relevant_accidentals.
*/
vector<Box>
Separation_item::boxes (Grob *me, Grob *left)
{
  Item *item = dynamic_cast<Item *> (me);

  int very_large = INT_MAX;
  Paper_column *pc = item->get_column ();
  vector<Box> out;
  extract_grob_set (me, left ? "conditional-elements" : "elements",
                    read_only_elts);
  vector<Grob *> elts;

  if (left)
    {
      vector<Grob *> accidental_elts;
      vector<Grob *> other_elts; // for now only arpeggios
      for (vsize i = 0; i < read_only_elts.size (); i++)
        {
          if (has_interface<Accidental_placement> (read_only_elts[i]))
            accidental_elts.push_back (read_only_elts[i]);
          else
            other_elts.push_back (read_only_elts[i]);
        }
      elts = Accidental_placement::get_relevant_accidentals (accidental_elts,
                                                             left);
      elts.insert (elts.end (), other_elts.begin (), other_elts.end ());
    }
  else
    elts = read_only_elts;

  Grob *ycommon = common_refpoint_of_array (elts, me, Y_AXIS);

  for (vsize i = 0; i < elts.size (); i++)
    {
      Item *il = dynamic_cast<Item *> (elts[i]);
      if (pc != il->get_column ())
        continue;

      // Exclude groups of grobs, so as to insert a box for each contained grob
      // into the skyline instead of a single box that bounds all of them.
      if (has_interface<Axis_group_interface> (il))
        continue;

      Interval y (il->pure_y_extent (ycommon, 0, very_large));
      Interval x (il->extent (pc, X_AXIS));

      Interval extra_width = from_scm (
        get_property (elts[i], "extra-spacing-width"), Interval (-0.1, 0.1));
      Interval extra_height = from_scm (
        get_property (elts[i], "extra-spacing-height"), Interval (0.0, 0.0));

      // The conventional empty extent is (+inf.0 . -inf.0)
      //  but (-inf.0 . +inf.0) is used as extra-spacing-height
      //  on items that must not overlap other note-columns.
      // If these two uses of inf combine, leave the empty extent.

      if (!std::isinf (x[LEFT]))
        x[LEFT] += extra_width[LEFT];
      if (!std::isinf (x[RIGHT]))
        x[RIGHT] += extra_width[RIGHT];
      if (!std::isinf (y[DOWN]))
        y[DOWN] += extra_height[DOWN];
      if (!std::isinf (y[UP]))
        y[UP] += extra_height[UP];

      if (!x.is_empty () && !y.is_empty ())
        out.push_back (Box (x, y));
    }

  return out;
}

ADD_INTERFACE (Separation_item,
               R"(
Item that computes widths to generate spacing rods.
               )",

               /* properties */
               R"(
X-extent
conditional-elements
elements
padding
horizontal-skylines
skyline-vertical-padding
               )");
