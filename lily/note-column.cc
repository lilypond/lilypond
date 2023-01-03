/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "note-column.hh"

#include "accidental-placement.hh"
#include "axis-group-interface.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

#include <cmath> // ceil

/*
  TODO: figure out if we can prune this class. This is just an
  annoying layer between (rest)collision & (note-head + stem)

  TODO: If it's not pruned altogether, replace most of these
  functions with storage in objects, either in the engraver or
  via object-callbacks. --JeanAS
*/

bool
Note_column::has_rests (Grob *me)
{
  return unsmob<Grob> (get_object (me, "rest"));
}

bool
Note_column::shift_less (Grob *const &p1, Grob *const &p2)
{
  SCM s1 = get_property (p1, "horizontal-shift");
  SCM s2 = get_property (p2, "horizontal-shift");

  int h1 = (scm_is_number (s1)) ? from_scm<int> (s1) : 0;
  int h2 = (scm_is_number (s2)) ? from_scm<int> (s2) : 0;
  return h1 < h2;
}

Item *
Note_column::get_stem (Grob *me)
{
  SCM s = get_object (me, "stem");
  return unsmob<Item> (s);
}

Item *
Note_column::get_flag (Grob *me)
{
  Item *stem = get_stem (me);
  if (stem)
    {
      SCM s = get_object (stem, "flag");
      return unsmob<Item> (s);
    }
  return 0;
}

Slice
Note_column::head_positions_interval (Grob *me)
{
  Slice iv;

  iv.set_empty ();

  extract_grob_set (me, "note-heads", heads);
  for (vsize i = 0; i < heads.size (); i++)
    {
      Grob *se = heads[i];

      int j = Staff_symbol_referencer::get_rounded_position (se);
      iv.unite (Slice (j, j));
    }
  return iv;
}

Direction
Note_column::dir (Grob *me)
{
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  if (has_interface<Stem> (stem))
    return get_strict_grob_direction (stem);
  else
    {
      extract_grob_set (me, "note-heads", heads);
      if (heads.size ())
        return Direction (head_positions_interval (me).center ());
    }

  if (has_interface<Note_column> (me))
    programming_error ("Note_column without heads and stem");
  else
    programming_error ("dir() given grob without Note_column interface");
  return CENTER;
}

void
Note_column::set_stem (Grob *me, Grob *stem)
{
  set_object (me, "stem", stem->self_scm ());
  Axis_group_interface::add_element (me, stem);
}

Grob *
Note_column::get_rest (Grob *me)
{
  return unsmob<Grob> (get_object (me, "rest"));
}

void
Note_column::add_head (Grob *me, Grob *h)
{
  bool both = false;
  if (has_interface<Rest> (h))
    {
      extract_grob_set (me, "note-heads", heads);
      if (heads.size ())
        both = true;
      else
        set_object (me, "rest", h->self_scm ());
    }
  else if (has_interface<Note_head> (h))
    {
      if (unsmob<Grob> (get_object (me, "rest")))
        both = true;
      Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"), h);
    }

  if (both)
    me->warning (_ ("cannot have note heads and rests together on a stem"));
  else
    Axis_group_interface::add_element (me, h);
}

Grob *
Note_column::first_head (Grob *me)
{
  Grob *st = get_stem (me);
  return st ? Stem::first_head (st) : 0;
}

Drul_array<Grob *>
Note_column::extremal_heads (Grob *me)
{
  // This looks weird because it is weird; see the implementation.
  return Stem::extremal_heads (me);
}

/*
  Return extent of the noteheads in the "main column",
  (i.e. excluding any suspended noteheads), or extent
  of the rest (if there are no heads).
*/
MAKE_SCHEME_CALLBACK (Note_column, calc_main_extent,
                      "ly:note-column::calc-main-extent", 1);
SCM
Note_column::calc_main_extent (SCM smob)
{
  auto *me = unsmob<Grob> (smob);
  Grob *main_head = 0;
  if (get_stem (me))
    main_head = first_head (me);
  else
    {
      // no stems => no suspended noteheads.
      extract_grob_set (me, "note-heads", heads);
      if (heads.size ())
        main_head = heads[0];
    }
  Grob *main_item
    = main_head ? main_head : unsmob<Grob> (get_object (me, "rest"));

  return to_scm (main_item ? main_item->extent (me, X_AXIS) : Interval (0, 0));
}

/*
  Return the first AccidentalPlacement grob that we find in a note-head.
*/
Grob *
Note_column::accidentals (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  Grob *acc = 0;
  for (vsize i = 0; i < heads.size (); i++)
    {
      Grob *h = heads[i];
      acc = h ? unsmob<Grob> (get_object (h, "accidental-grob")) : 0;
      if (acc)
        break;
    }

  if (!acc)
    return 0;

  if (has_interface<Accidental_placement> (acc->get_x_parent ()))
    return acc->get_x_parent ();

  /* compatibility. */
  return acc;
}

Grob *
Note_column::dot_column (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  for (vsize i = 0; i < heads.size (); i++)
    {
      Grob *dots = unsmob<Grob> (get_object (heads[i], "dot"));
      if (dots)
        return dots->get_x_parent ();
    }

  return 0;
}

/* If a note-column contains a cross-staff stem then
   nc->extent (Y_AXIS, refp) will not consider the extent of the stem.
   If you want the extent of the stem to be included (and you are safe
   from any cross-staff issues) then call this function instead. */
Interval
Note_column::cross_staff_extent (Grob *me, Grob *refp)
{
  Interval iv = me->extent (refp, Y_AXIS);
  if (Grob *s = get_stem (me))
    iv.unite (s->extent (refp, Y_AXIS));

  return iv;
}

ADD_INTERFACE (Note_column,
               R"(
Stem and noteheads combined.
               )",

               /* properties */
               R"(
force-hshift
horizontal-shift
ignore-collision
note-heads
main-extent
rest
rest-collision
stem
glissando-skip
               )");
