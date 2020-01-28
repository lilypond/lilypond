/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cmath> // ceil

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

/*
  TODO: figure out if we can prune this class. This is just an
  annoying layer between (rest)collision & (note-head + stem)
*/

bool
Note_column::has_rests (Grob *me)
{
  return unsmob<Grob> (me->get_object ("rest"));
}

bool
Note_column::shift_less (Grob *const &p1, Grob *const &p2)
{
  SCM s1 = p1->get_property ("horizontal-shift");
  SCM s2 = p2->get_property ("horizontal-shift");

  int h1 = (scm_is_number (s1)) ? scm_to_int (s1) : 0;
  int h2 = (scm_is_number (s2)) ? scm_to_int (s2) : 0;
  return h1 < h2;
}

Item *
Note_column::get_stem (Grob *me)
{
  SCM s = me->get_object ("stem");
  return unsmob<Item> (s);
}

Item *
Note_column::get_flag (Grob *me)
{
  Item *stem = get_stem (me);
  if (stem)
    {
      SCM s = stem->get_object ("flag");
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
  Grob *stem = unsmob<Grob> (me->get_object ("stem"));
  if (has_interface<Stem> (stem))
    return get_grob_direction (stem);
  else
    {
      extract_grob_set (me, "note-heads", heads);
      if (heads.size ())
        return (Direction)sign (head_positions_interval (me).center ());
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
  me->set_object ("stem", stem->self_scm ());
  Axis_group_interface::add_element (me, stem);
}

Grob *
Note_column::get_rest (Grob *me)
{
  return unsmob<Grob> (me->get_object ("rest"));
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
        me->set_object ("rest", h->self_scm ());
    }
  else if (has_interface<Note_head> (h))
    {
      if (unsmob<Grob> (me->get_object ("rest")))
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

/*
  Return extent of the noteheads in the "main column",
  (i.e. excluding any suspended noteheads), or extent
  of the rest (if there are no heads).
*/
Interval
Note_column::calc_main_extent (Grob *me)
{
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
      = main_head ? main_head : unsmob<Grob> (me->get_object ("rest"));

  return main_item ? main_item->extent (me, X_AXIS) : Interval (0, 0);
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
      acc = h ? unsmob<Grob> (h->get_object ("accidental-grob")) : 0;
      if (acc)
        break;
    }

  if (!acc)
    return 0;

  if (has_interface<Accidental_placement> (acc->get_parent (X_AXIS)))
    return acc->get_parent (X_AXIS);

  /* compatibility. */
  return acc;
}

Grob *
Note_column::dot_column (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  for (vsize i = 0; i < heads.size (); i++)
    {
      Grob *dots = unsmob<Grob> (heads[i]->get_object ("dot"));
      if (dots)
        return dots->get_parent (X_AXIS);
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

ADD_INTERFACE (Note_column, "Stem and noteheads combined.",

               /* properties */
               "force-hshift "
               "horizontal-shift "
               "ignore-collision "
               "note-heads "
               "rest "
               "rest-collision "
               "stem "
               "glissando-skip ");
