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

#include "item.hh"

#include "axis-group-interface.hh"
#include "paper-score.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "lily-guile.hh"
#include "system.hh"
#include "pointer-group-interface.hh"
#include "moment.hh"
#include "engraver.hh"

#include <cassert>

Item::Item (SCM s)
  : Grob (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] = 0;
  cached_pure_height_valid_ = false;
  add_interface (ly_symbol2scm ("item-interface"));
}

/**
   Item copy ctor.  Copy nothing: everything should be an elt property
   or a special purpose pointer (such as broken_to_drul_[]) */
Item::Item (Item const &s)
  : Grob (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] = 0;
  cached_pure_height_valid_ = false;
}

bool
Item::is_non_musical (Grob *me)
{
  Item *i = dynamic_cast<Item *> (me->get_x_parent ());
  return i ? Item::is_non_musical (i)
           : from_scm<bool> (get_property (me, "non-musical"));
}

Paper_column *
Item::get_column () const
{
  Item *parent = dynamic_cast<Item *> (get_x_parent ());
  return parent ? parent->get_column () : 0;
}

System *
Item::get_system () const
{
  Grob *g = get_x_parent ();
  return g ? g->get_system () : 0;
}

void
Item::break_breakable_item (System *sys)
{
  if (is_broken ())
    {
      programming_error ("item is already broken");
      return;
    }

  if (original ())
    {
      programming_error ("item is a clone; refusing to break");
      return;
    }

  if (Item::is_non_musical (this))
    {
      Drul_array<Item *> new_copies;
      for (const auto d : {LEFT, RIGHT})
        {
          Item *item = clone ();
          sys->typeset_grob (item);
          new_copies[d] = item;
        }

      broken_to_drul_ = new_copies;
    }
}

bool
Item::is_broken () const
{
  return broken_to_drul_[LEFT] || broken_to_drul_[RIGHT];
}

Item *
Item::find_broken_piece (System *l) const
{
  assert (l);
  if (get_system () == l)
    return const_cast<Item *> (this);

  for (const auto d : {LEFT, RIGHT})
    {
      Item *s = broken_to_drul_[d];
      if (s && s->get_system () == l)
        return s;
    }

  return 0;
}

Direction
Item::break_status_dir () const
{
  if (Item *i = original ())
    {
      return (i->broken_to_drul_[LEFT] == this) ? LEFT : RIGHT;
    }
  else
    return CENTER;
}

void
Item::handle_prebroken_dependencies ()
{
  Grob::handle_prebroken_dependencies ();
  if (!break_visible ())
    suicide ();
}

bool
Item::break_visible () const
{
  SCM vis = get_property (this, "break-visibility");
  if (scm_is_vector (vis))
    {
      const auto index = break_status_dir ().to_index ();
      return from_scm<bool> (scm_c_vector_ref (vis, index));
    }
  return true;
}

Item *
Item::pure_find_visible_prebroken_piece (vsize start, vsize end) const
{
  auto *it = const_cast<Item *> (this);

  // An Item "always" has a column, but it is possible for a grob to be killed
  // before its parents are set, so we have to be careful.
  const auto *col = get_column ();
  if (!col)
    return nullptr;

  const auto rank = static_cast<vsize> (col->get_rank ());
  if (rank == start)
    {
      it = find_prebroken_piece (RIGHT);
    }
  else if (rank == end)
    {
      it = find_prebroken_piece (LEFT);
    }

  return it->break_visible () ? it : nullptr;
}

bool
Item::internal_set_as_bound_of_spanner (Spanner *s, Direction)
{
  return s->accepts_as_bound_item (this);
}

Interval_t<int>
Item::spanned_column_rank_interval () const
{
  // An Item "always" has a column, but it is possible for a grob to be killed
  // before its parents are set, so we have to be careful.
  if (const auto *col = get_column ())
    {
      auto c = col->get_rank ();
      return Interval_t<int> (c, c);
    }
  return {};
}

System_rank_interval
Item::spanned_system_rank_interval () const
{
  if (System *st = get_system ())
    return System_rank_interval (st->get_rank (), st->get_rank ());

  System_rank_interval sr;
  for (const auto d : {LEFT, RIGHT})
    {
      Item *bi = find_prebroken_piece (d);
      if (bi && bi->get_system ())
        sr.add_point (bi->get_system ()->get_rank ());
    }

  return sr;
}

Interval_t<Moment>
spanned_time_interval (Item *l, Item *r)
{
  Drul_array<Item *> bounds (l, r);
  Interval_t<Moment> iv;

  for (const auto d : {LEFT, RIGHT})
    {
      if (bounds[d] && bounds[d]->get_column ())
        {
          iv[d]
            = from_scm (get_property (bounds[d]->get_column (), "when"), iv[d]);
        }
    }

  for (const auto d : {LEFT, RIGHT})
    {
      if (!bounds[d] || !bounds[d]->get_column ())
        iv[d] = iv[-d];
    }

  return iv;
}

Interval
Item::pure_y_extent (Grob *g, vsize start, vsize end)
{
  if (cached_pure_height_valid_)
    return cached_pure_height_ + pure_relative_y_coordinate (g, start, end);
  /* Note: cached_pure_height_ does not notice if start changes, implicitly
     assuming that Items' pure_heights do not depend on 'start' or 'end'.
  */

  cache_pure_height (Grob::pure_y_extent (this, start, end));
  return cached_pure_height_ + pure_relative_y_coordinate (g, start, end);
}

void
Item::cache_pure_height (Interval height)
{
  cached_pure_height_ = height;
  cached_pure_height_valid_ = true;
}

Item *
Item::make_sticky_same_type (Engraver *eng, SCM type, SCM cause,
                             char const *file, int line, char const *fun)
{
  Item *g = eng->internal_make_item (type, cause, file, line, fun);
  return g;
}

ADD_INTERFACE (Item,
               R"(
Grobs can be distinguished in their role in the horizontal spacing.  Many grobs
define constraints on the spacing by their sizes, for example, note heads,
clefs, stems, and all other symbols with a fixed shape.  These grobs form a
subtype called @code{Item}.

Some items need special treatment for line breaking.  For example, a clef is
normally only printed at the start of a line (i.e., after a line break).   To
model this, @q{breakable} items (clef, key signature, bar lines, etc.) are
copied twice.  Then we have three versions of each breakable item: one version
if there is no line break, one version that is printed before the line break
(at the end of a system), and one version that is printed after the line break.

Whether these versions are visible and take up space is determined by the
outcome of the @code{break-visibility} grob property, which is a function
taking a direction (@w{@code{-1}}, @code{0} or@tie{}@code{1}) as an argument.
It returns a cons of booleans, signifying whether this grob should be
transparent and have no extent.

The following variables for @code{break-visibility} are predefined:
@example
           grob will show:   before  no     after
                             break   break  break
  all-invisible              no      no     no
  begin-of-line-visible      no      no     yes
  end-of-line-visible        yes     no     no
  all-visible                yes     yes    yes
  begin-of-line-invisible    yes     yes    no
  end-of-line-invisible      no      yes    yes
  center-invisible           yes      no    yes
@end example
               )",

               /* properties */
               R"(
break-visibility
extra-spacing-height
extra-spacing-width
non-musical
               )");
