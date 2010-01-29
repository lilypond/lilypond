/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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


Grob *
Item::clone () const
{
  return new Item (*this);
}

Item::Item (SCM s)
  : Grob (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] = 0;
  cached_pure_height_valid_ = false;
}

/**
   Item copy ctor.  Copy nothing: everything should be a elt property
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
  if (me->original ())
    return false;

  Item *i = dynamic_cast<Item *> (me->get_parent (X_AXIS));
  return i ? Item::is_non_musical (i) : to_boolean (me->get_property ("non-musical"));
}

Paper_column *
Item::get_column () const
{
  Item *parent = dynamic_cast<Item *> (get_parent (X_AXIS));
  return parent ? parent->get_column () : 0;
}

System *
Item::get_system () const
{
  Grob *g = get_parent (X_AXIS);
  return g ? g->get_system () : 0;
}

void
Item::copy_breakable_items ()
{
  Drul_array<Item *> new_copies;
  Direction i = LEFT;
  do
    {
      Grob *dolly = clone ();
      Item *item = dynamic_cast<Item *> (dolly);
      get_root_system (this)->typeset_grob (item);
      new_copies[i] = item;
    }
  while (flip (&i) != LEFT);

  broken_to_drul_ = new_copies;
}

bool
Item::is_broken () const
{
  return broken_to_drul_[LEFT] || broken_to_drul_[RIGHT];
}

/*
  Generate items for begin and end-of line.
*/
void
Item::discretionary_processing ()
{
  if (is_broken ())
    return;

  if (Item::is_non_musical (this))
    copy_breakable_items ();
}

Grob *
Item::find_broken_piece (System *l) const
{
  if (get_system () == l)
    return (Item *) (this);

  Direction d = LEFT;
  do
    {
      Grob *s = broken_to_drul_[d];
      if (s && s->get_system () == l)
	return s;
    }
  while (flip (&d) != LEFT);

  return 0;
}

Item *
Item::find_prebroken_piece (Direction d) const
{
  Item *me = (Item *) (this);
  if (!d)
    return me;
  return dynamic_cast<Item *> (broken_to_drul_[d]);
}

Direction
Item::break_status_dir () const
{
  if (original ())
    {
      Item *i = dynamic_cast<Item *> (original ());

      return (i->broken_to_drul_[LEFT] == this) ? LEFT : RIGHT;
    }
  else
    return CENTER;
}

void
Item::handle_prebroken_dependencies ()
{
  Grob::handle_prebroken_dependencies ();

  /*
    Can't do this earlier, because try_visibility_lambda () might set
    the elt property transparent, which would then be copied.
  */
  if (!Item::break_visible (this))
    suicide ();
}

bool
Item::break_visible (Grob *g)
{
  Item *it = dynamic_cast<Item*> (g);
  SCM vis = g->get_property ("break-visibility");
  if (scm_is_vector (vis))
    return to_boolean (scm_c_vector_ref (vis, it->break_status_dir () + 1));
  return true;
}

bool
Item::pure_is_visible (int start, int end) const
{
  SCM vis = get_property ("break-visibility");
  if (scm_is_vector (vis))
    {
      int pos = 1;
      int pc_rank = Paper_column::get_rank (get_column ());
      if (pc_rank == start)
	pos = 2;
      else if (pc_rank == end)
	pos = 0;
      return to_boolean (scm_vector_ref (vis, scm_from_int (pos)));
    }
  return true;
}

Interval_t<int>
Item::spanned_rank_interval () const
{
  int c = get_column ()->get_rank ();
  return Interval_t<int> (c, c);
}

Interval_t<Moment>
spanned_time_interval (Item *l, Item *r) 
{
  Drul_array<Item*> bounds (l, r);
  Interval_t<Moment> iv;

  Direction d = LEFT;
  do
    {
      if (bounds[d] && bounds[d]->get_column ())
	iv[d] = robust_scm2moment (bounds[d]->get_column ()->get_property ("when"),
				  iv[d]);
    }
  while (flip (&d) != LEFT);

  do
    {
      if (!bounds[d] || !bounds[d]->get_column ())
	iv[d] = iv[-d];
    }
  while (flip (&d) != LEFT);
  
  
  return iv;
}


void
Item::derived_mark () const
{
  if (broken_to_drul_[LEFT])
    scm_gc_mark (broken_to_drul_[LEFT]->self_scm ());
  if (broken_to_drul_[RIGHT])
    scm_gc_mark (broken_to_drul_[RIGHT]->self_scm ());
}

Item *
unsmob_item (SCM s)
{
  return dynamic_cast<Item *> (unsmob_grob (s));
}

Interval
Item::pure_height (Grob *g, int start, int end)
{
  if (cached_pure_height_valid_)
    return cached_pure_height_ + pure_relative_y_coordinate (g, start, end);

  cached_pure_height_ = Grob::pure_height (this, start, end);
  cached_pure_height_valid_ = true;
  return cached_pure_height_ + pure_relative_y_coordinate (g, start, end);
}

bool
Item::less (Grob * const &g1, Grob * const &g2)
{
  return dynamic_cast<Item*> (g1)->get_column ()->get_rank () < dynamic_cast<Item*> (g2)->get_column ()->get_rank ();
}

ADD_INTERFACE (Item,
	       "Grobs can be distinguished in their role in the horizontal"
	       " spacing.  Many grobs define constraints on the spacing by"
	       " their sizes, for example, note heads, clefs, stems, and all"
	       " other symbols with a fixed shape.  These grobs form a"
	       " subtype called @code{Item}.\n"
	       "\n"
	       "Some items need special treatment for line breaking.  For"
	       " example, a clef is normally only printed at the start of a"
	       " line (i.e., after a line break).   To model this,"
	       " @q{breakable} items (clef, key signature, bar lines, etc.)"
	       " are copied twice.  Then we have three versions of each"
	       " breakable item: one version if there is no line break, one"
	       " version that is printed before the line break (at the end of"
	       " a system), and one version that is printed after the line"
	       " break.\n"
	       "\n"
	       "Whether these versions are visible and take up space is"
	       " determined by the outcome of the @code{break-visibility}"
	       " grob property, which is a function taking a direction"
	       " (@code{-1}, @code{0} or@tie{}@code{1}) as an argument.  It"
	       " returns a cons of booleans, signifying whether this grob"
	       " should be transparent and have no extent.\n"
	       "\n"
	       "The following variables for @code{break-visibility} are"
	       " predefined:\n"
	       "@example\n"
	       "           grob will show:   before  no     after\n"
	       "                             break   break  break\n"
	       "  all-invisible              no      no     no\n"
	       "  begin-of-line-visible      no      no     yes\n"
	       "  end-of-line-visible        yes     no     no\n"
	       "  all-visible                yes     yes    yes\n"
	       "  begin-of-line-invisible    yes     yes    no\n"
	       "  end-of-line-invisible      no      yes    yes\n"
	       "  center-invisible           yes      no    yes\n"
	       "@end example",

	       /* properties */
	       "break-visibility "
	       "extra-spacing-height "
	       "extra-spacing-width "
	       "non-musical "
	       );
