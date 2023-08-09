/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2023 Juergen Reuter <reuter@ipd.uka.de>

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

#include "tuplet-description.hh"

#include "spanner.hh"

const char *const Tuplet_description::type_p_name_ = "ly:tuplet-description?";

Tuplet_description::Tuplet_description (Stream_event *ev, const Moment &now)
  : event_ (ev)
{
  smobify_self ();
  start_moment_ = now;
  stop_moment_ = now + from_scm (get_property (ev, "length"), Moment (0));
  numerator_ = from_scm<unsigned> (get_property (ev, "numerator"));
  denominator_ = from_scm<unsigned> (get_property (ev, "denominator"));
}

Tuplet_description::~Tuplet_description ()
{
}

SCM
Tuplet_description::equal_p (SCM a, SCM b)
{
  return to_scm (*unsmob<Tuplet_description> (a)
                 == *unsmob<Tuplet_description> (b));
}

Rational const &
Tuplet_description::tuplet_start () const
{
  return start_moment_.grace_part_ || stop_moment_.grace_part_
           ? start_moment_.grace_part_
           : start_moment_.main_part_;
}
Rational const &
Tuplet_description::tuplet_stop () const
{
  return start_moment_.grace_part_ || stop_moment_.grace_part_
           ? stop_moment_.grace_part_
           : stop_moment_.main_part_;
}
Rational
Tuplet_description::tuplet_length () const
{
  return tuplet_stop () - tuplet_start ();
}

SCM
Tuplet_description::mark_smob () const
{
  scm_gc_mark (event_->self_scm ());
  if (bracket_)
    scm_gc_mark (bracket_->self_scm ());
  if (number_)
    scm_gc_mark (number_->self_scm ());
  if (parent_)
    scm_gc_mark (parent_->self_scm ());
  return SCM_EOL;
}

bool
operator== (Tuplet_description const &a, Tuplet_description const &b)
{
  return a.start_moment_ == b.start_moment_ && a.stop_moment_ == b.stop_moment_
         && a.parent_ == b.parent_ && a.numerator_ == b.numerator_
         && a.denominator_ == b.denominator_;
}
bool
operator!= (Tuplet_description const &a, Tuplet_description const &b)
{
  return !(a == b);
}
