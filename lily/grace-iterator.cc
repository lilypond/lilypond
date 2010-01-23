/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grace-iterator.hh"
#include "global-context.hh"
#include "warn.hh"

void
Grace_iterator::process (Moment m)
{
  Moment main;
  main.main_part_ = -start_mom_.grace_part_ + m.grace_part_;
  Music_wrapper_iterator::process (main);

  /* We can safely do this, since \grace should always be inside
     sequential.  */
  descend_to_child (child_iter_->get_outlet ());
}

Moment
Grace_iterator::pending_moment () const
{
  Moment cp = Music_wrapper_iterator::pending_moment ();

  Moment pending;
  pending.grace_part_ = start_mom_.grace_part_ + cp.main_part_;

  return pending;
}

IMPLEMENT_CTOR_CALLBACK (Grace_iterator);
