/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "acceptance-set.hh"

void
Acceptance_set::accept (SCM item)
{
  // Ignore \accept C when C is already the default child.  It seems perfectly
  // sane for a user to express both, but it should have no practical effect.
  if (!scm_is_eq (item, default_))
    {
      // We do not bother deduplicating \accepts.  It would most often be a
      // waste of time because it is not likely that a user would \accept a
      // context that was already accepted.
      //
      // Resetting the priority is a reason to repeat an \accept intentionally.
      // If desired, explicit deduplication is an option: \denies C \accepts C.

      if (!scm_is_null (default_))
        {
          // insert the new item after the default
          SCM rest = scm_cdr (accepted_);
          scm_set_cdr_x (accepted_, scm_cons (item, rest));
        }
      else
        {
          accepted_ = scm_cons (item, accepted_);
        }
    }
}

void
Acceptance_set::accept_default (SCM item)
{
  accepted_ = scm_cons (item, scm_delete_x (item, accepted_));
  default_ = item;
}

void
Acceptance_set::deny (SCM item)
{
  accepted_ = scm_delete_x (item, accepted_);
  if (scm_is_eq (item, default_))
    default_ = SCM_EOL;
}
