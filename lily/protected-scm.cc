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

#include "protected-scm.hh"

// We store the data in the car of a cons cell: it is faster to keep
// only one object protected during the life time of Protected_scm
// than several.

Protected_scm::Protected_scm ()
  : object_ (SCM_UNDEFINED)
{
}

Protected_scm::Protected_scm (SCM s)
  : object_ (s)
{
  // Only allow immediate objects at construction time.  Protected_scm
  // is intended for variables of static duration, and creating
  // non-immediate objects when GUILE is not yet up is a bad idea.
  assert (SCM_IMP (s));
}

SCM Protected_scm::list_ = SCM_EOL;
SCM Protected_scm::last_ = SCM_EOL;

void
Protected_scm::protectify (SCM s)
{
  s = ly_list (s);
  if (SCM_CONSP (last_))
    SCM_SETCDR (last_, s);
  else
    list_ = scm_permanent_object (s);
  last_ = object_ = s;
}

Protected_scm &
Protected_scm::operator= (SCM s)
{
  if (SCM_CONSP (object_))
    SCM_SETCAR (object_, s);
  else if (SCM_IMP (s))
    object_ = s;
  else
    protectify (s);

  return *this;
}

Protected_scm &
Protected_scm::operator= (Protected_scm const &s)
{
  return *this = static_cast<SCM> (s);
}

Protected_scm::operator SCM const & () const
{
  if (SCM_CONSP (object_))
    return *SCM_CARLOC (object_);
  return object_;
}

Protected_scm::operator SCM & ()
{
  // The reference may be used to overwrite an immediate value with a
  // non-immediate one, so we _have_ to create full protection.
  if (!SCM_CONSP (object_))
    protectify (object_);

  return *SCM_CARLOC (object_);
}

bool
Protected_scm::is_bound () const
{
  if (SCM_CONSP (object_))
    return !SCM_UNBNDP (SCM_CAR (object_));
  return !SCM_UNBNDP (object_);
}
