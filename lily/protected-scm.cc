/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

// For static objects, this will be called at program exit.  With the
// state of the memory system unknown, we refrain from any cleanup
// actions outside of the object memory itself.

Protected_scm::~Protected_scm ()
{
  object_ = SCM_UNDEFINED;
}

SCM Protected_scm::list_ = SCM_EOL;
SCM Protected_scm::last_ = SCM_EOL;

Protected_scm &
Protected_scm::operator = (SCM s)
{
  if (SCM_CONSP (object_))
    SCM_SETCAR (object_, s);
  else if (SCM_IMP (s))
    object_ = s;
  else
    {
      s = scm_list_1 (s);
      if (SCM_CONSP (last_))
        SCM_SETCDR (last_, s);
      else
        list_ = scm_permanent_object (s);
      last_ = object_ = s;
    }

  return *this;
}

Protected_scm &
Protected_scm::operator = (Protected_scm const &s)
{
  return *this = (SCM) s;
}

Protected_scm::operator SCM () const
{
  return SCM_CONSP (object_) ? SCM_CAR (object_) : object_;
}
