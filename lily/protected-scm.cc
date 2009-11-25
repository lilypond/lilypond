/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

Protected_scm::Protected_scm ()
{
  object_ = SCM_UNDEFINED;
}

Protected_scm::Protected_scm (SCM s)
{
  object_ = SCM_NIMP (s) ? scm_gc_protect_object (s) : s;
}

Protected_scm::Protected_scm (Protected_scm const &s)
{
  object_ = (SCM_NIMP (s.object_) ? scm_gc_protect_object (s.object_)
	     : s.object_);
}

Protected_scm::~Protected_scm ()
{
  if (SCM_NIMP (object_))
    scm_gc_unprotect_object (object_);
}

Protected_scm &
Protected_scm::operator = (SCM s)
{
  if (object_ == s)
    return *this;

  if (SCM_NIMP (object_))
    scm_gc_unprotect_object (object_);

  object_ = SCM_NIMP (s) ? scm_gc_protect_object (s) : s;
  return *this;
}

Protected_scm &
Protected_scm::operator = (Protected_scm const &s)
{
  return operator = (s.object_);
}

Protected_scm::operator SCM () const
{
  return object_;
}

SCM
Protected_scm::to_SCM () const
{
  return object_;
}
