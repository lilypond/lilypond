/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PROTECTED_SCM_HH
#define PROTECTED_SCM_HH

#include "lily-guile.hh"

/*
  Mix GUILE GC with C++ ctors and dtors.
*/
class Protected_scm
{
  SCM object_;
public:
  Protected_scm ();
  Protected_scm (SCM);
  Protected_scm (Protected_scm const &);
  ~Protected_scm ();
  Protected_scm &operator = (SCM);
  Protected_scm &operator = (Protected_scm const &);
  operator SCM () const;
  SCM to_SCM () const;
};

#endif /* PROTECTED_SCM_HH */
