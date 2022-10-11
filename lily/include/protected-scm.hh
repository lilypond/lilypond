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

#ifndef PROTECTED_SCM_HH
#define PROTECTED_SCM_HH

#include "lily-guile.hh"

/*
  Mix GUILE GC with C++ ctors and dtors.

  This version is intended to be only used for variables of static
  lifetime (which are not feasible to protect using the per-instance
  mechanism of the smob classes) but possibly varying content.  As a
  result, the protection mechanism being used is the irreversible
  scm_permanent_object.  The destructor (typically called after the
  end of program execution) does not free resources and consequently
  does not require the memory subsystem to be still workable.  A
  working memory subsystem is only required once a non-immediate
  Scheme value is assigned to the variable.  Since creation of a
  non-immediate Scheme value requires a working memory subsystem in
  the first place, this is not really a restriction.

  To avoid accidental creation of temporaries, the copy constructor is
  made unavailable.
*/
class Protected_scm
{
  SCM object_;
  static SCM list_;
  static SCM last_;
  Protected_scm (Protected_scm const &);
  void protectify (SCM);

public:
  Protected_scm ();
  Protected_scm (SCM);
  Protected_scm &operator= (SCM);
  Protected_scm &operator= (Protected_scm const &);
  operator const SCM & () const;
  operator SCM & ();
  bool is_bound () const; // SCM_UNBNDP balks at Protected_scm
};

#endif /* PROTECTED_SCM_HH */
