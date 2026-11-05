/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2026 David Kastrup <dak@gnu.org>

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

#ifndef NO_CALLBACK_HH
#define NO_CALLBACK_HH

#include "small-smobs.hh"

class No_callback : public Smob1<No_callback>
{
public:
  static const char *const type_p_name_;
  SCM call (SCM args) { return scm_apply_0 (scm1 (), args); }
  LY_DECLARE_SMOB_PROC (&No_callback::call, 0, 0, 1);
};
  

#endif
