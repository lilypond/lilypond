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

#include "no-callback.hh"

const char *const No_callback::type_p_name_ = "ly:no-callback?";

LY_DEFINE (ly_make_no_callback, "ly:make-no-callback", 1, 0, 0,
           (SCM proc),
           R"(
Mark function @var{proc} so that it will not get interpreted as
callback when stored in a grob property.
)")
{
  LY_ASSERT_TYPE (ly_is_procedure, proc, 1);

  return No_callback::make_smob (proc);
}
