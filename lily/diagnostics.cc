/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020 David Kastrup <dak@gnu.org>

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

#include "diagnostics.hh"
#include "input.hh"
#include "warn.hh"

void
Diagnostics::error (const std::string &arg) const
{
  ::error (arg, origin ());
}

void
Diagnostics::programming_error (const std::string &arg) const
{
  ::programming_error (arg, origin ());
}

void
Diagnostics::non_fatal_error (const std::string &arg) const
{
  ::non_fatal_error (arg, origin ());
}

void
Diagnostics::warning (const std::string &arg) const
{
  ::warning (arg, origin ());
}

void
Diagnostics::message (const std::string &arg) const
{
  ::message (arg, origin ());
}

void
Diagnostics::debug_output (const std::string &arg) const
{
  ::debug_output (arg, origin ());
}
