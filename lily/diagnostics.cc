/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 David Kastrup <dak@gnu.org>

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

[[noreturn]] void
Diagnostics::error (const std::string &arg) const
{
  if (Input *input = origin ())
    input->error (arg);
  else
    ::error (arg);
}

void
Diagnostics::programming_error (const std::string &arg) const
{
  if (Input *input = origin ())
    input->programming_error (arg);
  else
    ::programming_error (arg);
}

void
Diagnostics::non_fatal_error (const std::string &arg) const
{
  if (Input *input = origin ())
    input->non_fatal_error (arg);
  else
    ::non_fatal_error (arg);
}

void
Diagnostics::warning (const std::string &arg) const
{
  if (Input *input = origin ())
    input->warning (arg);
  else
    ::warning (arg);
}

void
Diagnostics::message (const std::string &arg) const
{
  if (Input *input = origin ())
    input->message (arg);
  else
    ::message (arg);
}

void
Diagnostics::debug_output (const std::string &arg) const
{
  if (Input *input = origin ())
    input->debug_output (arg);
  else
    ::debug_output (arg);
}
