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

#ifndef DIAGNOSTICS_HH
#define DIAGNOSTICS_HH

#include "lily-proto.hh"
#include <string>

// Derive from this class and provide an origin () function in order
// to gain a full set of diagnostic issuing calls.
class Diagnostics
{
protected:
  virtual Input *origin () const = 0;

public:
  [[noreturn]] void error (const std::string &) const;
  void programming_error (const std::string &) const;
  void non_fatal_error (const std::string &) const;
  void warning (const std::string &) const;
  void message (const std::string &) const;
  void debug_output (const std::string &) const;
};

#endif
