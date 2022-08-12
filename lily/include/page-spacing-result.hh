/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef PAGE_SPACING_RESULT_HH
#define PAGE_SPACING_RESULT_HH

#include "lily-proto.hh"
#include "real.hh"

#include <vector>

// This enum is a bitfield: since we use one System_count_status
// to represent the system count of several pages simultaneously,
// it could be that one page has too many systems while another
// has too few.
typedef enum
{
  SYSTEM_COUNT_OK = 0,
  SYSTEM_COUNT_TOO_MANY = 1,
  SYSTEM_COUNT_TOO_FEW = 2
} System_count_status;

struct Page_spacing_result
{
  std::vector<vsize> systems_per_page_;
  std::vector<Real> force_;
  Real penalty_;
  Real demerits_;
  int system_count_status_;

  Real average_force () const;
  vsize page_count () const;
  void print () const;
  Page_spacing_result ();
};

#endif /* PAGE_SPACING_RESULT_HH */
