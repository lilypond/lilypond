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

#include "page-spacing-result.hh"

#include <cstdio>

Page_spacing_result::Page_spacing_result ()
{
  penalty_ = 0;
  demerits_ = infinity_f;
  system_count_status_ = SYSTEM_COUNT_OK;
}

vsize
Page_spacing_result::page_count () const
{
  return systems_per_page_.size ();
}

Real
Page_spacing_result::average_force () const
{
  Real average_force = 0;
  for (vsize i = 0; i < page_count (); i++)
    average_force += force_[i];

  average_force /= static_cast<Real> (page_count ());
  return average_force;
}

void
Page_spacing_result::print () const
{
  printf ("penalty %lf, demerits %lf\n", penalty_, demerits_);
  for (vsize i = 0; i < page_count (); i++)
    printf (" %d:  #sys=%d, force=%lf\n", int (i), int (systems_per_page_[i]),
            force_[i]);
}
