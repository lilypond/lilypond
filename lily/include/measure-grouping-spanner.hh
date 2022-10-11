/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MEASURE_GROUPING_SPANNER_HH
#define MEASURE_GROUPING_SPANNER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Measure_grouping
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif /* MEASURE_GROUPING_SPANNER_HH */
