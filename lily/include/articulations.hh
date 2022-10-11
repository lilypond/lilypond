/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Carl Sorensen <c_sorensen@byu.edu>

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

#ifndef ARTICULATIONS_HH
#define ARTICULATIONS_HH

#include "lily-guile.hh"
#include "stream-event.hh"

#include <vector>

SCM articulation_list (std::vector<Stream_event *> notes,
                       std::vector<Stream_event *> articulations,
                       char const *articulation_name);

#endif /* ARTICULATIONS_HH */
