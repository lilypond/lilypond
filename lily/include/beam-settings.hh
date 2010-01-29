/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2010 Carl Sorensen <c_sorensen@byu.edu>

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

#ifndef BEAM_SETTINGS_HH
#define BEAM_SETTINGS_HH

#include "lily-guile.hh"

SCM ly_grouping_rules (SCM settings, SCM time_sig, SCM rule_type);
SCM ly_beam_grouping (SCM settings, SCM time_sig, SCM rule_type,
                      SCM beam_type);
SCM ly_beat_grouping (SCM context);
#endif // BEAM_SETTINGS_HH
