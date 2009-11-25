/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TIE_COLUMN_FORMAT_HH
#define TIE_COLUMN_FORMAT_HH

#include "lily-proto.hh"
#include "tie-configuration.hh"

void set_chord_outline (Skyline *skyline,
			vector<Item*> bounds,
			Grob *common,
			Direction d);
void set_tie_config_directions (Ties_configuration *tie_configs_ptr);
void shift_small_ties (Ties_configuration *tie_configs,
		       Grob *staff_referencer,
		       Tie_details const &details);
void final_shape_adjustment (Tie_configuration &conf,
			     Tie_formatting_problem const&,
			     Grob *staff_referencer);
void
set_chord_outlines (Drul_array<Skyline> *skyline_drul,
		    vector<Grob*> ties,
		    Grob *common);

void
set_manual_tie_configuration (Ties_configuration *tie_configs,
			      bool *manual_override,
			      SCM manual_configs
			      );


#endif /* TIE_COLUMN_FORMAT_HH */
