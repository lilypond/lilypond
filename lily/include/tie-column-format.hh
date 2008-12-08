/*
  tie-column-format.hh -- declare Tie column format routines.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
