/*
  beam-settings.hh -- Prototypes for retrieving beam settings

  source file of the GNU LilyPond music typesetter

  Copyright (c) 2009 Carl Sorensen <c_sorensen@byu.edu>
*/

#ifndef BEAM_SETTINGS_HH
#define BEAM_SETTINGS_HH

#include "lily-guile.hh"

SCM ly_grouping_rules (SCM settings, SCM time_sig, SCM rule_type);
SCM ly_beam_grouping (SCM settings, SCM time_sig, SCM rule_type,
                      SCM beam_type);
SCM ly_beat_grouping (SCM context);
#endif // BEAM_SETTINGS_HH
