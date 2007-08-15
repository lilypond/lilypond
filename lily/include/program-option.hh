/*
  program-option.hh -- declare Scheme options

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef SCM_OPTION_HH
#define SCM_OPTION_HH

#include "lily-guile.hh"

/* options */
extern bool do_midi_debugging_global;
extern int testing_level_global;
extern bool lily_1_8_relative;
extern bool lily_1_8_compatibility_used;
extern bool profile_property_accesses;

SCM ly_get_option (SCM);
SCM ly_set_option (SCM, SCM);

bool get_program_option (const char *);

#endif /* SCM_OPTION_HH */
