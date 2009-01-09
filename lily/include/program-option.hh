/*
  program-option.hh -- declare Scheme options

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef SCM_OPTION_HH
#define SCM_OPTION_HH

#include "lily-guile.hh"

/* options */
extern bool lily_1_8_relative;
extern bool lily_1_8_compatibility_used;

SCM ly_get_option (SCM);
SCM ly_set_option (SCM, SCM);

bool get_program_option (const char *);
string get_output_backend_name ();

#endif /* SCM_OPTION_HH */
