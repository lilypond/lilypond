/* 
  program-option.cc -- program options, non-scheme.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007--2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "program-option.hh"

string
get_output_backend_name ()
{
  return ly_symbol2string (ly_get_option (ly_symbol2scm ("backend")));
}

bool
get_program_option (const char *s)
{
  SCM sym = ly_symbol2scm (s);

  return to_boolean (ly_get_option (sym));
}

