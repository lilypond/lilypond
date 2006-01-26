/*
  relocate.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef RELOCATE_HH
#define RELOCATE_HH



#include "std-string.hh"

int sane_putenv (char const *key, std::string value, bool overwrite);
void setup_paths (char const *argv0);
extern bool relocate_binary;

#endif /* RELOCATE_HH */
