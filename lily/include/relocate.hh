/*
  relocate.hh -- declare relocation functions

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef RELOCATE_HH
#define RELOCATE_HH

#include "std-string.hh"

void read_relocation_dir (string dirname);
void read_relocation_file (string filename);
string expand_environment_variables (string orig);

int sane_putenv (char const *key, string value, bool overwrite);
void setup_paths (char const *argv0);
extern bool relocate_binary;

#endif /* RELOCATE_HH */
