/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#ifndef MAIN_HH
#define MAIN_HH

#include "lily-proto.hh"
#include "std-string.hh"
#include "std-vector.hh"

void debug_init ();
void set_debug (bool);
void do_scores ();
void clear_scores ();
void add_score (Score *s);
void call_constructors ();
std::vector<std::string> get_inclusion_names ();
void set_inclusion_names (std::vector<std::string>);

extern std::string init_name_global;

/* options */
extern std::vector<std::string> dump_header_fieldnames_global;
extern std::vector<std::string> start_environment_global;
extern std::string output_backend_global;
extern std::string output_name_global;
extern bool be_safe_global;
extern bool do_internal_type_checking_global;
extern std::string lilypond_datadir;
extern bool use_object_keys;
extern bool strict_infinity_checking;
extern std::string init_scheme_code_global;
extern std::string init_scheme_variables_global;

/*
  todo: collect in Output_option struct?
*/
extern std::string output_format_global;

/* misc */
extern std::vector<std::string> failed_files;
extern int exit_status_global;
extern File_path global_path;
extern const char *LILYPOND_DATADIR;

/*
  Debugging options: switch on

  Cannot switch on -DNDEBUG, because it causes weird errors if you mix
  DNDEBUG and normal builds.
*/

#define DEBUG_SLUR_SCORING 1
#define DEBUG_TIE_SCORING 1
#define DEBUG_BEAM_SCORING 1

#endif /* MAIN_HH */
