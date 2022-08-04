/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "file-path.hh"

#include <string>
#include <vector>

extern std::string init_name_global;

/* options */
extern std::vector<std::string> dump_header_fieldnames_global;
extern std::string output_name_global;
extern bool do_internal_type_checking_global;
extern std::string lilypond_datadir;
extern std::string lilypond_libdir;
extern bool strict_infinity_checking;
extern std::string init_scheme_code_global;
extern std::string init_scheme_variables_global;

extern std::vector<std::string> output_formats_global;

/* misc */
extern File_path global_path;

#endif /* MAIN_HH */
