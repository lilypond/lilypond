/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "input.hh"
#include "source-file.hh"
#include "std-string.hh"

using std::string;

/* Dummy input location for use if real one is missing.  */
Input dummy_input_global;

const char *const Input::type_p_name_ = "ly:input-location?";

SCM
Input::mark_smob () const
{
  if (Source_file *sf = get_source_file ())
    return sf->self_scm ();

  return SCM_EOL;
}

int
Input::print_smob (SCM port, scm_print_state *) const
{
  string str = "#<location " + location_string () + ">";
  scm_puts (str.c_str (), port);
  return 1;
}

SCM
Input::equal_p (SCM sa, SCM sb)
{
  Input *a = unsmob<Input> (sa);
  Input *b = unsmob<Input> (sb);
  if (a->get_source_file () == b->get_source_file ()
      && a->start () == b->start () && a->end () == b->end ())
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
