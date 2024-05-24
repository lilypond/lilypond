/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "time-tracer.hh"

#include "international.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <string_view>

using namespace std::literals;

LY_DEFINE (ly_time_tracer_restart, "ly:time-tracer-restart", 1, 0, 0,
           (SCM name),
           R"(
Reinitialize the global tracer in a child process to avoid interfering with the
parent's trace.  @var{name} is the name given to the top-level duration slice in
the new trace.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  tracer_global.log_first_begin_event (ly_scm2string (name));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_time_tracer_set_file, "ly:time-tracer-set-file", 1, 0, 0,
           (SCM file_name),
           R"(
Direct time-trace output to file @var{file-name}.  If @var{file-name} is
@code{#f}, disable tracing.
           )")
{
  if (!scm_is_false (file_name))
    {
      LY_ASSERT_TYPE (scm_is_string, file_name, 1);

      std::string f = ly_scm2string (file_name);
      if (!tracer_global.set_file (f.c_str ()))
        {
          error (_f ("failed redirecting time-trace output to `%s'", f.c_str ())
                   .c_str ());
        }
    }
  else
    {
      tracer_global.disable ();
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_time_tracer_stop, "ly:time-tracer-stop", 0, 0, 0, (),
           R"(
Finalize the global tracer.
           )")
{
  tracer_global.log_final_end_event ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_time_tracer_include_and_remove_file,
           "ly:time-tracer-include-and-remove-file", 1, 0, 0, (SCM file_name),
           R"(
Incorporate records from file @var{file-name} into the current trace.  If
successful, remove @var{file-name}.  This supports aggregating completed traces
from child processes into the parent's trace.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, file_name, 1);

  const auto f = ly_scm2string (file_name);
  auto trace_slice = tracer_global.log_scope (
    String_convert::form_string ("Include time-trace %s", f.c_str ()));

  if (tracer_global.include_file (f.c_str ()))
    {
      remove (f.c_str ());
    }

  return SCM_UNSPECIFIED;
}
