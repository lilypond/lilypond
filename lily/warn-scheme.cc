/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2011 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "config.hh"

#include "lily-guile.hh"
#include "program-option.hh"
#include "version.hh"
#include "warn.hh"

/*
  Error / warning / progress / debug message output functions
*/

LY_DEFINE (ly_error, "ly:error",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue the error @var{str}."
           "  The error is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_programming_error, "ly:programming-error",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue the internal warning"
           "  @var{str}.  The message is formatted with @code{format}"
           " and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);

  if (get_program_option ("warning-as-error"))
    error (ly_scm2string (str));
  else
    programming_error (ly_scm2string (str));

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_warning, "ly:warning",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue the warning @var{str}."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);

  if (get_program_option ("warning-as-error"))
    error (ly_scm2string (str));
  else
    warning (ly_scm2string (str));

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_progress, "ly:progress",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to print progress @var{str}."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  // Calls to ly:progress should in general not start a new line
  progress_indication (ly_scm2string (str), false);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_success, "ly:success",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue a success message @var{str}."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  successful (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_message, "ly:message",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue the message @var{str}."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  message (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_debug, "ly:debug",
           1, 0, 1, (SCM str, SCM rest),
           "A Scheme callable function to issue a debug message @var{str}."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  // TODO: Add the newline flag!
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  debug_output (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_warning_located, "ly:warning-located",
           2, 0, 1, (SCM location, SCM str, SCM rest),
           "A Scheme callable function to issue the warning @var{str} at"
           " the specified location in an input file."
           "  The message is formatted with @code{format} and @var{rest}.")
{
  LY_ASSERT_TYPE (scm_is_string, location, 1);
  LY_ASSERT_TYPE (scm_is_string, str, 2);
  str = scm_simple_format (SCM_BOOL_F, str, rest);

  if (get_program_option ("warning-as-error"))
    error (ly_scm2string (str), ly_scm2string (location));
  else
    warning (ly_scm2string (str), ly_scm2string (location));

  return SCM_UNSPECIFIED;
}
