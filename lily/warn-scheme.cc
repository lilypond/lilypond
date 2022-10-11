/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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
#include "international.hh"
#include "warn.hh"

using std::string;

/*
  Error / warning / progress / debug message output functions
*/

LY_DEFINE (ly_error, "ly:error", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to issue the error @var{str}.  The error is
formatted with @code{format}; @var{rest} holds the formatting arguments (if
any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_non_fatal_error, "ly:non-fatal-error", 1, 0, 1,
           (SCM str, SCM rest),
           R"(
A Scheme callable function to issue the error @var{str}.  The error is
formatted with @code{format}; @var{rest} holds the formatting
arguments (if any).  When using this function, some way of signalling
the error should be employed in order for the compilation to
eventually result in a nonzero return code.
)")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  non_fatal_error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_programming_error, "ly:programming-error", 1, 0, 1,
           (SCM str, SCM rest),
           R"(
A Scheme callable function to issue the internal warning @var{str}.  The
message is formatted with @code{format}; @var{rest} holds the formatting
arguments (if any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  programming_error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_warning, "ly:warning", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to issue the warning @var{str}.  The message is
formatted with @code{format}; @var{rest} holds the formatting arguments (if
any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  warning (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_progress, "ly:progress", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to print progress @var{str}.  The message is
formatted with @code{format}; @var{rest} holds the formatting arguments (if
any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  // Calls to ly:progress should in general not start a new line
  progress_indication (ly_scm2string (str), false);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_basic_progress, "ly:basic-progress", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to issue a basic progress message @var{str}.  The
message is formatted with @code{format}; @var{rest} holds the formatting
arguments (if any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  basic_progress (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_message, "ly:message", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to issue the message @var{str}.  The message is
formatted with @code{format}; @var{rest} holds the formatting arguments (if
any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  message (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_debug, "ly:debug", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to issue a debug message @var{str}.  The message is
formatted with @code{format}; @var{rest} holds the formatting arguments (if
any).
           )")
{
  // TODO: Add the newline flag!
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  debug_output (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_warning_located, "ly:warning-located", 2, 0, 1,
           (SCM location, SCM str, SCM rest),
           R"(
A Scheme callable function to issue the warning @var{str} at the specified
location in an input file.  The message is formatted with @code{format};
@var{rest} holds the formatting arguments (if any).
           )")
{
  LY_ASSERT_TYPE (scm_is_string, location, 1);
  LY_ASSERT_TYPE (scm_is_string, str, 2);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  warning (ly_scm2string (str), ly_scm2string (location));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_expect_warning, "ly:expect-warning", 1, 0, 1, (SCM str, SCM rest),
           R"(
A Scheme callable function to register a warning to be expected and
subsequently suppressed.  If the warning is not encountered, a warning about
the missing warning is shown.  The message should be translated with @code{(_
...)} and changing parameters given after the format string.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  expect_warning (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_check_expected_warnings, "ly:check-expected-warnings", 0, 0, 0,
           (),
           R"(
Check whether all expected warnings have really been triggered.
           )")
{
  check_expected_warnings ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_translate_cpp_warning_scheme, "ly:translate-cpp-warning-scheme",
           1, 0, 0, (SCM str),
           R"(
Translate a string in C++ @code{printf} format and modify it to use it for
Scheme formatting.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);
  string s = _ (ly_scm2string (str).c_str ());

  /* Now replace all printf placeholders by scheme placeholders (~a).
   * Guile's format syntax is pretty similar to C's printf, only with
   * a tilde as the placeholder instead of a percent sign.
   * There is no easy way to replace all ~ -> ~~, %% -> %, % -> ~,
   * so simply walk through each character.
   */
  //   size_t pos = 0;
  const char *pos = s.c_str ();
  string result = "";
  while (*pos != '\0')
    {
      // In some cases (%%, %s) we need to do a lookahead. As the C string is
      // always \0-terminated the next char is never beyond the end of the
      // memory!
      switch (*pos)
        {
        case '~':
          result += "~~";
          break;
        case '%':
          if (*(pos + 1) == '%')
            {
              result += "%";
              // Skip the second '%'
              pos++;
            }
          else if (*(pos + 1) == 's' || *(pos + 1) == 'd')
            {
              // %s in C++ corresponds to ~a; ~s would add quotes!
              // ~d is only supported by ice-9, use ~a instead
              result += "~a";
              // Skip the following 's'
              pos++;
            }
          else
            result += "~";
          break;
        default:
          result += *pos;
        }
      pos++;
    }
  return ly_string2scm (result);
}
