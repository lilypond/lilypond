/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "std-string.hh"
#include "input.hh"

using std::string;

LY_DEFINE (ly_input_warning, "ly:input-warning", 2, 0, 1,
           (SCM sip, SCM msg, SCM rest),
           R"(
Print @var{msg} as a GNU compliant warning message, pointing to the location in
@var{sip}.  @var{msg} is interpreted similar to @code{format}'s argument, using
@var{rest}.
           )")
{
  auto *const ip = LY_ASSERT_SMOB (Input, sip, 1);
  LY_ASSERT_TYPE (scm_is_string, msg, 2);

  msg = scm_simple_format (SCM_BOOL_F, msg, rest);

  string m = ly_scm2string (msg);
  ip->warning (m);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_input_message, "ly:input-message", 2, 0, 1,
           (SCM sip, SCM msg, SCM rest),
           R"(
Print @var{msg} as a GNU compliant error message, pointing to the location in
@var{sip}.  @var{msg} is interpreted similar to @code{format}'s argument, using
@var{rest}.
           )")
{
  auto *const ip = LY_ASSERT_SMOB (Input, sip, 1);
  LY_ASSERT_TYPE (scm_is_string, msg, 2);

  msg = scm_simple_format (SCM_BOOL_F, msg, rest);

  string m = ly_scm2string (msg);
  ip->message (m);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_input_file_line_char_column, "ly:input-file-line-char-column", 1,
           0, 0, (SCM sip),
           R"(
Return input location in @var{sip} as @code{(file-name line char column)}.
           )")
{
  auto *const ip = LY_ASSERT_SMOB (Input, sip, 1);

  ssize_t l, ch, col, offset = 0;
  ip->get_counts (&l, &ch, &col, &offset);
  return ly_list (ly_string2scm (ip->file_string ()), scm_from_ssize_t (l),
                  scm_from_ssize_t (ch), scm_from_ssize_t (col));
}

LY_DEFINE (ly_input_both_locations, "ly:input-both-locations", 1, 0, 0,
           (SCM sip),
           R"(
Return input location in @var{sip} as
@example
(file-name first-line first-column last-line last-column)
@end example
           )")
{

  auto *const ip = LY_ASSERT_SMOB (Input, sip, 1);

  return ly_list (ly_string2scm (ip->file_string ()),
                  scm_from_ssize_t (ip->line_number ()),
                  scm_from_ssize_t (ip->column_number ()),
                  scm_from_ssize_t (ip->end_line_number ()),
                  scm_from_ssize_t (ip->end_column_number ()));
}
