/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

/* We don't use IMPLEMENT_TYPE_P, since the smobification part is
   implemented separately from the class.  */
LY_DEFINE (ly_input_location_p, "ly:input-location?", 1, 0, 0,
	   (SCM x),
	   "Is @var{x} an @code{input-location}?")
{
  return unsmob_input (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

LY_DEFINE (ly_input_message, "ly:input-message", 2, 0, 1, (SCM sip, SCM msg, SCM rest),
	   "Print @var{msg} as a GNU compliant error message, pointing"
	   " to the location in @var{sip}.  @var{msg} is interpreted"
	   " similar to @code{format}'s argument, using @var{rest}.")
{
  Input *ip = unsmob_input (sip);

  LY_ASSERT_TYPE (unsmob_input, sip, 1);
  LY_ASSERT_TYPE (scm_is_string, msg,2);

  msg = scm_simple_format (SCM_BOOL_F, msg, rest);

  string m = ly_scm2string (msg);
  ip->message (m);

  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_input_file_line_char_column,
	   "ly:input-file-line-char-column",
	   1, 0, 0, (SCM sip),
	   "Return input location in @var{sip} as"
	   " @code{(file-name line char column)}.")
{
  LY_ASSERT_TYPE (unsmob_input, sip, 1);
  Input *ip = unsmob_input (sip);

  int l, ch, col, offset = 0;
  ip->get_counts (&l, &ch, &col, &offset);
  return scm_list_4 (ly_string2scm (ip->file_string ()),
		     scm_from_int (l),
		     scm_from_int (ch),
		     scm_from_int (col));
}

LY_DEFINE (ly_input_both_locations,
	   "ly:input-both-locations",
	   1, 0, 0, (SCM sip),
	   "Return input location in @var{sip} as"
	   " @code{(file-name first-line first-column last-line last-column)}.")
{
  
  LY_ASSERT_TYPE (unsmob_input, sip, 1);
  Input *ip = unsmob_input (sip);
  
  return scm_list_5 (ly_string2scm (ip->file_string ()),
		     scm_from_int (ip->line_number ()),
		     scm_from_int (ip->column_number ()),
		     scm_from_int (ip->end_line_number ()),
		     scm_from_int (ip->end_column_number ()));
}
