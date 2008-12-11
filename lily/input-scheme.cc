/*
  input-scheme.cc -- implement Input bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  int l = 0;
  int ch = 0;
  int col = 0;
  ip->get_counts (&l, &ch, &col);
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
