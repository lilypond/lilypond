/*
  input.cc -- implement Input

  source file of the LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "input.hh"

#include <cstdio>

#include "source.hh"
#include "source-file.hh"

Input::Input (Input const &i)
{
  source_file_ = i.source_file_;
  start_ = i.start_;
  end_ = i.end_;
}

Input::Input ()
{
  source_file_ = 0;
  start_ = 0;
  end_ = 0;
}

Input
Input::spot () const
{
  return *this;
}

void
Input::set_spot (Input const &i)
{
  *this = i;
}

void
Input::step_forward ()
{
  if (end_ == start_)
    end_++;
  start_++;
}

void
Input::set_location (Input const &i_start, Input const &i_end)
{
  source_file_ = i_start.source_file_;
  start_ = i_start.start_;
  end_ = i_end.end_;
}

/*
  Produce GNU-compliant error message.  Correcting lilypond source is
  such a breeze if you ('re edidor) know (s) the error column too

  Format:

  [file:line:column:][warning:]message
*/
void
Input::message (String message_string) const
{
  String str;

  /*
    marked "Work in prgress" in GNU iostream
    libg++ 2.7.2.8
    libstdc++ 2.8.1

    why not just return always -1 (unknown),
    iso breaking the interface?

    int col = cerr.rdbuf ()->column ();

  */

  // well, we don't want to loose first warning...
  int col = 1;
  if (col > 0)
    str += "\n";

  if (source_file_)
    str += location_string () + String (": ");

  str += message_string;
  if (source_file_)
    {
      str += ":\n";
      str += source_file_->error_string (start_);
    }
  fprintf (stderr, "%s\n", str.to_str0 ());
  fflush (stderr);
}

void
Input::warning (String message_string) const
{
  message (_ ("warning: ") + message_string);
}
void
Input::error (String s) const
{
  message (_ ("error: ")+ s);
}

void
Input::non_fatal_error (String s) const
{
  message (_ ("non fatal error: ") + s);
}
String
Input::location_string () const
{
  if (source_file_)
    return source_file_->file_line_column_string (start_);
  else
    return " (" + _ ("position unknown") + ")";
}

String
Input::line_number_string () const
{
  if (source_file_)
    return to_string (source_file_->get_line (start_));
  else
    return "?";
}

String
Input::file_string () const
{
  if (source_file_)
    return source_file_->name_string ();
  else
    return "";
}

int
Input::line_number () const
{
  if (source_file_)
    return source_file_->get_line (start_);
  else
    return 0;
}

int
Input::column_number () const
{
  if (source_file_)
    return source_file_->get_column (start_);
  else
    return 0;
}

int
Input::end_line_number () const
{
  if (source_file_)
    return source_file_->get_line (end_);
  else
    return 0;
}

int
Input::end_column_number () const
{
  if (source_file_)
    return source_file_->get_column (end_);
  else
    return 0;
}
