/*
 input.cc -- implement Input

 source file of the LilyPond music typesetter

  (c) 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "flower-proto.hh"
#include "input.hh"
#include "string.hh"
#include "source.hh"
#include "source-file.hh"

Input::Input (Source_file*s, char const *cl)
{
  source_file_=s;
  defined_str0_=cl;
}

Input::Input ()
{
  source_file_ = 0;
  defined_str0_ = 0;
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
    str += source_file_->error_string (defined_str0_);
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
    return source_file_->file_line_column_string (defined_str0_);
  else
    return " (" + _ ("position unknown") + ")";
}

String
Input::line_number_string () const
{
  if (source_file_)
    return to_string (source_file_->get_line (defined_str0_));
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
    return source_file_->get_line (defined_str0_);
  else
    return 0;

}

int
Input::column_number () const
{
  if (source_file_)
    return source_file_->get_column (defined_str0_);
  else
    return 0;

}
