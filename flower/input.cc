/*
 input.cc -- implement Input

 source file of the LilyPond music typesetter

 (c) 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <iostream.h>
#include "proto.hh"
#include "input.hh"
#include "string.hh"
#include "source.hh"
#include "source-file.hh"

Input::Input (Source_file*s, char const *cl)
{
  source_file_l_=s;
  defined_ch_C_=cl;
}

Input::Input ()
{
  source_file_l_ = 0;
  defined_ch_C_ = 0;
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
  Produce almost GNU-compliant error message.  Lily used to be rather
  GNU-compliant in this too, but correcting mudela is such a breeze if 
  you('re edidor) know(s) the error column too (there's no GNU standard
  on columns, is there?).

  Format:

    [file:line:column:][warning:]message

 */
void
Input::message (String message_str) const
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
  
  if (source_file_l_)
    str += location_str () + String (": ");

  str += message_str;
  if (source_file_l_)
   {
    str += ":\n";
    str += source_file_l_->error_str (defined_ch_C_);
   }
  cerr << str << endl;
}

void
Input::warning (String message_str) const
{
  message (_ ("warning: ") + message_str);
}
void
Input::error (String s) const
{
  message (_ ("error: ")+ s);
}

void
Input::non_fatal_error (String s) const
{
  message (_ ("Non fatal error: ") + s);
}
String
Input::location_str () const
{
  if (source_file_l_)
    return source_file_l_->file_line_column_str (defined_ch_C_);
  else
    return "(" + _ ("position unknown") + ")";
}

String
Input::line_number_str () const
{
  if (source_file_l_)
    return to_str (source_file_l_->line_i (defined_ch_C_));
  else
    return "?";
}
