/*
  input.cc -- implement Input

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
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

Input::Input (Input const &s)
{
    source_file_l_ = s.source_file_l_;
    defined_ch_C_ = s.defined_ch_C_;
}

void
Input::set_spot (Input const &i)
{
    *this  = i;
}

void
Input::message (String message_str) const
{
    String str = "";

    if (source_file_l_)
      {
	str += location_str () + String (": ");
      }

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

String
Input::location_str () const
{
    if (source_file_l_)
	return source_file_l_->file_line_no_str (defined_ch_C_);
    else
	return _ ("(location unknown)");
}
