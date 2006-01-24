/*
  binary-source-file.cc -- implement Binary_source_file

  source file of the LilyPond music typesetter

  (c) 1997--2006 Jan Nieuwenhuizen
*/

#include <climits>		// INT_MAX
using namespace std;

#include "binary-source-file.hh"
#include "string-convert.hh"

Binary_source_file::Binary_source_file (String &file_name_string)
  : Source_file (file_name_string)
{
}

Binary_source_file::~Binary_source_file ()
{
}

String
Binary_source_file::quote_input (char const *pos_str0) const
{
  assert (this);
  if (!contains (pos_str0))
    return "";

  char const *begin_str0 = max (pos_str0 - 8, c_str ());
  char const *end_str0 = min (pos_str0 + 7, c_str () + length ());

  String pre_string ((Byte const *)begin_str0, pos_str0 - begin_str0);
  pre_string = String_convert::bin2hex (pre_string);
  for (int i = 2; i < pre_string.length (); i += 3)
    pre_string = pre_string.left_string (i)
      + " " + pre_string.cut_string (i, INT_MAX);
  String post_string ((Byte const *)pos_str0, end_str0 - pos_str0);
  post_string = String_convert::bin2hex (post_string);
  for (int i = 2; i < post_string.length (); i += 3)
    post_string = post_string.left_string (i)
      + " " + post_string.cut_string (i, INT_MAX);

  String str = pre_string
    + to_string ('\n')
    + to_string (' ', pre_string.length () + 1)
    + post_string;
  return str;
}

int
Binary_source_file::get_line (char const *pos_str0) const
{
  if (!contains (pos_str0))
    return 0;

  return pos_str0 - c_str ();
}

U8
Binary_source_file::get_U8 ()
{
  return *(U8 *)forward_str0 (1);
}

U16
Binary_source_file::get_U16 ()
{
  U16 b;

  b = get_U8 () << 8;
  b |= get_U8 ();

  return b;
}

/*
  naming is wrong. This is a UNIX-endian-32 (as opposed to xinu or ixun)
*/

U32
Binary_source_file::get_U32 ()
{
  U32 b;

  b = get_U8 () << 24;
  b |= get_U8 () << 16;
  b |= get_U8 () << 8;
  b |= get_U8 ();

  return b;
}

