/*
  binary-source-file.cc -- implement Binary_source_file

  source file of the LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen
*/


#include <limits.h>		// INT_MAX
#include <assert.h>

#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "source-file.hh"
#include "binary-source-file.hh"
#include "string-convert.hh"

Binary_source_file::Binary_source_file (String& filename_str)
	: Source_file (filename_str)
{
}

Binary_source_file::~Binary_source_file ()
{
}

String
Binary_source_file::error_str (char const* pos_ch_C) const
{
    assert (this);
    if (!in_b (pos_ch_C))
	return "";

    char const* begin_ch_C = pos_ch_C - 8 >? ch_C ();
    char const* end_ch_C = pos_ch_C + 7 <? ch_C () + length_i ();

    String pre_str ((Byte const*)begin_ch_C, pos_ch_C - begin_ch_C);
    pre_str = String_convert::bin2hex_str (pre_str);
    for (int i = 2; i < pre_str.length_i (); i += 3)
	pre_str = pre_str.left_str (i) + " " + pre_str.cut_str (i, INT_MAX);
    String post_str ((Byte const*)pos_ch_C, end_ch_C - pos_ch_C);
    post_str = String_convert::bin2hex_str (post_str);
    for (int i = 2; i < post_str.length_i (); i += 3)
	post_str = post_str.left_str (i) + " " + post_str.cut_str (i, INT_MAX);

    String str = pre_str
	+ to_str ('\n')
    	+ to_str (' ', pre_str.length_i () + 1) 
    	+ post_str;
    return str;
}

int
Binary_source_file::line_i (char const* pos_ch_C) const
{
    if (!in_b (pos_ch_C))
    	return 0;

    return pos_ch_C - ch_C ();
}

U8
Binary_source_file::get_U8 ()
{
  return *(U8*)forward_ch_C (1);
}


U16
Binary_source_file::get_U16 ()
{
  U16 b;

  b = get_U8 () << 8;
  b |= get_U8 ();

  return b;
}


U32
Binary_source_file::get_U32()
{
  U32 b;
  
  b = get_U8 () << 24;
  b |= get_U8 () << 16;
  b |= get_U8 () << 8;
  b |= get_U8 ();

  return b;
}


