/*
  binary-source-file.cc -- implement Binary_source_file

  source file of the LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen
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
Binary_source_file::error_str (char const* pos_ch_c_l) const
{
    assert (this);
    if (!in_b (pos_ch_c_l))
	return "";

    char const* begin_ch_c_l = pos_ch_c_l - 8 >? ch_C ();
    char const* end_ch_c_l = pos_ch_c_l + 7 <? ch_C () + length_i ();

    String pre_str ((Byte const*)begin_ch_c_l, pos_ch_c_l - begin_ch_c_l);
    pre_str = String_convert::bin2hex_str (pre_str);
    for (int i = 2; i < pre_str.length_i (); i += 3)
	pre_str = pre_str.left_str (i) + " " + pre_str.cut_str (i, INT_MAX);
    String post_str ((Byte const*)pos_ch_c_l, end_ch_c_l - pos_ch_c_l);
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
Binary_source_file::line_i (char const* pos_ch_c_l) const
{
    if (!in_b (pos_ch_c_l))
    	return 0;

    return pos_ch_c_l - ch_C ();
}

