/*
  clef.cc -- implement  Clef

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>,
  Mats Bengtsson <matsb@s3.kth.se>
*/

#include "clef.hh"
#include "debug.hh"

Clef::Clef()
{
    set_type("violin");
}

void 
Clef::set_type(String type_str)
{
    clef_type_str_  = type_str;
    if (clef_type_str_ == "violin") {
	c0_position_i_= -2;
    } else if (clef_type_str_ == "alto") {
	c0_position_i_= 4;
    } else if (clef_type_str_ == "tenor") {
	c0_position_i_= 6;
    } else if (clef_type_str_ == "bass") {
	c0_position_i_= 10;
    } else
	error("unknown clef type `"+clef_type_str_+"\'");
}
