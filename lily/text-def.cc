/*
  text-def.cc -- implement Text_def

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"
#include <ctype.h>

Direction
Text_def::staff_dir () const
{
  if (style_str_ == "finger")
    return UP;
  return DOWN;
}

Real
Text_def::guess_width_f(Atom& a) const
{
  // Count each TeX command as one character, ugh
  int index, length=0;
  int total_length=text_str_.length_i();
  const char* str=text_str_.ch_C();
  for (index=0;index<total_length;index++) {
    length++;
    if (str[index]=='\\')
      for (index++;(index < total_length) && isalpha(str[index]);index++)
	;
  }
  return length * a.dim_.x ().length (); // ugh
}

Interval
Text_def::width (Paper_def * p) const
{
  Atom a = get_atom (p,CENTER);


  Interval i (0, guess_width_f(a));
  i += - (align_dir_ + 1)* i.center();
  return i;
}

void
Text_def::do_print() const
{
#ifndef NPRINT
  DOUT << "align " << align_dir_ << " `" << text_str_ << "'";
#endif
}

Text_def::Text_def()
{   
  align_dir_ = RIGHT;
  style_str_ = "roman";
}

bool
Text_def::do_equal_b (General_script_def const *gdef) const
{
  Text_def const *def= (Text_def*)gdef;
  return align_dir_ == def->align_dir_ && text_str_ == def->text_str_
	&& style_str_ == def->style_str_;
}

Atom
Text_def::get_atom (Paper_def *p, Direction) const
{
  Atom a= p->lookup_l(0)->text (style_str_, text_str_);

  a.translate_axis (-(align_dir_ + 1)* guess_width_f (a) / 2, X_AXIS);
  
  return a;
}

void
Text_def::print() const
{
  DOUT << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_dir_ << '\n';
}


IMPLEMENT_IS_TYPE_B1(Text_def,General_script_def);
