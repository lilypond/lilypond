/*
  text-def.cc -- implement Text_def

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"
#include "dimen.hh"

Direction
Text_def::staff_dir () const
{
  if (style_str_ == "finger")
    return UP;
  return DOWN;
}

Interval
Text_def::width (Paper_def * p) const
{
  Atom a = get_atom (p,CENTER);

  Real guess_width_f = text_str_.length_i() * a.dim_.x ().length (); // ugh
  Interval i (0, guess_width_f);
  i += - (align_i_ + 1)* i.center();
  return i;
}

void
Text_def::do_print() const
{
#ifndef NPRINT
  DOUT << "align " <<align_i_ << " `" << text_str_ << "'";
#endif
}

Text_def::Text_def()
{   
  align_i_ = RIGHT;
  style_str_ = "roman";
}

bool
Text_def::do_equal_b (General_script_def const *gdef) const
{
  Text_def const *def= (Text_def*)gdef;
  return align_i_ == def->align_i_ && text_str_ == def->text_str_
	&& style_str_ == def->style_str_;
}

Atom
Text_def::get_atom (Paper_def *p, Direction) const
{
  return p->lookup_l()->text (style_str_, text_str_, -align_i_);
}

void
Text_def::print() const
{
  DOUT << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_i_ << '\n';
}


IMPLEMENT_IS_TYPE_B1(Text_def,General_script_def);
