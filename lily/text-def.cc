/*
  text-def.cc -- implement Text_def

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>
#include "debug.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "text-def.hh"

Direction
Text_def::staff_dir () const
{
  if (style_str_ == "finger")
    return UP;
  return DOWN;
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
  Text_def const *def= dynamic_cast<Text_def const*>(gdef);
  return def&& align_dir_ == def->align_dir_ && text_str_ == def->text_str_
	&& style_str_ == def->style_str_;
}

Molecule
Text_def::get_molecule (Paper_def *p, Direction) const
{
  Molecule a= p->lookup_l(0)->text (style_str_, text_str_);

  a.translate_axis (-(align_dir_ + 1)* a.dim_[X_AXIS].center (), X_AXIS);
  
  return a;
}

void
Text_def::print() const
{
  DOUT << "Text `" << text_str_ << "\', style " <<
	style_str_ << "align " << align_dir_ << '\n';
}



