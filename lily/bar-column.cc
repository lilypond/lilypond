/*
  bar-column.cc -- implement Bar_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "bar.hh"
#include "bar-column.hh"

Bar_column::Bar_column()
{
  bar_l_ =0;
  breakable_b_ = true;
}

void
Bar_column::set_bar (Bar*b)
{
  Script_column::add_support (b);
  bar_l_ = b;	
}

void
Bar_column::do_substitute_dependency (Score_element*o,Score_element*n)
{
  Script_column::do_substitute_dependency (o,n);
  if (o == bar_l_) 
    {
      bar_l_ =  dynamic_cast<Bar *> (n);
    }
}

IMPLEMENT_IS_TYPE_B1(Bar_column, Script_column);

