/*
  item.cc -- implement Item

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "item.hh"
#include "p-col.hh"

Item::Item()
{
    pcol_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Item);

void
Item::do_print() const
{
#ifndef NPRINT
    mtor << "(unknown)";
#endif
}


Real 
Item::hpos_f()const
{
    return pcol_l_->hpos + offset().x;
}

Line_of_score *
Item::line_l()const
{
    return pcol_l_->line_l_;
}
