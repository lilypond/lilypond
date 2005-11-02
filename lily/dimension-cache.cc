/*
  dimension-cache.cc -- implement Dimension_cache

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "dimension-cache.hh"
#include "warn.hh"
#include "grob.hh"

Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init ();
  offset_ = d.offset_ ? new Real (*d.offset_) : 0;
  parent_ = d.parent_;
  extent_ = d.extent_ ? new Interval (*d.extent_) : 0;
}

Dimension_cache::Dimension_cache ()
{
  init ();
}

void
Dimension_cache::init ()
{
  offset_ = 0;
  extent_ = 0;
  parent_ = 0;
}

Dimension_cache::~Dimension_cache ()
{
  delete extent_;
  delete offset_;
}
