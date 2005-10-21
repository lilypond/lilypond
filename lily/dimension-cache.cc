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
  offset_ = d.offset_;
  offset_callbacks_ = d.offset_callbacks_;
  offsets_left_ = d.offsets_left_;
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
  offsets_left_ = 0;
  offset_callbacks_ = SCM_EOL;
  offset_ = 0.0;
  extent_ = 0;
  parent_ = 0;
}

Dimension_cache::~Dimension_cache ()
{
  delete extent_;
}
