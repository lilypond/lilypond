/*   
  dimension-cache.cc --  implement Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */
#include <math.h>
#include "warn.hh"
#include "dimension-cache.hh"
#include "parray.hh"
#include "grob.hh"


Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init();
  dimension_ = d.dimension_;

  offset_ = d.offset_;
  offset_callbacks_ = d.offset_callbacks_;
  offsets_left_ = d.offsets_left_;
  parent_l_ = d.parent_l_;  
}

Dimension_cache::Dimension_cache ()
{
  init();
}

void
Dimension_cache::init()
{
  dimension_ = SCM_EOL;

  offsets_left_ = 0;
  offset_callbacks_ = SCM_EOL;
  offset_ =0.0;
  
  parent_l_ =0;
}







