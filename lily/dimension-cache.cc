/*   
  dimension-cache.cc --  implement Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */
#include <math.h>
#include "warn.hh"
#include "dimension-cache.hh"
#include "parray.hh"
#include "score-element.hh"


Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init();
  extent_callback_l_ = d.extent_callback_l_;
  offset_ = d.offset_;
  off_callbacks_ = d.off_callbacks_;
  parent_l_ = d.parent_l_;  
}

Dimension_cache::Dimension_cache ()
{
  init();
}

void
Dimension_cache::init()
{
  extent_callback_l_ =0;
  offset_ =0.0;
  offset_ =0.0;
  
  dim_.set_empty ();
  parent_l_ =0;
  valid_b_ = false;
}







