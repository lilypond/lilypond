/*   
  dimension-cache.hh -- declare Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "interval.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "dimension-cache-callback.hh"


/**
  Adminstration of offset dimension info. 
 */
struct Dimension_cache
{
  bool valid_b_;
  Interval dim_;
  /**
    The offset wrt. to the center of #parent_l_#
   */

  Real offset_;


  
  Array<Offset_callback> off_callbacks_;

  /**
     What to call to find extent.  Nil means empty. 
   */
  Dim_cache_callback extent_callback_l_;
  Score_element * parent_l_;

  Dimension_cache(Dimension_cache const&);
  Dimension_cache ();
  void init ();
};


#endif /* DIMENSION_CACHE_HH */

