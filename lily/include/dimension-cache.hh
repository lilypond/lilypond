/*   
  dimension-cache.hh -- declare Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "interval.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "dimension-cache-callback.hh"
#include "lily-guile.hh"


/**
  Adminstration of offset dimension info.
*/
struct Dimension_cache
{
  /*
    Multi typed:

     - cons: interval
     - procedure: callback
     - else: empty
   */
  SCM dimension_;

  /**
    The offset wrt. to the center of #parent_l_#
   */

  Real offset_;
  SCM offset_callbacks_;
  
  char offsets_left_;

  /**
     What to call to find extent.  Nil means empty. 
   */
  Grob * parent_l_;

  Dimension_cache (Dimension_cache const&);
  Dimension_cache ();
  void init ();
};


#endif /* DIMENSION_CACHE_HH */

