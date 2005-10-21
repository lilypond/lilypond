/*
  dimension-cache.hh -- declare Dimension_cache

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "lily-proto.hh"
#include "parray.hh"
#include "dimension-cache-callback.hh"
#include "lily-guile.hh"

/**
   Adminstration of offset dimension info.
*/
class Dimension_cache
{
  /**
     The offset wrt. to the center of #parent_#
  */
  Interval *extent_;
  Real offset_;
  SCM offset_callbacks_;

  char offsets_left_;

  /**
     What to call to find extent.  Nil means empty.
  */
  Grob *parent_;
  friend class Grob;
  
  Dimension_cache (Dimension_cache const &);
  ~Dimension_cache ();
  Dimension_cache ();
  void init ();
};

#endif /* DIMENSION_CACHE_HH */

