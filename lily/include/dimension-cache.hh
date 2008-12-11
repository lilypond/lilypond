/*
  dimension-cache.hh -- declare Dimension_cache

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "lily-proto.hh"

/*
  XY offset/refpoint/extent structure.
*/
class Dimension_cache
{
  Interval *extent_;
  Real *offset_;
  Grob *parent_;
  void init ();
  void clear ();
  
  friend class Grob;
  
  Dimension_cache (Dimension_cache const &);
  ~Dimension_cache ();
  Dimension_cache ();
};

#endif /* DIMENSION_CACHE_HH */

