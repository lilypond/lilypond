/*
  dimension-cache-callback.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DIMENSION_CACHE_CALLBACK_HH
#define DIMENSION_CACHE_CALLBACK_HH

#include "axis.hh"

typedef Interval (*Dim_cache_callback) (Grob *, Axis);
typedef Real (*Offset_callback) (Grob *, Axis);

#endif /* DIMENSION_CACHE_CALLBACK_HH */

