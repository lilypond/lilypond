/*   
  dimension-cache-callback.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_CALLBACK_HH
#define DIMENSION_CACHE_CALLBACK_HH

class Dimension_cache;
typedef Interval (*Dim_cache_callback)(Dimension_cache const *);
typedef Real (*Offset_cache_callback)(Dimension_cache const *);

#endif /* DIMENSION_CACHE_CALLBACK_HH */

