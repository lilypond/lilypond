/*   
  dimension-cache-callback.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_CALLBACK_HH
#define DIMENSION_CACHE_CALLBACK_HH


typedef Interval (*Dim_cache_callback) (Grob *,Axis);
typedef Real (*Offset_callback) (Grob *,Axis);

#endif /* DIMENSION_CACHE_CALLBACK_HH */

