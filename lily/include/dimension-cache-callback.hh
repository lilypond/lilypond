/*   
  dimension-cache-callback.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_CALLBACK_HH
#define DIMENSION_CACHE_CALLBACK_HH


typedef Interval (*Dim_cache_callback)(Score_element const *,Axis);
typedef Real (*Offset_callback)(Score_element const *,Axis);

#endif /* DIMENSION_CACHE_CALLBACK_HH */

