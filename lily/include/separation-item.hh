/*   
  single-malt-grouping-item.hh -- declare Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SINGLE_MALT_GROUPING_ITEM_HH
#define SINGLE_MALT_GROUPING_ITEM_HH

#include "lily-proto.hh"

/**  
*/
struct Separation_item
{
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static Interval my_width (Score_element*) ;
  static void add_item (Score_element*,Item*);
};

#endif /* SINGLE_MALT_GROUPING_ITEM_HH */

