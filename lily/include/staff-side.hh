/*   
  staff-side.hh -- declare Staff_side_{element,spanner,item}
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH


#include "spanner.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element.hh"

struct Side_position_interface
{
  Score_element * elt_l_;
public:
  Side_position_interface (Score_element*);
  static Real side_position (Dimension_cache const *);
  static Real self_alignment (Dimension_cache const *);
  static Real aligned_side (Dimension_cache const *);  
  
  void set_axis (Axis);
  Axis get_axis () const;
  
  bool supported_b () const;
  bool is_staff_side_b () const;
  void add_support (Score_element*);
  
  Direction get_direction () const;
  void set_direction (Direction);
};



#endif /* STAFF_SIDE_HH */

