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

struct Staff_sidify
{
  Score_element * elt_l_;
public:
  Staff_sidify (Score_element*);
  static Real position_self (Dimension_cache const *);

  void set_axis (Axis);
  Axis get_axis () const;
  
  bool is_staff_side_b ();
  void add_support (Score_element*);
  Real aligned_position (Dimension_cache const*);

  
  Direction get_direction () const;
  void set_direction (Direction);
};



#endif /* STAFF_SIDE_HH */

