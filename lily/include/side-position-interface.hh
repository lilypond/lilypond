/*   
  side-position-interface.hh -- declare Side_position_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "spanner.hh"
#include "item.hh"

struct Side_position_interface
{
  Score_element * elt_l_;
public:
  Side_position_interface (Score_element const*);
  static Real side_position (Dimension_cache const *);
  static Real self_alignment (Dimension_cache const *);
  static Real aligned_side (Dimension_cache const *);  
  static Real quantised_position (Dimension_cache const*);
  void set_axis (Axis);
  void set_quantised (Axis);
  Axis get_axis () const;
  
  bool supported_b () const;
  bool has_interface_b () const;
  void add_support (Score_element*);
  
  Direction get_direction () const;
  void set_direction (Direction);
};


#endif /* SIDE_POSITION_INTERFACE_HH */

