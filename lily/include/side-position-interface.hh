/*   
  side-position-interface.hh -- declare Side_position_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIDE_POSITION_INTERFACE_HH
#define SIDE_POSITION_INTERFACE_HH

#include "spanner.hh"
#include "item.hh"


/**
   Position victim object (ELT_L_) next to other objects (the support).

   side-support -- list of score elements

   direction -- where to put the victim object (left or right?)

   side-relative-direction -- if set: get the direction from a different object, and multiply by this.
   
   direction-source -- in case side-relative-direction is set, where
   to get the direction

   minimum-space -- minimum distance that the victim should move
   (after padding)

   padding -- add this much extra space between victim and support

   TODO: move  out unrelated callbacks.
   
 */
struct Side_position_interface
{
  Score_element * elt_l_;
public:
  Side_position_interface (Score_element const*);
  static Real side_position (Dimension_cache const *);
  static Real aligned_on_self (Dimension_cache const *);
  static Real aligned_side (Dimension_cache const *);  
  static Real quantised_position (Dimension_cache const*);
  static Real centered_on_parent (Dimension_cache const*);
  void set_axis (Axis);
  void set_minimum_space (Real);
  void set_padding (Real);
  void set_quantised (Axis);
  Axis get_axis () const;
  
  bool supported_b () const;
  bool has_interface_b () const;
  void add_support (Score_element*);

  void add_staff_support ();
  Direction get_direction () const;
  void set_direction (Direction);
};

Side_position_interface side_position (Score_element*);

#endif /* SIDE_POSITION_INTERFACE_HH */

