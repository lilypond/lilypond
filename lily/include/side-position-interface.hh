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

   self-alignment-X -- real number: -1 = left aligned, 0 = center, 1
     right-aligned in X direction.

     Set to an element pointer, if you want that element to be the center. 
     
   self-alignment-Y -- like self-alignment-X but for Y axis
   
   TODO: move  out unrelated callbacks.

   TODO: reduce number of methods.

*/
struct Side_position
{
public:
  static Real side_position (Score_element *, Axis);
  static Real aligned_on_self (Score_element *, Axis);
  static Real aligned_side (Score_element *, Axis);  
  static Real quantised_position (Score_element *, Axis);
  static Real centered_on_parent (Score_element *, Axis);
  static void set_axis (Score_element*,Axis);
  static void set_minimum_space (Score_element*,Real);
  static void set_padding (Score_element*,Real);
  static Axis get_axis (Score_element*) ;
  static bool supported_b (Score_element*) ;
  static bool has_interface (Score_element*) ;
  static void add_support (Score_element*,Score_element*);
  static void add_staff_support (Score_element*);
  static Direction get_direction (Score_element*);
  static void set_direction (Score_element*,Direction);
};


#endif /* SIDE_POSITION_INTERFACE_HH */

