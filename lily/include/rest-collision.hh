/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Rest_collision		// interface
{
public:
  Score_element *elt_l_;
  
  void add_column (Note_column*);
  Rest_collision(Score_element*);
  void set_interface ();

  static Real force_shift_callback (Score_element *, Axis);
  static SCM do_shift (Score_element*,SCM);
};
#endif // REST_COLLISION_HH
