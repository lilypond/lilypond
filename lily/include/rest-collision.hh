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
  static void add_column (Score_element*me,Note_column*);
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  static Real force_shift_callback (Score_element *, Axis);
  static SCM do_shift (Score_element*,SCM);
};
#endif // REST_COLLISION_HH
