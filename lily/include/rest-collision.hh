/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"




class Rest_collision
{
public:
  static void add_column (Score_element*me,Score_element*);
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  DECLARE_SCHEME_CALLBACK(force_shift_callback, (SCM element, SCM axis));
  static SCM do_shift (Score_element*,SCM);
};
#endif // REST_COLLISION_HH
