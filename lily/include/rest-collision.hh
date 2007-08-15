/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Rest_collision
{
public:
  static void add_column (Grob *me, Grob *);

  static bool has_interface (Grob *);
  DECLARE_SCHEME_CALLBACK (force_shift_callback, (SCM element));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (force_shift_callback_rest, (SCM element, SCM off));
  static SCM do_shift (Grob *);
};
#endif // REST_COLLISION_HH
