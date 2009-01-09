/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Rest_collision
{
public:
  static void add_column (Grob *me, Grob *);

  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (force_shift_callback, (SCM element));
  DECLARE_SCHEME_CALLBACK (calc_positioning_done, (SCM element));
  DECLARE_SCHEME_CALLBACK (force_shift_callback_rest, (SCM element, SCM off));
  static SCM do_shift (Grob *);
};
#endif // REST_COLLISION_HH
