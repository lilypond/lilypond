/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "item.hh"

class Rest_collision : public Item {
public:
  void add_column (Note_column*);
  Interval rest_dim () const;
    
  Rest_collision(SCM);

  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);
};
#endif // REST_COLLISION_HH
