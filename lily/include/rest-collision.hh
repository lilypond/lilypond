/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "item.hh"

class Rest_collision : public Item {
public:
  void add_column (Note_column*);
  Interval rest_dim () const;
    
  Rest_collision();
protected:
  virtual void do_pre_processing();
};
#endif // REST_COLLISION_HH
