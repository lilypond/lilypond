/*
  collision.hh -- declare Collision

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH
#include "lily-proto.hh"
#include "item.hh"

/** TODO 

  multistaff support (see Chlapik: equal noteheads should be on the
  same hpos.)  
*/
class Collision : public Item {
protected:
    virtual void do_pre_processing();
public:
    Array<Note_column*> clash_l_arr_;
    NAME_MEMBERS(Collision);
    void add (Note_column*ncol_l);
    Collision();
    
};
#endif // COLLISION_HH
