/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH
#include "lily-proto.hh"
#include "horizontal-vertical-group-item.hh"

/**
  Resolve conflicts between various Note_columns (chords).
  
  TODO 

  multistaff support (see Chlapik: equal noteheads should be on the
  same hpos.)  
*/
class Collision : public Horizontal_vertical_group_item {
protected:
    virtual void do_substitute_dependency (Score_element*,Score_element*);
    virtual void do_pre_processing();
public:
    Link_array<Note_column> clash_l_arr_;
    
    void add_column (Note_column*ncol_l);
    Collision();
};
#endif // COLLISION_HH
