/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "bow.hh"

/**
  Connect two noteheads.
  */
class Tie : public Bow {
    virtual void do_add_processing();
    virtual void do_post_processing();
    virtual void set_default_dir();
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    
public:
    bool same_pitch_b_;
    Note_head * left_head_l_;
    Note_head * right_head_l_;
    void set_head(int, Note_head*head_l);

    Tie();
    NAME_MEMBERS(Tie);
    SPANNER_CLONE(Tie)
};
#endif // TIE_HH
