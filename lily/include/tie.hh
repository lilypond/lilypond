/*
  tie.hh -- declare Tie

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "bow.hh"

class Tie : public Bow {
    virtual Spanner* do_break_at(PCol*,PCol*)const;
    virtual void do_add_processing();
    virtual void do_post_processing();
    virtual void set_default_dir();
public:
    bool same_pitch_b_;
    Notehead * left_head_l_;
    Notehead * right_head_l_;
    void set_head(int, Notehead*head_l);
    Tie();
    
};
#endif // TIE_HH
