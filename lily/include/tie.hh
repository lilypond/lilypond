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
class Tie : public Bow
{
public:
    Tie ();
    void set_head (Direction, Note_head*head_l);

    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Tie);

    bool same_pitch_b_;
    Drul_array<Note_head *> head_l_drul_;

protected:
    virtual void do_add_processing ();
    virtual void do_post_processing ();
    virtual void set_default_dir();
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    Array<Offset> get_controls () const;
};
#endif // TIE_HH
