/*
  span-brace-engraver.hh -- declare Span_brace_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_BRACE_GRAV_HH
#define SPAN_BRACE_GRAV_HH

#include "engraver.hh"

class Span_brace_engraver : public Engraver
{
public:
    NAME_MEMBERS();
    Span_brace_engraver();
    ~Span_brace_engraver();

    virtual void acknowledge_element( Score_elem_info i );
    virtual void do_pre_move_processing();

private:
    Span_brace_item* span_brace_p_;
    Array<Bar*> bar_l_arr_;
    Vertical_align_element* valign_l_;
};

#endif // SPAN_BRACE_GRAV_HH
