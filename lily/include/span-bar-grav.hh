/*
  span-bar-grav.hh -- declare Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_BAR_GRAV_HH
#define SPAN_BAR_GRAV_HH

#include "engraver.hh"

class Span_bar_engraver : public Engraver
{
    Span_bar * spanbar_p_;
    Array<Bar*> bar_l_arr_;
    Vertical_align_element * valign_l_;
public:
    NAME_MEMBERS();
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
    Span_bar_engraver();

};

#endif // SPAN_BAR_GRAV_HH
