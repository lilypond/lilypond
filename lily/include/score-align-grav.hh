/*
  score-align-grav.hh -- declare Score_align_reg

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_ALIGN_GRAV_HH
#define SCORE_ALIGN_GRAV_HH

#include "engraver.hh"

class Score_align_engraver: public Engraver
{
    Horizontal_group_item  * align_p_;
public:
    
    const char* type_ch_C_;
    int priority_i_;
    Score_align_engraver();
    DECLARE_MY_RUNTIME_TYPEINFO;
protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
};
#endif // SCORE_ALIGN_GRAV_HH
