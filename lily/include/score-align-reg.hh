/*
  score-align-reg.hh -- declare Score_align_reg

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_ALIGN_REG_HH
#define SCORE_ALIGN_REG_HH

#include "register.hh"

class Score_align_register: public Request_register
{
    
    const char* type_ch_C_;
    Horizontal_group_item  * align_p_;
public:
    int priority_i_;
    Score_align_register(char const*);
    NAME_MEMBERS();
protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
};
#endif // SCORE_ALIGN_REG_HH
