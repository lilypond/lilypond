/*
  score-halign-reg.hh -- declare Score_horizontal_align_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_HALIGN_REG_HH
#define SCORE_HALIGN_REG_HH
#include "register.hh"
class Score_horizontal_align_register : public Request_register {
    
    Break_align_item * halign_p_;
public:
    NAME_MEMBERS();
    Score_horizontal_align_register();
protected:
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
};
#endif // SCORE_HALIGN_REG_HH
