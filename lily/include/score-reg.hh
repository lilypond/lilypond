/*
  score-reg.hh -- declare Score_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_REG_HH
#define SCORE_REG_HH

#include "register-group.hh"
#include "pqueue.hh"

class Score_register : public Register_group_register {
    Line_of_score * scoreline_l_;
    Score * score_l_;
    
    Array<Item*> nobreak_item_p_arr_;
    Link_array<Score_elem> musical_item_p_arr_;
    
    Score_column* command_column_l_;
    Score_column* musical_column_l_;
    
    friend class Score;
    void set_cols(Score_column*,Score_column*);
    void typeset_all();
    
    PQueue<Moment> extra_mom_pq_;
    Moment last_mom_;
public:
    NAME_MEMBERS();

    void add_moment_to_process(Moment);
    Score_register();
    int depth_i() const;
protected:   
    void set_score(Score * score_l);
    

    virtual Staff_info get_staff_info()const;
    virtual bool do_try_request(Request*);
    virtual void do_creation_processing();
    virtual void do_removal_processing();
    virtual void announce_element(Score_elem_info);
    virtual void typeset_breakable_item(Item * nobreak_p);
    virtual void do_announces();
    virtual void typeset_element(Score_elem*elem_p);
    virtual Paper_def * paper() const;
    virtual void do_pre_move_processing();
    
};

#endif // SCORE_REG_HH
