/*
  score-reg.hh -- declare Score_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_REG_HH
#define SCORE_REG_HH

#include "register-group.hh"
#include "global-acceptor.hh"

class Score_register : public Register_group_register, public Global_acceptor {
    Line_of_score * scoreline_l_;

    
    Array<Item*> nobreak_item_p_arr_;
    Link_array<Score_elem> musical_item_p_arr_;
    
    Score_column* command_column_l_;
    Score_column* musical_column_l_;
    
    friend class Score;
    void set_cols(Score_column*,Score_column*);
    void typeset_all();
    
public:
    NAME_MEMBERS();

    Score_register();

protected:   
    /* Global_acceptor interface */
    virtual void set_score(Score * score_l);
    virtual void prepare(Moment);
    virtual void finish();
    virtual void process();
    virtual int depth_i() const { return Global_acceptor::depth_i();}
    virtual Acceptor* ancestor_l(int l) { return Global_acceptor::ancestor_l(l);}

protected:
    /* Register_group_register interface */
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
