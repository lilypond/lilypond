/*
  complex-walker.hh -- declare Complex_walker

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef COMPLEXWALKER_HH
#define COMPLEXWALKER_HH

#include "lily-proto.hh"
#include "staff-walker.hh"
#include "score-elem-info.hh"

/**
  A staff walker which uses registers to decide what to print
 */
class Complex_walker: public Staff_walker {
    bool try_command_request(Command_req *req_l);
    void do_announces();
    void try_request(Request*req);    


    
public:
    Walker_registers *walk_regs_p_;
    
    /* *************** */

    void regs_process_requests();
    void typeset_element(Score_elem *elem_p);
    void announce_element(Score_elem_info);
    virtual void process_requests();
    virtual void do_post_move();
    virtual void do_pre_move();

    Complex_walker(Staff*);
    ~Complex_walker();

private:
};


#endif // COMPLEXWALKER_HH


