/*
  walkregs.hh -- declare Walker_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef WALKREGS_HH
#define WALKREGS_HH


#include "register-group.hh"
#include "parray.hh"
/**
  Top level registers: the interface to Complex_walker.

  [sigh. Sometimes I wish C++ could do better late binding.]

  Basically, this distributes and collects elements and elementinfo to
  children
  */
class Walker_registers : public Register_group_register {

    Array<Item*> prebreak_item_p_arr_;
    Array<Item*> nobreak_item_p_arr_;
    Array<Item*> postbreak_item_p_arr_;
    Link_array<Score_elem> musical_item_p_arr_;
    
    Array<Score_elem_info> announce_info_arr_;
 
    Complex_walker * walk_l_;
protected:   
    virtual Staff_info get_staff_info();

    virtual void announce_element(Score_elem_info);
    virtual void acknowledge_element(Score_elem_info);
    virtual void typeset_breakable_item(Item * pre_p , Item * nobreak_p, Item * post_p);
    virtual void typeset_element(Score_elem*elem_p);
    virtual Paper_def * paper() const;
public:
    virtual void pre_move_processing();
    virtual void post_move_processing();


    void do_announces();
    Walker_registers(Complex_walker*);
};

#endif // WALKREGS_HH
