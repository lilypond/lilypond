/*
  walkregs.hh -- declare Walker_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef WALKREGS_HH
#define WALKREGS_HH


#include "registergroup.hh"

/**
  Top level registers: the interface to Complex_walker.

  [sigh. Sometimes I wish C++ could do better late binding.]
 */
struct Walker_registers : Register_group_register {
    Array<Item*> prebreak_item_p_arr_;
    Array<Item*> nobreak_item_p_arr_;
    Array<Item*> postbreak_item_p_arr_;
    Array<Staff_elem_info> announce_info_arr_;
    Array<Voice_group_registers*> group_l_arr_;
    Complex_walker * walk_l_;

    /* *************** */
    void change_group(Group_change_req * greq_l,
		      Voice_registers *voice_regs_l,
		      Voice_group_registers * old_group);
    Voice_group_registers * get_group(String id);
    void typeset_musical_item(Staff_elem * elem_p);
    Walker_registers(Complex_walker*);
    void do_announces();
    void terminate_register(Request_register * reg);
    virtual bool try_request(Request * r);
    virtual Staff_info get_staff_info();

    virtual void announce_element(Staff_elem_info);
    virtual void acknowledge_element(Staff_elem_info);
    virtual void typeset_breakable_item(Item * pre_p , Item * nobreak_p, Item * post_p);
    virtual void typeset_element(Staff_elem*elem_p);
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual Paper_def * paper() const;
};

#endif // WALKREGS_HH
