/*
  complexwalker.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef COMPLEXWALKER_HH
#define COMPLEXWALKER_HH

// this SUX
#include "proto.hh"
#include "grouping.hh"
#include "voicegroup.hh"
#include "assoc.hh"
#include "staffwalker.hh"
#include "key.hh"
#include "clef.hh"
#include "register.hh"

struct Complex_walker: Staff_walker {
    Local_key local_key_;
    Key key_;
    Array<int> *oldkey_undo;
    Array<int> typesetkey;

    int processed_bar_priority;
    bool processed_key;
    bool processed_clef;
    Clef clef_;
    Rhythmic_grouping default_grouping;
    

    IPointerList<Voice_registers *> voice_reg_list_;
    IPointerList<Voice_group_registers*> group_reg_list_;
    Assoc<const Voice *, Voice_group_registers *> voice_group_map_;

    Local_key_register local_key_reg_;
    Array<Staff_elem_info> announce_info_arr_;
    
    /****************/
    void  do_change_group(const Voice * v, String group_id_str);

    Voice_registers *find_voice_reg(Voice*v_l);
    Voice_registers *get_voice_reg(Voice*v_l);
    
    /// search and return. return 0 if not found.
    Voice_group_registers *find_voice_group(Voice* v_l);
    /// search. Create if necessary
    Voice_group_registers *get_voice_group(Voice* v_l);
    /// search and return. return 0 if not found
    Voice_group_registers *find_voice_group(const char* id);
    /// Create if necessary
    Voice_group_registers *get_voice_group(const char*);
    
    void regs_process_requests();
    void do_announces();
    void try_request(Request*req);
    void typeset_element(Staff_elem *elem_p);
    void announce_element(Staff_elem_info);
    virtual void do_TYPESET_command(Command*);
    virtual void do_INTERPRET_command(Command*);
    virtual void process_requests();
    virtual void do_post_move();
    virtual void do_pre_move();
    
    Complex_walker(Complex_staff*);
    Complex_column *col();
    Complex_staff *staff();
};


#endif // COMPLEXWALKER_HH


