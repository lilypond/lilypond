/*
  complexwalker.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef COMPLEXWALKER_HH
#define COMPLEXWALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "voicegroup.hh"
#include "assoc.hh"
#include "staffwalker.hh"
#include "key.hh"
#include "clef.hh"

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
//    Assoc<Voice*, Voice_registers*> voice_reg_map_;    
    //IPointerList<Voice_group_registers *> voice_reg_list_;
    //Assoc<int, Voice_group_registers*> group_reg_map_;
    Voice_group_registers group_regs_;
    Local_key_register local_key_reg_;
    Array<Staff_elem_info> announce_info_arr_;
    
    /****************/
    Voice_registers *find_voice_reg(Voice*v_l);
    
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
    
    void do_note(CNote_info);
    Complex_walker(Complex_staff*);
    Complex_column *col();
    Complex_staff *staff();

    void do_local_key(Note_req*, Notehead*);

};


#endif // SIMPLEWALKER_HH


