/*
  voicegroup.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef VOICEGROUP_HH
#define VOICEGROUP_HH

#include "proto.hh"
#include "register.hh"
#include "varray.hh"
#include "string.hh"

struct Voice_registers {
    Notehead_register head_reg_;
    Slur_register slur_reg_;
    Voice *voice_l_;

    /****************/
    static bool acceptable_request(Request*);
    void pre_move_processing();
    void post_move_processing();
    void announce_element(Staff_elem_info info);
    Voice_registers(Complex_walker*,Voice*);
    bool try_request(Request*);
    void process_requests();
};


struct Voice_group_registers {
    String group_id_str_;
    Text_register text_reg_;
    Stem_beam_register stem_beam_reg_;
    Script_register script_reg_;
    Complex_walker * walk_l_;
    int dir_i_;
    
    /****************/
    static bool acceptable_request(Request*);
    void pre_move_processing();
    void post_move_processing();
    void announce_element(Staff_elem_info info);
    Voice_group_registers(Complex_walker*, String id = "");
    void process_requests();
    bool try_request(Request*);
};

#endif
