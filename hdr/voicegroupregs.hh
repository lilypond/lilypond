/*
  voicegroupregs.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEGROUPREGS_HH
#define VOICEGROUPREGS_HH

#include "registergroup.hh"

struct Voice_group_registers  : Register_group {
    String group_id_str_;
    Complex_walker * walk_l_;
    
    /* *************** */
    void set_dir(int i);
    static bool acceptable_request_b(Request*);
  
    void acknowledge_element(Staff_elem_info info);
    Voice_group_registers(Complex_walker*, String id = "");
    bool try_request(Request*);
};
#endif // VOICEGROUPREGS_HH
