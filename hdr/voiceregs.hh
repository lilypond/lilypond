/*
  voiceregs.hh -- declare Voice_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEREGS_HH
#define VOICEREGS_HH

#include "registergroup.hh"

struct Voice_registers : Register_group {
    Voice *voice_l_;

    /* *************** */

    static bool acceptable_request_b(Request*);
    virtual void acknowledge_element(Staff_elem_info info);

    Voice_registers(Complex_walker*,Voice*);
};


#endif // VOICEREGS_HH
