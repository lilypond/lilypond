/*
  voice-element.hh -- declare Voice_element

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICE_ELEMENT_HH
#define VOICE_ELEMENT_HH

#include "proto.hh"
#include "plist.hh"
#include "moment.hh"

/** one horizontal bit.  Voice_element is nothing but a container for
    *the requests, */
struct Voice_element {
    Moment duration;
    char const* defined_ch_c_l_;
    Voice const *voice_l_;
    IPointerList<Request*> reqs;

    /* *************** */
    void transpose(Melodic_req const &)const;
    Voice_element();
    Voice_element(Voice_element const & src );

    void add(Request*);
    bool find_plet_start_bo(char c, Moment& moment_r);
    void print ()const;
    void set_default_group(String id);
    void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
};

#endif // VOICE-ELEMENT_HH
