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
#include "input.hh"

/** one horizontal bit.  Voice_element is nothing but a container for
    *the requests, */
class Voice_element : public Input{
public:
    /** the duration of the element.  This can be 0; The duration is
      determined from rhythmical requests contained in this
      Voice_element */
    Moment duration_;
    Voice const *voice_C_;
    IPointer_list<Request*> req_p_list_;
    Request * principal_req_l_;

    /* *************** */
    void transpose(Melodic_req const &)const;
    Voice_element();
    Voice_element(Voice_element const & src );

    void add(Request*);
    bool find_plet_start_b(char c, Moment& moment_r);
    void print ()const;
    void set_default_group(String id);
    void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
};

#endif // VOICE-ELEMENT_HH
