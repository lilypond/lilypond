#ifndef VOICE_HH
#define VOICE_HH

#include "proto.hh"
#include "plist.hh"
#include "moment.hh"

/// class for  horizontal stuff.
/**

    Voice is a ordered row of Voice_elements. It is strictly horizontal:
    you cannot have two rhythmic elements running parallel in a Voice

    */

struct Voice {
    IPointerList<Voice_element *> elts;
    Moment start;

    /* *************** */
    Moment when(const Voice_element*)const;
    Moment last() const;
    Voice();
    Voice(Voice const&);
    void add(Voice_element*);
    void print() const;
    void set_default_group(String id);
};
/// one horizontal bit. 
/** Apart from being a container for the requests, Voice_element is nothing
    */
struct Voice_element {
    Moment duration;
    char const* defined_ch_c_l_m;
    const Voice *voice_l_;
    IPointerList<Request*> reqs;

    /* *************** */
    
    void add(Request*);
    Voice_element();
    Voice_element(Voice_element const & src );
    void print ()const;
    void set_default_group(String id);
};
#endif
