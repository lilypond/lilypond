#ifndef VOICE_HH
#define VOICE_HH

#include "proto.hh"
#include "plist.hh"
#include "moment.hh"

/** class for  horizontal stuff.

    Voice is a ordered row of Voice_elements. It is strictly horizontal:
    you cannot have two rhythmic elements running parallel in a Voice

    */

struct Voice {
    IPointerList<Voice_element *> elts;
    Moment start;

    /* *************** */
    Voice();
    Voice(Voice const&);

    Moment when(const Voice_element*)const;
    Moment last() const;

    void add(Voice_element*);
    bool find_plet_start_bo(char c, Moment& moment_r);
    void print() const;
    void set_default_group(String id);
    void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
};

/** one horizontal bit.  Apart from being a container for the requests, Voice_element is nothing
    */
struct Voice_element {
    Moment duration;
    char const* defined_ch_c_l_;
    const Voice *voice_l_;
    IPointerList<Request*> reqs;

    /* *************** */
    
    Voice_element();
    Voice_element(Voice_element const & src );

    void add(Request*);
    bool find_plet_start_bo(char c, Moment& moment_r);
    void print ()const;
    void set_default_group(String id);
    void set_plet_backwards(Moment& now_moment_r, Moment until_moment, int num_i, int den_i);
    /// return true when done
};
#endif
