#ifndef VOICE_HH
#define VOICE_HH

#include "proto.hh"
#include "plist.hh"
#include "moment.hh"

/// class for  horizontal stuff.
struct Voice {
    IPointerList<Voice_element *> elts;
    Moment start;

    /****************/
    Moment when(const Voice_element*)const;
    Moment last() const;
    Voice();
    Voice(Voice const&);
    void add(Voice_element*);
    void print() const;
};
/**

    Voice is a ordered row of Voice_elements. It is strictly horizontal:
    you cannot have two rhythmic elements running parallel in a Voice

    */

struct Voicegroup {
    /// don't know how to identify these.
};

/// one horizontal bit. 
struct Voice_element {
    Moment duration;
    const Voicegroup *group;
    const Voice *voice_;
    IPointerList<Request*> reqs;

    /****************/
    
    void add(Request*);
    Voice_element();
    Voice_element(Voice_element const & src );
    void print ()const;
};
/** Apart from being a container for the requests, Voice_element is
    glue between related items and spanners, between requests and
    (voice)groups
    */
#endif
