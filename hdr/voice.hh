#ifndef VOICE_HH
#define VOICE_HH


#include "plist.hh"
#include "request.hh"

/// class for  horizontal stuff.
struct Voice {
    IPointerList<Voice_element *> elts;
    Real start;

    /****************/
    Real when(const Voice_element*)const;
    Real last() const;
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
    Real duration;
    const Voicegroup *group;
    const Voice *voice_;
    IPointerList<Request*> reqs;

   PointerList<const Item *> granted_items;
    PointerList<const Spanner *> granted_spanners;

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
