/*
  voice.hh -- declare Voice

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef VOICE_HH
#define VOICE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "moment.hh"

/** class for  horizontal stuff.

    Voice is a ordered row of Voice_elements. It is strictly
    horizontal: you cannot have two rhythmic elements running parallel
    in a Voice. For proper processing, each Voice should have
    Group_change_req as a first element.

    */

struct Voice {
    /** the elements, earliest first.
      Please use the member #add()# to add a new element
      */
    Pointer_list<Voice_element *> elts_;
    Moment start_;

    /* *************** */
    Voice();
    Voice(Voice const&);

    Moment when(Voice_element const *)const;
    Moment last() const;
    void transpose(Melodic_req const &)const;
    void add(Voice_element*);
    void print() const;
    void set_default_group(String id);
};

#endif
