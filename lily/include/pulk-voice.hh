/*
  pulk-voice.hh -- declare Pulk_voice

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PULK_VOICE_HH
#define PULK_VOICE_HH

#include "proto.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "priorities.hh"
#include "pcursor.hh"

/**
  Align requests with starting time.
 */
class Pulk_voice
{
    PCursor<Voice_element*> cur_;
    Moment elt_mom_;
    Priorities<Moment> subtle_moment_priorities_;
    int subtle_idx_;
    void set_subtle();
    void next();
public:
    int staff_idx_;

    Moment when()const;
    bool ok()const { return cur_.ok() ; }

    Pulk_voice(Voice*, int staff_idx);
    Array<Request*> get_req_l_arr();
};

#endif // PULK_VOICE_HH
