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

  To our foreign readers "pulk"ing is what you do with the stuff in
  your nose to get it out.  (and I don't mean blowing) */
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

    /**
      Get the requests at when(), and advance.
     */
    Array<Request*> get_req_l_arr();
};

#endif // PULK_VOICE_HH
