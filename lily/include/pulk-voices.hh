/*
  pulk-voices.hh -- declare Pulk_voices

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO 
  integrate Meter handling, to guarantee proper creation of staff_columns.
*/


#ifndef PULK_VOICES_HH
#define PULK_VOICES_HH

#include "pqueue.hh"
#include "plist.hh"
#include "moment.hh"
#include "lily-proto.hh"
#include "lily-proto.hh"
#include "voice.hh"
#include "time-description.hh"


struct Voice_l { 
    Voice *l_;
    int  staff_idx_;
    Voice_l(Voice*v, int i){ l_ = v;
      staff_idx_ = i;
    }
    Voice_l() { l_ = 0; staff_idx_ =0; }
};
int compare(Voice_l const &p1, Voice_l const &p2);

class Pulk_voices
{
PQueue< Voice_l > voice_pq_;
    Pointer_list< Pulk_voice * > pulk_p_list_;
    Link_list<Staff *> staff_l_list_;
    Array < Time_description > time_arr_;
    Moment next_mom_;

public:
    Moment last_;
    bool time_checks_failed_b() const;
    bool ok() const;
    Moment next_mom() const;
    Pulk_voices(Link_list<Staff*> const&);
    void get_aligned_request(Request_column *col_l );
};


#endif // PULK_VOICES_HH
