/*
  score-performer.hh -- declare Score_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef SCORE_PERFORMER_HH
#define SCORE_PERFORMER_HH

#include "performer-group-performer.hh"
#include "global-translator.hh"

class Score_performer: 
    public Performer_group_performer, public Global_translator 
{
    Midi_file* file_p_;
    Moment prev_mom_;

public:
    virtual void play_event( Midi_item i );
    virtual void prepare( Moment mom );
};

#endif // SCORE_PERFORMER_HH
