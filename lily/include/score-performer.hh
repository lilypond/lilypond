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
public:
    NAME_MEMBERS();
    Score_performer();
    ~Score_performer();

protected:
    virtual Translator* ancestor_l( int l );
    virtual int depth_i() const;

    virtual void play_event( Midi_item* l );
    virtual void prepare( Moment mom );
    virtual void process();

private:
    Midi_stream* midi_stream_p_;
    Moment prev_mom_;
    Moment now_mom_;
};

#endif // SCORE_PERFORMER_HH
