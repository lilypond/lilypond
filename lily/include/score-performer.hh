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
    DECLARE_MY_RUNTIME_TYPEINFO;
    Score_performer();
    ~Score_performer();

protected:
    virtual Translator* ancestor_l( int l );
    virtual int depth_i() const;

    virtual void finish();
    virtual Moment get_mom() const;
    virtual void prepare( Moment mom );
    virtual void process();
    virtual void set_score( Score* score_l );
    virtual void start();
    virtual int get_tempo_i() const;
    virtual void play_event(Midi_item*);
private:
    void header(Midi_stream&);

    Midi_def* midi_l_;

    Moment prev_mom_;
    Moment now_mom_;

    Link_array<Midi_item> midi_item_p_arr_;
};

#endif // SCORE_PERFORMER_HH
