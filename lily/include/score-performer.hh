/*
  score-performer.hh -- declare Score_performer

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef SCORE_PERFORMER_HH
#define SCORE_PERFORMER_HH

#include "performer-group-performer.hh"
#include "global-translator.hh"

/**
  Top level performer. Completely takes care of MIDI output
 */
class Score_performer: 
  public Performer_group_performer, public Global_translator 
{
public:
  TRANSLATOR_CLONE(Score_performer);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Score_performer();
  ~Score_performer();
  Performance *performance_p_;

protected:
  virtual void finish();
  virtual void prepare (Moment mom);
  virtual void process();
  virtual void start();
  virtual void do_add_processing ();
  virtual int get_tempo_i() const;
  virtual void play (Audio_element* p);
  virtual Music_output *get_output_p ();
private:
  void header (Midi_stream&);

  Audio_column* audio_column_l_;
};

#endif // SCORE_PERFORMER_HH
