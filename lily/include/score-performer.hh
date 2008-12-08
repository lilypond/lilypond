/*
  score-performer.hh -- declare Score_performer

  (c) 1996--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SCORE_PERFORMER_HH
#define SCORE_PERFORMER_HH

#include "moment.hh"
#include "performer-group.hh"

/**
   Top level performer. Completely takes care of MIDI output
*/
class Score_performer : public Performer_group
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Translator_group, Score_performer);
  Performance *performance_;

  ~Score_performer ();
  Score_performer ();

protected:
  DECLARE_LISTENER (finish);
  DECLARE_LISTENER (prepare);
  DECLARE_LISTENER (one_time_step);

  /* Engraver_group_engraver interface */
  virtual void connect_to_context (Context *);
  virtual void disconnect_from_context ();
  virtual void initialize ();
  virtual void announce_element (Audio_element_info);
  virtual void derived_mark () const;
  virtual void acknowledge_audio_elements ();
private:
  void header (Midi_stream &);

  Audio_column *audio_column_;
  bool skipping_;
  Moment skip_start_mom_;
  Moment offset_mom_;
};

#endif // SCORE_PERFORMER_HH
