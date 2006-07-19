/*
  tempo-performer.cc -- implement Tempo_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"

#include "audio-item.hh"
#include "duration.hh"
#include "stream-event.hh"

#include "translator.icc"

class Tempo_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Tempo_performer);
  ~Tempo_performer ();

protected:

  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (metronome_change);
private:
  Stream_event *tempo_event_;
  Audio_tempo *audio_;
};

Tempo_performer::Tempo_performer ()
{
  tempo_event_ = 0;
  audio_ = 0;
}

Tempo_performer::~Tempo_performer ()
{
}

void
Tempo_performer::process_music ()
{
  if (tempo_event_)
    {
      SCM met = tempo_event_->get_property ("metronome-count");
      Duration *d = unsmob_duration (tempo_event_->get_property ("tempo-unit"));

      Rational r = (d->get_length () / Moment (Rational (1, 4)) * Moment (scm_to_int (met))).main_part_;

      audio_ = new Audio_tempo (r.to_int ());

      Audio_element_info info (audio_, tempo_event_);
      announce_element (info);
      tempo_event_ = 0;
    }
}

void
Tempo_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      play_element (audio_);
      audio_ = 0;
    }
}

IMPLEMENT_TRANSLATOR_LISTENER (Tempo_performer, metronome_change);
void
Tempo_performer::listen_metronome_change (Stream_event *event)
{
  tempo_event_ = event;
}

ADD_TRANSLATOR (Tempo_performer, "", "",
		"metronome-change-event",
		"", "");
