/*
  beam-performer.cc -- implement Beam_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Beam_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Beam_performer);

protected:
  void start_translation_timestep ();
  void process_music ();
  void set_melisma (bool);
  DECLARE_TRANSLATOR_LISTENER (beam);
private:
  Stream_event *start_ev_;
  Stream_event *now_stop_ev_;
  bool beam_;
};

Beam_performer::Beam_performer ()
{
  beam_ = false;
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

void
Beam_performer::process_music ()
{
  if (now_stop_ev_)
    {
      beam_ = false;
      set_melisma (false);
    }

  if (start_ev_)
    {
      beam_ = true;
      set_melisma (true);
    }
}

void
Beam_performer::set_melisma (bool ml)
{
  SCM b = get_property ("autoBeaming");
  if (!to_boolean (b))
    context ()->set_property ("beamMelismaBusy", ml ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Beam_performer::start_translation_timestep ()
{
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Beam_performer, beam);
void
Beam_performer::listen_beam (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));

  if (d == START)
    start_ev_ = ev;
  else if (d == STOP)
    now_stop_ev_ = ev;
}

ADD_TRANSLATOR (Beam_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "",

                /* write */
                ""
                );

