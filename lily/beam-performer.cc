/*
  beam-performer.cc -- implement Beam_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "warn.hh"
#include "music.hh"

#include "translator.icc"

class Beam_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Beam_performer);

protected:
  virtual bool try_music (Music *ev);
  void start_translation_timestep ();
  void process_music ();
  void set_melisma (bool);
private:
  Music *start_ev_;
  Music *now_stop_ev_;
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

bool
Beam_performer::try_music (Music *m)
{
  if (m->is_mus_type ("beam-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));

      if (d == START)
	start_ev_ = m;
      else if (d == STOP)
	now_stop_ev_ = m;
      return true;
    }
  return false;
}

ADD_TRANSLATOR (Beam_performer, "", "",
		"beam-event", "", "");

