/*
  slur-performer.cc -- implement Slur_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "warn.hh"
#include "music.hh"

/*
  this is C&P from beam_performer.
*/

class Slur_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Slur_performer);

protected:
  virtual bool try_music (Music *ev);
  void start_translation_timestep ();
  void process_music ();
  void set_melisma (bool);
private:
  Music *start_ev_;
  Music *now_stop_ev_;
  bool slur_;
};

Slur_performer::Slur_performer ()
{
  slur_ = false;
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

void
Slur_performer::process_music ()
{
  if (now_stop_ev_)
    {
      slur_ = false;
      set_melisma (false);
    }

  if (start_ev_)
    {
      slur_ = true;
      set_melisma (true);
    }
}

void
Slur_performer::set_melisma (bool ml)
{
  context ()->set_property ("slurMelismaBusy", ml ? SCM_BOOL_T : SCM_BOOL_F);
}

void
Slur_performer::start_translation_timestep ()
{
  start_ev_ = 0;
  now_stop_ev_ = 0;
}

bool
Slur_performer::try_music (Music *m)
{
  if (m->is_mus_type ("slur-event"))
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

#include "translator.icc"

ADD_TRANSLATOR (Slur_performer,
		"", "",
		"slur-event",
		"", "");

