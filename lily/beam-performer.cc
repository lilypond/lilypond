/*
  beam-performer.cc -- implement Beam_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer.hh"
#include "event.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-translator.hh"
#include "warn.hh"

/**
Convert evs to audio beams.
*/
class Beam_performer : public Performer {
public:
  TRANSLATOR_DECLARATIONS(Beam_performer);
  
protected:
  virtual bool try_music (Music *ev) ;
  virtual void start_translation_timestep ();
  virtual void process_music ();
  void set_melisma (bool);
private:
  Music *start_ev_;
  Music *now_stop_ev_;
  bool beam_;
};

void 
Beam_performer::process_music ()
{
  if (now_stop_ev_)
    {
      beam_ = false;
    }

  if (start_ev_)
    {
      if (beam_)
	{
	  start_ev_->origin ()->warning (_ ("already have a beam"));
	  return;
	}
      
      beam_ = true;
      set_melisma (true);
    }
}


void
Beam_performer::set_melisma (bool ml)
{
  SCM b = get_property ("autoBeaming");
  if (!to_boolean (b))
    daddy_trans_->set_property ("beamMelismaBusy", ml ? SCM_BOOL_T :SCM_BOOL_F);
}


void
Beam_performer::start_translation_timestep ()
{
  if (beam_)
    {
      set_melisma (true);
    }
  
  start_ev_ = 0;
  now_stop_ev_ = 0;
}


 
bool
Beam_performer::try_music (Music *m)
{
  if (m->is_mus_type ("beam-event"))
    {
      Direction d = to_dir (m->get_mus_property ("span-direction"));

      if (d == START)
	{
	  start_ev_ = m;
	}
      else if (d==STOP)
	{
	  now_stop_ev_ = m;
	}
      return true;
    }
  return false;
}

ENTER_DESCRIPTION(Beam_performer,"","",
		  "beam-event","","","");

Beam_performer::Beam_performer()
{
  beam_ = false;
}
