/*
  slur-engraver.cc -- implement Slur_engraver

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "new-slur.hh"
#include "warn.hh"
#include "note-column.hh"
#include "context.hh"

#include "engraver.hh"
#include "spanner.hh"

class Slur_engraver : public Engraver
{
  Drul_array<Music *> events_;
  Music * running_slur_start_;
  Grob * slur_;

  Moment last_start_;

  void set_melisma (bool);

protected:
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

Slur_engraver::Slur_engraver ()
{
  events_[START] =events_[STOP] = 0;
  slur_ = 0;
  last_start_ = Moment (-1);
}

bool
Slur_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("slur-event"))
    {
      /*
	Let's not start more than one slur per moment.
      */
      Direction d = to_dir (m->get_property ("span-direction"));
      if (d == START)
	{
	  if (slur_)
	    return false;
	  
	  events_[START] = m;
	  return true;
	}
      else if (d == STOP)
	{
	  if (!slur_)
	    return false;
	  
	  events_[STOP] = m;
	  return true;
	}
    }
  return false;
}

void
Slur_engraver::set_melisma (bool m)
{
  context ()->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_grob (Grob_info info)
{
  Grob *e =info.grob_;
  if (Note_column::has_interface (info.grob_))
    {
      if (slur_)
	New_slur::add_column (slur_, e);
    }
  else
    {
      if (slur_)
	New_slur::add_extra_encompass (slur_, e);
    }
}

void
Slur_engraver::finalize ()
{
  if (slur_)
    slur_->warning (_("unterminated slur"));
}

void
Slur_engraver::process_music ()
{
  if (events_[STOP] && events_[START])
    {
      events_[START]->origin()->warning (_ ("Cannot start and end slur on same note"));
    }
  
  if (events_[START] && !slur_)
    {
      Music *ev = events_[START];
      slur_ = make_spanner ("Slur", events_[START]->self_scm ());
      if (Direction updown = to_dir (ev->get_property ("direction")))
	slur_->set_property ("direction", scm_int2num (updown));
    }

  set_melisma (slur_ && !events_[STOP]);
}

void
Slur_engraver::stop_translation_timestep ()
{
  if (events_[STOP])
    {
      slur_ = 0;
    }
  
  events_[START] = events_[STOP] = 0;
}



ENTER_DESCRIPTION (Slur_engraver,
/* descr */       "Build slurs grobs from slur events",
/* creats*/       "Slur",
/* accepts */     "slur-event",
/* acks  */      "note-column-interface accidental-interface fingering-interface script-interface",
/* reads */       "slurMelismaBusy",
/* write */       "");
