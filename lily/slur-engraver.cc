/*
  slur-engraver.cc -- implement Slur_engraver

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "slur.hh"
#include "note-column.hh"
#include "context.hh"
#include "directional-element-interface.hh"
#include "engraver.hh"
#include "spanner.hh"
#include "tie.hh"

/*
  It is possible that a slur starts and ends on the same note.  At
  least, it is for phrasing slurs: a note can be both beginning and
  ending of a phrase.
*/

class Slur_engraver : public Engraver
{
  Drul_array<Music *> events_;
  Music * running_slur_start_;
  Link_array<Grob> slurs_;
  Link_array<Grob> end_slurs_;

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
  events_[START] = events_[STOP] = 0;
}

bool
Slur_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("slur-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));
      if (d == START)
	{
	  events_[START] = m;
	  return true;
	}
      else if (d == STOP)
	{
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
  Grob *e = info.grob_;
  if (Note_column::has_interface (info.grob_))
    {
      for (int i = slurs_.size (); i--; )
	Slur::add_column (slurs_[i], e);
      for (int i = end_slurs_.size (); i-- ; )
	Slur::add_column (end_slurs_[i], e);
    }
  else
    {
      SCM inside = e->get_property ("inside-slur");
      if (Tie::has_interface (e)
	  || to_boolean (inside))
	{
	  for (int i = slurs_.size (); i--; )
	    Slur::add_extra_encompass (slurs_[i], e);
	  for (int i = end_slurs_.size (); i--; )
	    Slur::add_extra_encompass (end_slurs_[i], e);
	}
      else if (inside == SCM_BOOL_F)
	{
	  Grob *slur = slurs_.size()?slurs_[0] : 0;
	  slur =  (end_slurs_.size () && !slur)
	    ? end_slurs_[0] : slur;

	  if (slur)
	    {
	      e->add_offset_callback (Slur::outside_slur_callback_proc, Y_AXIS);
	      e->set_property ("slur", slur->self_scm());
	    }
	}
    }
}

void
Slur_engraver::finalize ()
{
  if (slurs_.size ())
    slurs_[0]->warning (_("unterminated slur"));
}

void
Slur_engraver::process_music ()
{
  if (events_[STOP])
    {
      if (slurs_.size() == 0)
	{
	  events_[STOP]->origin()->warning (_ ("No slur to end"));
	}
      
      end_slurs_ = slurs_;
      slurs_.clear ();
    }
  
  if (events_[START] && slurs_.is_empty ())
    {
      Music *ev = events_[START];

      bool double_slurs = to_boolean (get_property ("doubleSlurs"));

      Grob * slur = make_spanner ("Slur", events_[START]->self_scm ());
      Direction updown = to_dir (ev->get_property ("direction"));
      if (updown && !double_slurs)
	set_grob_direction (slur, updown);

      slurs_.push (slur);

      if (double_slurs)
	{
	  set_grob_direction (slur, DOWN);
	  slur = make_spanner ("Slur", events_[START]->self_scm ());
	  set_grob_direction (slur, UP);
	  slurs_.push (slur); 
	}
    }
  set_melisma (slurs_.size ());
}

void
Slur_engraver::stop_translation_timestep ()
{
  end_slurs_.clear ();
  events_[START] = events_[STOP] = 0;
}

ADD_TRANSLATOR (Slur_engraver,
  /* descr */       "Build slurs grobs from slur events",
  /* creats*/       "Slur",
  /* accepts */     "slur-event",
  /* acks  */      "note-column-interface accidental-interface fingering-interface script-interface tie-interface text-script-interface",
  /* reads */       "slurMelismaBusy doubleSlurs",
  /* write */       "");
