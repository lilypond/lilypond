/*
  phrasing-slur-engraver.cc -- implement Phrasing_slur_engraver

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "directional-element-interface.hh"
#include "engraver.hh"
#include "event.hh"
#include "new-slur.hh"
#include "note-column.hh"
#include "spanner.hh"
#include "tie.hh"

/*
  It is possible that a slur starts and ends on the same note.  At
  least, it is for phrasing slurs: a note can be both beginning and
  ending of a phrase.
*/

class Phrasing_slur_engraver : public Engraver
{
  Drul_array<Music *> events_;
  Music * running_slur_start_;
  Link_array<Grob> slurs_;
  Link_array<Grob> end_slurs_;

protected:
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
};

Phrasing_slur_engraver::Phrasing_slur_engraver ()
{
  events_[START] =events_[STOP] = 0;
}

bool
Phrasing_slur_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("phrasing-slur-event"))
    {
      /*
	Let's not start more than one slur per moment.
      */
      Direction d = to_dir (m->get_property ("span-direction"));
      if (d == START)
	{
	  events_[START] = m;
	  return true;
	}
      else if (d == STOP)
	{
	  if (slurs_.is_empty ())
	    return false;
	  
	  events_[STOP] = m;
	  return true;
	}
    }
  return false;
}


void
Phrasing_slur_engraver::acknowledge_grob (Grob_info info)
{
  Grob *e =info.grob_;
  if (Note_column::has_interface (info.grob_))
    {
      for (int i = slurs_.size (); i--; )
	New_slur::add_column (slurs_[i], e);
      for (int i = end_slurs_.size (); i-- ; )
	New_slur::add_column (end_slurs_[i], e);
    }
  else
    {
      /*
	TODO: maybe take more objects?  
       */
      for (int i = slurs_.size (); i--; )
	New_slur::add_extra_encompass (slurs_[i], e);
      for (int i = end_slurs_.size (); i--; )
	New_slur::add_extra_encompass (end_slurs_[i], e);
    }
}

void
Phrasing_slur_engraver::finalize ()
{
  if (slurs_.size ())
    slurs_[0]->warning (_("unterminated slur"));
}

void
Phrasing_slur_engraver::process_music ()
{
  if (events_[STOP])
    {
      end_slurs_ = slurs_;
      slurs_.clear ();
    }
  
  if (events_[START] && slurs_.is_empty ())
    {
      Music *ev = events_[START];

      Grob * slur = make_spanner ("Slur", events_[START]->self_scm ());
      Direction updown = to_dir (ev->get_property ("direction"));
      if (updown)
	set_grob_direction (slur, updown);

      slurs_.push (slur);
    }
}

void
Phrasing_slur_engraver::stop_translation_timestep ()
{
  end_slurs_.clear ();
  events_[START] = events_[STOP] = 0;
}


ENTER_DESCRIPTION (Phrasing_slur_engraver,
/* descr */       "Print phrasing slurs. Similar to @ref{Slur_engraver}",
/* creats*/       "PhrasingSlur",
/* accepts */     "phrasing-slur-event",
/* acks  */       "note-column-interface slur-interface",
/* reads */       "",
/* write */       "");
