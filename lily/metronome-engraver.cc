/*
  mark-engraver.cc -- implement Metronome_mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>

#include "note-column.hh"
#include "bar-line.hh"
#include "time-signature.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "item.hh"
#include "context.hh"

/**
  put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
  rehearsal marks.
 */
class Metronome_mark_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Metronome_mark_engraver);
protected:
  Item *text_;
  Grob *bar_line_;
  Music *mark_ev_;
  
  void create_items (Music*);
protected:
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *ev);
  virtual void process_music ();
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ =0;
  mark_ev_ = 0;
}

void 
Metronome_mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      Grob*mc = unsmob_grob (get_property( "currentMusicalColumn"));
      text_->set_parent (mc, X_AXIS);
      text_->set_property ("side-support-elements" , get_property ("stavesFound"));
      
      text_ =0;
    }
  mark_ev_ = 0;
}


void
Metronome_mark_engraver::create_items (Music *rq)
{
  if (text_)
    return;

  text_ = make_item ("MetronomeMark");
  announce_grob (text_, rq->self_scm ());

}


bool
Metronome_mark_engraver::try_music (Music* r)
{
  mark_ev_ = r;
  return true;
}

void
Metronome_mark_engraver::process_music ()
{
  if (mark_ev_)
    {
      create_items (mark_ev_);

      SCM proc = get_property ("metronomeMarkFormatter");
      SCM result= scm_call_2 (proc, mark_ev_->self_scm (),
			      daddy_context_->self_scm ()); 
      
      text_->set_property ("text", result);
    }
}

ENTER_DESCRIPTION (Metronome_mark_engraver,
/* descr */       "Engrave metro nome marking. This delegates the formatting work "
		   "to the function in the metronomeMarkFormatter property. "
		   "The mark is put over all staves. "
		   "The staves are taken from the @code{stavesFound} property, "
		   "which is maintained by @code{@ref{Staff_collecting_engraver}}. "
	
		   ,
/* creats*/       "MetronomeMark",
/* accepts */     "metronome-change-event",
		   /* acks  */       "",
/* reads */       "stavesFound metronomeMarkFormatter",
/* write */       "");
