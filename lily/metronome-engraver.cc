/*
  mark-engraver.cc -- implement Metronome_mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>

#include "bar-line.hh"
#include "time-signature.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "item.hh"

/**
  put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
  rehearsal marks.
 */
class Metronome_mark_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Metronome_mark_engraver);
protected:
  Item* text_;
  Grob *   bar_line_;
  
protected:
  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  void create_items (Music*);
  virtual bool try_music (Music *req);
  virtual void start_translation_timestep ();
  virtual void process_music ();
  
private:
  Music * mark_req_;
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ =0;
  mark_req_ = 0;
}

void
Metronome_mark_engraver::acknowledge_grob (Grob_info inf)
{
  if (Bar_line::has_interface (inf.grob_))
    {
      bar_line_ = inf.grob_;
    }
  else if (text_ && Time_signature::has_interface (inf.grob_))
    {
      text_->set_parent (inf.grob_, X_AXIS);
    }
}

void 
Metronome_mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      if (bar_line_ && !text_->get_parent (X_AXIS))
	text_->set_parent (bar_line_, X_AXIS);
      
      text_->set_grob_property ("side-support-elements" , get_property ("stavesFound"));
      typeset_grob (text_);
      text_ =0;
    }
}


void
Metronome_mark_engraver::create_items (Music *rq)
{
  if (text_)
    return;

  SCM s = get_property ("MetronomeMark");
  text_ = new Item (s);

  announce_grob(text_, rq->self_scm());
}

void
Metronome_mark_engraver::start_translation_timestep ()
{
  mark_req_ = 0;
}

bool
Metronome_mark_engraver::try_music (Music* r)
{
  mark_req_ = r;
  return true;
}

void
Metronome_mark_engraver::process_music ()
{
  if (mark_req_)
    {
      create_items (mark_req_);

      SCM proc = get_property ("metronomeMarkFormatter");
      SCM result= scm_call_2 (proc, mark_req_->self_scm (),
			      daddy_trans_->self_scm()); 
      
      text_->set_grob_property ("text", result);
    }
}

ENTER_DESCRIPTION(Metronome_mark_engraver,
/* descr */       "Engrave metro nome marking. This delegates the real work to the function in the metronomeMarkFormatter property",
/* creats*/       "MetronomeMark",
/* accepts */     "metronome-change-event",
/* acks  */       "time-signature-interface bar-line-interface",
/* reads */       "stavesFound metronomeMarkFormatter",
/* write */       "");
