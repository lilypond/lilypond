/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "musical-request.hh"
#include "item.hh"
#include "paper-def.hh"
#include "font-metric.hh"
#include "side-position-interface.hh"

/**
   Generate texts for lyric syllables.  We only do one lyric at a time.  
   Multiple copies of this engraver should be used to do multiple voices.
 */
class Lyric_engraver : public Engraver 
{
protected:
  virtual void stop_translation_timestep ();
  virtual bool try_music (Music *);
  virtual void process_acknowledged_grobs ();
  virtual void start_translation_timestep ();
  
public:
  TRANSLATOR_DECLARATIONS(Lyric_engraver);
private:
  Lyric_req * req_;
  Item* text_;
};




Lyric_engraver::Lyric_engraver ()
{
  text_ =0;
  req_ =0;
}

bool
Lyric_engraver::try_music (Music*r)
{
  if (Lyric_req* l = dynamic_cast <Lyric_req *> (r))
    {
      if (req_)
	return false;
      req_ =l;
      return true;
    }
  return false;
}

void
Lyric_engraver::process_acknowledged_grobs ()
{
  if (req_)
    {
      text_=  new Item (get_property ("LyricText"));
      
      text_->set_grob_property ("text", req_->get_mus_property ("text"));

      /*
	We can't reach the notehead where we're centered from here. So
	we kludge.

 (UGH UGH, pulled amount of space out of thin air)
      */
      
      text_->translate_axis (0.66, X_AXIS);
      
      announce_grob(text_, req_->self_scm());
      req_ = 0;
    }
}

void
Lyric_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      typeset_grob (text_);
      text_ =0;
    }
}

void
Lyric_engraver::start_translation_timestep ()
{
  req_ =0;
}


ENTER_DESCRIPTION(Lyric_engraver,
/* descr */       "",
/* creats*/       "",
/* accepts */     "general-music",
/* acks  */      "",
/* reads */       "",
/* write */       "");
