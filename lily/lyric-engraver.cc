/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  virtual void stop_translation_timestep();
  virtual bool try_music (Music *);
  virtual void create_grobs ();
  virtual void start_translation_timestep ();
  
public:
  Lyric_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Lyric_req * req_l_;
  Item* text_p_;
};

ADD_THIS_TRANSLATOR (Lyric_engraver);


Lyric_engraver::Lyric_engraver()
{
  text_p_ =0;
  req_l_ =0;
}

bool
Lyric_engraver::try_music (Music*r)
{
  if (Lyric_req* l = dynamic_cast <Lyric_req *> (r))
    {
      if (req_l_)
	return false;
      req_l_ =l;
      return true;
    }
  return false;
}

void
Lyric_engraver::create_grobs ()
{
  if (req_l_)
    {
      text_p_=  new Item (get_property ("LyricText"));
      
      text_p_->set_grob_property ("text", req_l_->get_mus_property ("text"));

      /*
	We can't reach the notehead where we're centered from here. So
	we kludge.

	(UGH UGH, pulled amount of space out of thin air)
      */
      
      text_p_->translate_axis (0.66, X_AXIS);
      
      announce_grob (text_p_, req_l_);
      req_l_ = 0;
    }
}

void
Lyric_engraver::stop_translation_timestep()
{
  if (text_p_)
    {
      typeset_grob (text_p_);
      text_p_ =0;
    }
}

void
Lyric_engraver::start_translation_timestep ()
{
  req_l_ =0;
}


