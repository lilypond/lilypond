/*
  extender-engraver.cc -- implement Extender_engraver

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>,
  Han-Wen Nienhuys, Jan Nieuwenhuizen.
  
*/

#include "flower-proto.hh"
#include "musical-request.hh"
#include "lyric-extender.hh"
#include "paper-column.hh"
#include "item.hh"
#include "engraver.hh"
#include "drul-array.hh"
#include "lyric-extender.hh"
#include "pqueue.hh"


/**
  Generate an centred extender.  Should make a Extender_spanner that
  typesets a nice centred extender of varying length depending on the
  gap between syllables.

  We remember the last Item that come across. When we get a
  request, we create the spanner, and attach the left point to the
  last lyrics, and the right point to any lyrics we receive by
  then.  */
class Extender_engraver : public Engraver
{
  Grob *last_lyric_l_;
  Grob *current_lyric_l_;
  Extender_req* req_l_;
  Spanner* extender_p_;
public:
  Extender_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep();
  virtual void start_translation_timestep ();
  virtual void process_music ();
private:

};


ADD_THIS_TRANSLATOR (Extender_engraver);

Extender_engraver::Extender_engraver ()
{
  current_lyric_l_ = 0;
  last_lyric_l_ = 0;
  extender_p_ = 0;
  req_l_ = 0;
}

void
Extender_engraver::acknowledge_grob (Grob_info i)
{
  // -> text_item
  if (i.elem_l_->has_interface (ly_symbol2scm("lyric-syllable-interface")))
    {
      current_lyric_l_ = i.elem_l_;
      if (extender_p_
	  && !extender_p_->get_bound (RIGHT)
	    )
	  {
	    Lyric_extender::set_textitem (extender_p_, RIGHT, dynamic_cast<Item*> (i.elem_l_));
	  }
    }
}


bool
Extender_engraver::try_music (Music* r)
{
  if (Extender_req* p = dynamic_cast <Extender_req *> (r))
    {
      if (req_l_)
	return false;

      req_l_ = p;
      return true;
    }
  return false;
}

void
Extender_engraver::finalize ()
{
  if (extender_p_)
    {
      req_l_->origin ()->warning (_ ("unterminated extender"));
      extender_p_->set_bound(RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Extender_engraver::process_music ()
{
  if (req_l_ && ! extender_p_)
    {
      if (!last_lyric_l_)
	{
	  req_l_->origin ()->warning (_ ("Nothing to connect extender to on the left.  Ignoring extender request."));
	  return;
	}
      
      extender_p_ = new Spanner (get_property ("LyricExtender"));


      Lyric_extender::set_textitem  (extender_p_, LEFT, last_lyric_l_);
      announce_grob (extender_p_, req_l_);
    }
}


void
Extender_engraver::stop_translation_timestep ()
{
  if (extender_p_)
    {
      typeset_grob (extender_p_);
      extender_p_ = 0;
    }

  if (current_lyric_l_)
    {
      last_lyric_l_ = current_lyric_l_;
      current_lyric_l_ =0;
    }
}

void
Extender_engraver::start_translation_timestep ()
{
  req_l_ = 0;
}


