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
  Grob *last_lyric_;
  Grob *current_lyric_;
  Extender_req* req_;
  Spanner* extender_;
public:
  TRANSLATOR_DECLARATIONS(Extender_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
private:

};




Extender_engraver::Extender_engraver ()
{
  current_lyric_ = 0;
  last_lyric_ = 0;
  extender_ = 0;
  req_ = 0;
}

void
Extender_engraver::acknowledge_grob (Grob_info i)
{
  // -> text_item
  if (i.grob_->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      current_lyric_ = i.grob_;
      if (extender_
	  && !extender_->get_bound (RIGHT)
	    )
	  {
	    Lyric_extender::set_textitem (extender_, RIGHT, dynamic_cast<Item*> (i.grob_));
	  }
    }
}


bool
Extender_engraver::try_music (Music* r)
{
  if (Extender_req* p = dynamic_cast <Extender_req *> (r))
    {
      if (req_)
	return false;

      req_ = p;
      return true;
    }
  return false;
}

void
Extender_engraver::finalize ()
{
  if (extender_)
    {
      req_->origin ()->warning (_ ("unterminated extender"));
      extender_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Extender_engraver::process_music ()
{
  if (req_ && ! extender_)
    {
      if (!last_lyric_)
	{
	  req_->origin ()->warning (_ ("Nothing to connect extender to on the left.  Ignoring extender request."));
	  return;
	}
      
      extender_ = new Spanner (get_property ("LyricExtender"));


      Lyric_extender::set_textitem (extender_, LEFT, last_lyric_);
      announce_grob(extender_, req_->self_scm());
    }
}


void
Extender_engraver::stop_translation_timestep ()
{
  if (extender_)
    {
      typeset_grob (extender_);
      extender_ = 0;
    }

  if (current_lyric_)
    {
      last_lyric_ = current_lyric_;
      current_lyric_ =0;
    }
}

void
Extender_engraver::start_translation_timestep ()
{
  req_ = 0;
}


ENTER_DESCRIPTION(Extender_engraver,
/* descr */       "Create lyric extenders",
/* creats*/       "LyricExtender",
/* accepts */     "general-music",
/* acks  */      "lyric-syllable-interface",
/* reads */       "",
/* write */       "");
