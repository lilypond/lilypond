/*
  time-signature-engraver.cc -- implement Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "time-signature.hh"
#include "command-request.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"

/**
  generate time_signatures. 
  */
class Time_signature_engraver : public Engraver
{
  Item * time_signature_p_;
  SCM last_time_fraction_;

protected:
  virtual void stop_translation_timestep ();
  virtual void process_music ();
public:
  TRANSLATOR_DECLARATIONS(Time_signature_engraver);
};


Time_signature_engraver::Time_signature_engraver ()
{ 
  time_signature_p_ =0;
  last_time_fraction_ = SCM_BOOL_F;
}

void
Time_signature_engraver::process_music ()
{
  /*
    not rigorously safe, since the value might get GC'd and
    reallocated in the same spot */
  SCM fr= get_property ("timeSignatureFraction");
  if (!time_signature_p_ && last_time_fraction_ != fr)
    {
      last_time_fraction_ = fr; 
      time_signature_p_ = new Item (get_property ("TimeSignature"));
      time_signature_p_->set_grob_property ("fraction",fr);

      if (time_signature_p_)
	announce_grob(time_signature_p_, SCM_EOL);
    }
}

void
Time_signature_engraver::stop_translation_timestep ()
{
  if (time_signature_p_) 
    {
      typeset_grob (time_signature_p_);
      time_signature_p_ =0;
    }
}
 

ENTER_DESCRIPTION(Time_signature_engraver,
/* descr */       "Create a TimeSignature whenever @code{timeSignatureFraction} changes",
/* creats*/       "TimeSignature",
/* acks  */       "",
/* reads */       "",
/* write */       "");
