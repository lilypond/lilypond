/*
  breathing_sign-engraver.cc -- implement Breathing_sign_engraver

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

TODO:

  . Cancel any beams running through the breathing sign
 ([e8 \breathe f e f] should become [e8] \breathe [f e f])
  . Spacing is not yet completely pretty

*/

#include "staff-symbol-referencer.hh"
#include "breathing-sign.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "engraver-group-engraver.hh"
#include "item.hh"
#include "engraver.hh"
#include "command-request.hh"

class Breathing_sign_engraver : public Engraver {
public:
  TRANSLATOR_DECLARATIONS(Breathing_sign_engraver);
  
protected:
  virtual bool try_music (Music *req);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();

private:
  Music * breathing_sign_req_;
  Grob * breathing_sign_;
};

Breathing_sign_engraver::Breathing_sign_engraver ()
{
  breathing_sign_ = 0;
  breathing_sign_req_ = 0;
}

bool
Breathing_sign_engraver::try_music (Music*r)
{
  breathing_sign_req_ = r;
  return true;
}

void
Breathing_sign_engraver::process_acknowledged_grobs ()
{
  if (breathing_sign_req_ && ! breathing_sign_)
    {
      SCM b = get_property ("BreathingSign");
      breathing_sign_ = new Item (b);

      announce_grob(breathing_sign_, breathing_sign_req_->self_scm());
      breathing_sign_req_ = 0;
    }
}

void 
Breathing_sign_engraver::stop_translation_timestep ()
{
  if (breathing_sign_)
    {
      typeset_grob (breathing_sign_);
      breathing_sign_ = 0;
    }
}

void
Breathing_sign_engraver::start_translation_timestep ()
{
  breathing_sign_req_ = 0;
}


ENTER_DESCRIPTION(Breathing_sign_engraver,
/* descr */       "",
/* creats*/       "BreathingSign",
/* accepts */     "breathing-event",
/* acks  */      "",
/* reads */       "",
/* write */       "");
