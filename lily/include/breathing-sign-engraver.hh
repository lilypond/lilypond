/*
  breathing-sign-engraver.hh -- declare Breathing_Sign_engraver

  Copyright (C) 1999 Michael Krause

  written for the GNU LilyPond music typesetter

*/

#ifndef BREATHING_SIGN_ENGRAVER_HH
#define BREATHING_SIGN_ENGRAVER_HH

#include "engraver.hh"
#include "command-request.hh"
#include "breathing-sign.hh"

class Breathing_sign_engraver : public Engraver {
public:
  Breathing_sign_engraver();
  VIRTUAL_COPY_CONS(Translator);
  
protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests();

  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  Breathing_sign_req * breathing_sign_req_l_;
  Breathing_sign * breathing_sign_p_;
};

#endif // BREATHING_SIGN_ENGRAVER_HH
