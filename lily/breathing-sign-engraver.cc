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
  Breathing_sign_engraver();
  VIRTUAL_COPY_CONS(Translator);
  
protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music();

  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  Breathing_sign_req * breathing_sign_req_l_;
  Score_element * breathing_sign_p_;
};

Breathing_sign_engraver::Breathing_sign_engraver()
{
  breathing_sign_p_ = 0;
  breathing_sign_req_l_ = 0;
}

bool
Breathing_sign_engraver::do_try_music (Music*r_l)
{
  if (Breathing_sign_req  * b= dynamic_cast <Breathing_sign_req *> (r_l))
    {
      breathing_sign_req_l_ = b;
      return true;
    }
 
  return false;
}

void
Breathing_sign_engraver::do_process_music()
{
  if(breathing_sign_req_l_)
    {
      SCM b = get_property ("basicBreathingSignProperties");
      breathing_sign_p_ = new Item (b);

      Breathing_sign::set_interface (breathing_sign_p_);

      announce_element (breathing_sign_p_, breathing_sign_req_l_);
    }
}

void 
Breathing_sign_engraver::do_pre_move_processing()
{
  if(breathing_sign_p_)
    {
      typeset_element(breathing_sign_p_);
      breathing_sign_p_ = 0;
    }
}

void
Breathing_sign_engraver::do_post_move_processing()
{
  breathing_sign_req_l_ = 0;
}

ADD_THIS_TRANSLATOR(Breathing_sign_engraver);
