/*
  time_signature-reg.cc -- implement Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "time-signature.hh"
#include "command-request.hh"
#include "engraver.hh"


#include "engraver-group-engraver.hh"


/**
  generate time_signatures. 
  */
class Time_signature_engraver : public Engraver {
protected:
  virtual void do_process_music();
  virtual void do_pre_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Item * time_signature_p_;
  SCM last_time_fraction_;
  Time_signature_engraver();
};


Time_signature_engraver::Time_signature_engraver()
{ 
  time_signature_p_ =0;
  last_time_fraction_ = SCM_EOL;
}

void
Time_signature_engraver::do_process_music()
{
  /*
    not rigorously safe, since the value might get GC'd and
    reallocated in the same spot */
  SCM fr= get_property ("timeSignatureFraction");
  if (last_time_fraction_ != fr)
    {
      last_time_fraction_ = fr; 
      time_signature_p_ = new Item (get_property ("basicTimeSignatureProperties"));
      time_signature_p_->set_elt_property ("fraction",fr);
    }
  
  if (time_signature_p_)
    announce_element (time_signature_p_, 0);
}

void
Time_signature_engraver::do_pre_move_processing()
{
  if (time_signature_p_) 
    {
      typeset_element (time_signature_p_);
      time_signature_p_ =0;
    }
}


ADD_THIS_TRANSLATOR(Time_signature_engraver);
 

