/*
  timing-engraver.hh -- declare Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIMING_GRAV_HH
#define TIMING_GRAV_HH

#include "timing-translator.hh"
#include "engraver.hh"

/**
  Do time bookkeeping
 */
class Timing_engraver : public Timing_translator, public Engraver
{   

  Bar_req * bar_req_l_;
protected:
  virtual void fill_staff_info (Staff_info&);
  virtual bool do_try_music (Music * );
  virtual void do_post_move_processing ();
public:
  String which_bar (); 
  VIRTUAL_COPY_CONS(Translator);
  
};

#endif // TIMING_GRAV_HH
