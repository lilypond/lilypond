/*
  tie-engraver.hh -- declare Tie_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_GRAV_HH
#define TIE_GRAV_HH

#include "engraver.hh"


/**
   Make a (one) tie from a request.  To be used in conjunction  with
   the Note_head_engraver.  Obsolete now that Thread context is junked.
 */
class Tie_engraver : public Engraver {
  Tie * end_tie_p_;
  Tie * tie_p_;
  Moment end_mom_;
  Tie_req * req_l_;
  Direction dir_;
  Tie_req *end_req_l_;
  Melodic_req * end_melodic_req_l_;
  Melodic_req  * melodic_req_l_;
    
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_request (Request*);
  virtual void do_process_requests();
  virtual void do_post_move_processing();
  virtual void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Tie_engraver);
  Tie_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // TIE_GRAV_HH
