/*   
  auto-plet-engraver.hh -- declare Auto_plet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AUTO_PLET_ENGRAVER_HH
#define AUTO_PLET_ENGRAVER_HH

#include "engraver.hh"

class Tuplet_engraver : public Engraver
{
  void typeset_all ();
public:
  Tuplet_engraver ();
  DECLARE_MY_RUNTIME_TYPEINFO;
  TRANSLATOR_CLONE(Tuplet_engraver);

protected:
  Link_array<Bracket_req> bracket_req_arr_;

  Link_array<Slur> started_span_p_arr_;
  Link_array<Slur> stop_now_span_p_arr_;

  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();
  virtual bool do_try_request (Request*r);
  virtual void do_process_requests ();
  virtual void do_post_move_processing ();
};



#endif /* AUTO_PLET_ENGRAVER_HH */
