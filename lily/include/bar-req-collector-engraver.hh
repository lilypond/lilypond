/*   
  bar-req-collect-engraver.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BAR_REQ_COLLECT_ENGRAVER_HH
#define BAR_REQ_COLLECT_ENGRAVER_HH

#include "engraver.hh"
class Bar_req_collect_engraver : public Engraver
{
  Bar_req* bar_req_l_;
public:
  VIRTUAL_COPY_CONS(Translator);
  void do_post_move_processing ();
  bool do_try_music (Music *);
};



#endif /* Bar_Req_COLLECT_ENGRAVER_HH */

