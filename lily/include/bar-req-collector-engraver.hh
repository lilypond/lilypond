/*   
  bar-req-collect-engraver.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BAR_REQ_COLLECT_ENGRAVER_HH
#define BAR_REQ_COLLECT_ENGRAVER_HH

#include "engraver.hh"
class Bar_req_collect_engraver : public Engraver
{
  Bar_req* bar_req_l_;
public:
  VIRTUAL_COPY_CONS(Translator);
  void start_translation_timestep ();
  bool try_music (Music *);
};



#endif /* Bar_Req_COLLECT_ENGRAVER_HH */

