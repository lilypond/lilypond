/*
  script-engraver.hh -- part of GNU LilyPond

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Script_GRAV
#define Script_GRAV

#include "engraver.hh"


class Script_engraver : public Engraver {
  Link_array<Script> script_p_arr_;
  Link_array<Articulation_req> script_req_l_arr_;

public:
  VIRTUAL_COPY_CONS(Translator);
  
  Script_engraver();
protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_requests ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void acknowledge_element (Score_element_info);
};

#endif // Script_GRAV
