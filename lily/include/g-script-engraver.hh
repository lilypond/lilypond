/*
  script-engraver.hh -- part of GNU LilyPond

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef G_SCRIPT_GRAV
#define G_SCRIPT_GRAV

#include "engraver.hh"


class G_script_engraver : public Engraver {
  Link_array<G_script> script_p_arr_;
  Link_array<G_staff_side_item> staff_side_p_arr_;
  Link_array<Articulation_req> script_req_l_arr_;

public:
  VIRTUAL_COPY_CONS(Translator);
  
  G_script_engraver();
protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_requests ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void acknowledge_element (Score_element_info);
};

#endif // G_SCRIPT_GRAV
