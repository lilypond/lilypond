/*
  tie-engraver.hh -- declare Ties_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Ties_GRAV_HH
#define Ties_GRAV_HH

#include "engraver.hh"

struct Head_melodic_tuple {
  Melodic_req *mel_l_ ;
  Note_head *head_l_;

  Head_melodic_tuple ();
  Head_melodic_tuple (Note_head*, Melodic_req*);
  static int compare (Head_melodic_tuple const &, Head_melodic_tuple const &);
};

class Ties_engraver : public Engraver {
  Link_array<Tie> end_tie_p_arr_;
  Link_array<Tie> tie_p_arr_;

  Tie_req *req_l_;
  Tie_req *end_req_l_;
  Array<Head_melodic_tuple> head_mel_tuple_arr_;
  Array<Head_melodic_tuple> left_head_mel_tuple_arr_;  
  int processed_ack_pass_i_;
  
  Link_array<Melodic_req *> end_melodic_req_l_arr_;
  Link_array<Melodic_req *> melodic_req_l_arr_;
    
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();
  virtual void process_acknowledged ();
  virtual void do_post_move_processing();
  virtual void do_pre_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Ties_engraver();
  
};

#endif // Ties_GRAV_HH
