/*   
  ctie-engraver.hh -- declare Tie_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CTIE_ENGRAVER_HH
#define CTIE_ENGRAVER_HH

#include "pqueue.hh"
#include "engraver.hh"

struct CHead_melodic_tuple {
  Melodic_req *mel_l_ ;
  Note_head *head_l_;
  Moment end_;
  CHead_melodic_tuple ();
  CHead_melodic_tuple (Note_head*, Melodic_req*, Moment);
  static int pitch_compare (CHead_melodic_tuple const &, CHead_melodic_tuple const &);
  static int time_compare (CHead_melodic_tuple const &, CHead_melodic_tuple const &);  
};

inline int compare (CHead_melodic_tuple const &a, CHead_melodic_tuple const &b)
{
  return CHead_melodic_tuple::time_compare (a,b);
}


class Tie_engraver : public Engraver
{
  PQueue<CHead_melodic_tuple> past_notes_pq_;
  Tie_req *req_l_;
  Array<CHead_melodic_tuple> now_heads_;
  Array<CHead_melodic_tuple> stopped_heads_;
  Link_array<Tie> tie_p_arr_;
  
protected:
  virtual void do_post_move_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_requests ();
  virtual void process_acknowledged ();

public:
  VIRTUAL_COPY_CONS(Translator);
  Tie_engraver();
};

#endif /* CTIE_ENGRAVER_HH */

