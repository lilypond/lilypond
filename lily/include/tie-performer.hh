/*   
  tie-performer.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef TIE_PERFORMER_HH
#define TIE_PERFORMER_HH

#include "pqueue.hh"
#include "performer.hh"

struct CNote_melodic_tuple {
  Melodic_req *req_l_ ;
  Audio_note *note_l_;
  Moment end_;
  CNote_melodic_tuple ();
  CNote_melodic_tuple (Audio_note*, Melodic_req*, Moment);
  static int pitch_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);
  static int time_compare (CNote_melodic_tuple const &, CNote_melodic_tuple const &);  
};

inline int compare (CNote_melodic_tuple const &a, CNote_melodic_tuple const &b)
{
  return CNote_melodic_tuple::time_compare (a,b);
}


/**
   Manufacture ties.  Acknowledge notes, and put them into a
   priority queue. If we have a Tie_req, connect the notes that finish
   just at this time, and note that start at this time.

   TODO: should share code with Tie_engraver ?
 */
class Tie_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Tie_performer ();

private:
  PQueue<CNote_melodic_tuple> past_notes_pq_;
  Tie_req *req_l_;
  Array<CNote_melodic_tuple> now_notes_;
  Array<CNote_melodic_tuple> stopped_notes_;
  Link_array<Audio_tie> tie_p_arr_;
  
protected:
  virtual void do_post_move_processing ();
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Audio_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_requests ();
  virtual void process_acknowledged ();

};


#endif /* TIE_PERFORMER_HH */

