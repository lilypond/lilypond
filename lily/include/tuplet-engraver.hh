/*   
  auto-plet-engraver.hh -- declare Auto_plet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AUTO_PLET_ENGRAVER_HH
#define AUTO_PLET_ENGRAVER_HH

#include "engraver.hh"

class Tuplet_engraver : public Engraver
{
  void typeset_all ();
public:
  VIRTUAL_COPY_CONS(Translator);


protected:
  Link_array<Compressed_music> compressed_music_arr_;
  Array<Moment> stop_moments_;
  Link_array<Tuplet_spanner> started_span_p_arr_;


  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*r);
  virtual void do_process_requests ();
  virtual void do_post_move_processing ();
};



#endif /* AUTO_PLET_ENGRAVER_HH */
