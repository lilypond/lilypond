/*   
  beam-engraver.hh -- declare Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef BEAM_ENGRAVER_HH
#define BEAM_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"

class Beam_engraver : public Engraver {
  Drul_array<Beam_req*> reqs_drul_;

  Beam *finished_beam_p_;
  Beam *beam_p_;
  Beam_req * prev_start_req_;
  Rhythmic_grouping*grouping_p_;
  Rhythmic_grouping*finished_grouping_p_;
  
  void typeset_beam ();
protected:
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
  virtual void do_process_requests ();
public:
  Beam_engraver ();
  VIRTUAL_COPY_CONS (Translator);
};

#endif /* BEAM_ENGRAVER_HH */

