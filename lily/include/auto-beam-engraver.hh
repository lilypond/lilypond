/*   
  auto-beam-engraver.hh -- declare Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef AUTO_BEAM_ENGRAVER_HH
#define AUTO_BEAM_ENGRAVER_HH

#include "engraver.hh"

class Auto_beam_engraver : public Engraver
{
public:
  Auto_beam_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_process_music ();
  virtual void process_acknowledged ();
  virtual void do_creation_processing ();
private:
  void begin_beam ();
  void consider_end_and_begin (Moment test_mom);
  Beam* create_beam_p ();
  void end_beam ();
  void junk_beam ();
  bool same_grace_state_b (Score_element* e);
  void typeset_beam ();

  Moment shortest_mom_;
  Beam *finished_beam_p_;
  Array<Stem*>* stem_l_arr_p_;
  
  Moment last_add_mom_;
  Moment extend_mom_;


  Moment beam_start_moment_;
  Moment beam_start_location_;
  
  Timing_translator * timer_l_;
  // We act as if beam were created, and start a grouping anyway.
  Beaming_info_list*grouping_p_;  
  Beaming_info_list*finished_grouping_p_;
};

#endif /* AUTO_BEAM_ENGRAVER_HH */

