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
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_process_requests ();
  virtual void process_acknowledged ();

private:
  void begin_beam ();
  void consider_end_and_begin ();
  Beam* create_beam_p ();
  void end_beam ();
  void junk_beam ();
  void typeset_beam ();

  Moment shortest_mom_;
  Beam *finished_beam_p_;
  Array<Stem*>* stem_l_arr_p_;
  Moment last_add_mom_;
  Moment extend_mom_;
  Rhythmic_grouping*grouping_p_;
  Rhythmic_grouping*finished_grouping_p_;
};

#endif /* AUTO_BEAM_ENGRAVER_HH */

