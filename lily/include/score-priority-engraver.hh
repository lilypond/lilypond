/*
  score-align-engraver.hh -- declare Type_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCOREF_ALIGN_GRAV_HH
#define SCOREF_ALIGN_GRAV_HH

#include "engraver.hh"

/**
  Group  a number of items across staffs
 */
class Score_priority_engraver : public Engraver
{
  Hash_table<int, Horizontal_group_item  *> align_p_tab_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Score_priority_engraver ();
  
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
};

#endif // SCORE_ALIGN_GRAV_HH
