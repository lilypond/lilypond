/*
  score-align-engraver.hh -- declare Type_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCOREF_ALIGN_GRAV_HH
#define SCOREF_ALIGN_GRAV_HH

#include "engraver.hh"

/**
  Group a number of items across staffs:

  Acknowledge items, put each priority in a separate column.  Put all
  columns in a horizontal align engraver.  We manufacture two types of
  elements: the alignment element and the columns.  This is probably
  more convenient, and I question the use having one without the
  other.
*/
class Score_priority_engraver : public Engraver
{
  Break_align_item * halign_p_;
  Link_array<Item> column_p_arr_;

  void add_horizontal_group (Item* , int p);
public:
  VIRTUAL_COPY_CONS(Translator);
  Score_priority_engraver ();
  
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
};

#endif // SCORE_ALIGN_GRAV_HH
