/*
  score-halign-grav.hh -- declare Score_horizontal_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef FSCORE_HALIGN_GRAV_HH
#define FSCORE_HALIGN_GRAV_HH

#include "engraver.hh"

class Priority_horizontal_align_engraver : public Engraver {
  Break_align_item * halign_p_;
public:
  TRANSLATOR_CLONE(Priority_horizontal_align_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Priority_horizontal_align_engraver();
protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
};
#endif // Priority_HALIGN_GRAV_HH
