/*
  score-align-grav.hh -- declare Type_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCORE_ALIGN_GRAV_HH
#define SCORE_ALIGN_GRAV_HH

#include "engraver.hh"

/**
  Group  a number of items across staffs
 */
class Type_align_engraver: public Engraver
{
  Horizontal_group_item  * align_p_;
public:
  TRANSLATOR_CLONE(Type_align_engraver);
    
  const char* type_ch_C_;
  int priority_i_;
  Type_align_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
};
#endif // SCORE_ALIGN_GRAV_HH
