/*
  bar-number-engraver.hh -- declare Bar_number_grav

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BAR_NUMBER_GRAV_HH
#define BAR_NUMBER_GRAV_HH

#include "engraver.hh"

/**
  catch bars, and put a number over them.
 */
class Bar_number_engraver : public Engraver {
  Script * script_p_;
protected:

  void acknowledge_element (Score_element_info);
  void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Bar_number_engraver);
  Bar_number_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // BAR_NUMBER_GRAV_HH
