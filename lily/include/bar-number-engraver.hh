/*
  bar-number-engraver.hh -- declare Bar_number_grav

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BAR_NUMBER_GRAV_HH
#define BAR_NUMBER_GRAV_HH

#include "bar-script-engraver.hh"

class Bar_number_engraver : public Bar_script_engraver {
protected:
  void do_process_requests ();
public:
  VIRTUAL_COPY_CONS(Translator);
  Bar_number_engraver();
};
#endif // BAR_NUMBER_GRAV_HH
