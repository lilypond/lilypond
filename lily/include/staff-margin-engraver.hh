/*
  staff-margin-engraver.hh -- declare Staff_margin_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef STAFF_MARGIN_ENGRAVER_HH
#define STAFF_MARGIN_ENGRAVER_HH

#include "bar-script-engraver.hh"

/**
  put (instrument) text to left of line
 */
class Staff_margin_engraver : public Bar_script_engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Staff_margin_engraver ();
protected:
  void acknowledge_element (Score_element_info);
};

#endif // STAFF_MARGIN_ENGRAVER_HH
