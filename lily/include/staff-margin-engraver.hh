/*
  staff-margin-engraver.hh -- declare Staff_margin_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef STAFF_MARGIN_ENGRAVER_HH
#define STAFF_MARGIN_ENGRAVER_HH

#include "engraver.hh"

/**
  put (instrument) text to left of line
 */
class Staff_margin_engraver : public Engraver 
{
public:
  VIRTUAL_COPY_CONS(Translator);
  

  Staff_margin_engraver ();

protected:
  void acknowledge_element (Score_element_info);
  void do_pre_move_processing ();

private:
  Script * script_p_;
};

#endif // STAFF_MARGIN_ENGRAVER_HH
