/*
  span-bar-engraver.hh -- declare Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_BAR_GRAV_HH
#define SPAN_BAR_GRAV_HH

#include "engraver.hh"

/** 

  Make bars that span multiple "staffs". Catch bars, and span a
  Span_bar over them if we find more than 2 bars

  */
class Span_bar_engraver : public Engraver
{
  Span_bar * spanbar_p_;
  Array<Bar*> bar_l_arr_;
  Vertical_align_spanner * valign_l_;
public:
  TRANSLATOR_CLONE(Span_bar_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
    
  Span_bar_engraver();
protected:
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual Span_bar* get_span_bar_p() const;
};

#endif // SPAN_BAR_GRAV_HH
