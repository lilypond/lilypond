/*
  vertical-align-engraver.hh -- declare Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VERTICAL_ALIGN_GRAV_HH
#define VERTICAL_ALIGN_GRAV_HH

#include "engraver.hh"
class Axis_align_spanner;
class Vertical_align_engraver : public Engraver {
  Axis_align_spanner * valign_p_;
    
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Vertical_align_engraver();
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_creation_processing();
  virtual void do_removal_processing();
};

#endif // VERTICAL_ALIGN_GRAV_HH
