/*   
  font-size-engraver.hh -- declare Font_size_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef FONT_SIZE_GRAV_HH
#define FONT_SIZE_GRAV_HH

#include "engraver.hh"

class Font_size_engraver : public Engraver {
  int size_i_;
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void  do_process_requests ();
public:
  Font_size_engraver ();
  
  VIRTUAL_COPY_CONS (Translator);
};

#endif /* FONT_SIZE_GRAV_HH */

