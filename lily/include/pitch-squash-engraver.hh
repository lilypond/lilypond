/*   
  pitch-squash-engraver.hh -- declare Pitch_squash_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef PITCH_SQUASH_GRAV_HH
#define PITCH_SQUASH_GRAV_HH

#include "engraver.hh"

class Pitch_squash_engraver : public Engraver {
public:
  
  VIRTUAL_COPY_CONS (Translator);
  virtual void acknowledge_element (Score_element_info);
  
};

#endif /* PITCH_SQUASH_GRAV_HH */

