/*
  swallow-engraver.hh -- declare Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SWALLOW_GRAV_HH
#define SWALLOW_GRAV_HH

#include "engraver.hh"

/**
  This engraver swallows everything given to it silently. The purpose of
  this is to prevent spurious "request junked" warnings.
 */
class Swallow_engraver : public Engraver {
protected:
  bool do_try_music (Music*) ;
public:
  VIRTUAL_COPY_CONS(Translator);
  
};
#endif // SWALLOW_GRAV_HH
