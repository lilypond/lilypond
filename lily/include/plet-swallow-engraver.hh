/*
  plet-swallow-engraver.hh -- declare Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PLET_SWALLOW_ENGRAVER_HH
#define PLET_SWALLOW_ENGRAVER_HH

#include "swallow-engraver.hh"

/**
  This engraver swallows plets silently.
 */
class Plet_swallow_engraver : public Swallow_engraver 
{
public:
  VIRTUAL_COPY_CONS(Translator);
  

protected:
  virtual bool do_try_music (Music*);
};

#endif // PLET_SWALLOW_ENGRAVER_HH
