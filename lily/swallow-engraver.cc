/*
  swallow-reg.cc -- implement Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "swallow-engraver.hh"



ADD_THIS_TRANSLATOR(Swallow_engraver);


bool
Swallow_engraver::try_music (Music*) 
{
  return true;
}
