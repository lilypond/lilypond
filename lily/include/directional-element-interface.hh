/*
  directional-element.hh -- declare Directional_element

  source file of the GNU LilyPond music typesetter

  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "grob.hh"

// what is the advantage not having these two as STATICs of GROB -- jcn
void set_grob_direction (Grob *, Direction);
Direction get_grob_direction (Grob *);

#endif /* DIRECTIONAL_ELEMENT_HH */

