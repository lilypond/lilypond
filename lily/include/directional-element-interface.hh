/*
  directional-element.hh -- declare Directional_element

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "lily-proto.hh"
#include "direction.hh"

// what is the advantage not having these two as STATICs of GROB -- jcn
void set_grob_direction (Grob *, Direction);
Direction get_grob_direction (Grob *);

#endif /* DIRECTIONAL_ELEMENT_HH */

