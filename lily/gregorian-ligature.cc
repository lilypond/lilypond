/*   
  gregorian-ligature.cc -- implement Gregorian_ligature

  source file of the GNU LilyPond music typesetter

  (c) 2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "grob.hh"

class Gregorian_ligature {
public:
  static bool has_interface (Grob *);
};


/*
  CHECK ME -- does prefix-set come from here ? 
 */
ADD_INTERFACE (Gregorian_ligature, "gregorian-ligature-interface",
	       "A gregorian ligature",
	       "virga stropha inclinatum auctum descendens ascendens "
	       "pes-or-flexa semivocalis oriscus quilisma prefix-set deminutum");
