/*
  mensural-ligature.hh -- part of GNU LilyPond

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Juergen Reuter <reuter@ipd.uka.de>,
  Pal Benko <benkop@freestart.hu>
*/

#ifndef MENSURAL_LIGATURE_HH
#define MENSURAL_LIGATURE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"


/*
 * These are all possible mensural ligature primitives.
 */
#define MLP_NONE 0x00 // no output
#define MLP_UP 0x01 // upward left stem
#define MLP_DOWN 0x02 // downward left stem
#define MLP_BREVIS 0x04 // mensural brevis head
#define MLP_LONGA 0x08 // mensural brevis head with right cauda
#define MLP_MAXIMA 0x10 // mensural maxima head without stem
#define MLP_FLEXA 0x20 // mensural flexa-like shape

#define MLP_STEM (MLP_UP | MLP_DOWN)
#define MLP_SINGLE_HEAD (MLP_BREVIS | MLP_LONGA | MLP_MAXIMA)
#define MLP_ANY (MLP_FLEXA | MLP_SINGLE_HEAD)

struct Mensural_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif /* MENSURAL_LIGATURE_HH */
