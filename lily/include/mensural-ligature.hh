/*
  mensural-ligature.hh -- part of GNU LilyPond

  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef MENSURAL_LIGATURE_HH
#define MENSURAL_LIGATURE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

/*
 * These are all possible mensural ligature primitives.
 */
#define MLP_BB    0x01 // flexa with left downward cauda (for Brevis-Brevis)
#define MLP_sc    0x02 // last head of asc. sine proprietate cum perfectione
		       // (i.e. brevis head with downward right cauda)
#define MLP_ss    0x04 // last head of asc. sine proprietate sine perfectione
		       // (i.e. pure brevis head)
#define MLP_cs    0x08 // last head of asc. cum proprietate sine perfectione
		       // (i.e. brevis head with downward left cauda)
#define MLP_SS    0x10 // flexa with left upward cauda (for Semibr.-Semibr.)
#define MLP_LB    0x20 // core flexa (for Longa-Brevis)

#define MLP_NONE  0x00 // no output
#define MLP_SINGLE_HEAD	(MLP_sc | MLP_ss | MLP_cs)
#define MLP_FLEXA	(MLP_BB | MLP_SS | MLP_LB)
#define MLP_ANY		(MLP_FLEXA | MLP_SINGLE_HEAD)

struct Mensural_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM ));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);
};

#endif /* MENSURAL_LIGATURE_HH */
