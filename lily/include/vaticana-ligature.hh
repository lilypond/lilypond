/*
  vaticana-ligature.hh

  source file of the GNU LilyPond music typesetter

  (c) 2003--2008 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef VATICANA_LIGATURE_HH
#define VATICANA_LIGATURE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Vaticana_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE();
};

/*
 * Ligature context info: these attributes are derived from the head
 * prefixes by considering the current and the two neighbouring heads.
 *
 * The below definition extends those in gregorian-ligature.hh.
 */
#define STACKED_HEAD 0x0100 // this head is stacked on the previous one

#endif // VATICANA_LIGATURE_HH
