/*
  template1.cc -- instantiate some List classes

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "idealspacing.hh"
#include "plist.hh"
#include "p-col.hh"
#include "item.hh"
#include "musical-request.hh"
#include "spanner.hh"
#include "pcursor.tcc"
#include "plist.tcc"


#define IPLC_instantiate(a) IPL_instantiate(a); PL_instantiate(const a)


IPLC_instantiate(Score_elem);
IPLC_instantiate(Spanner);
IPLC_instantiate(Idealspacing);
IPLC_instantiate(PCol);

