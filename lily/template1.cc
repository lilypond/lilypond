/*
  template1.cc -- instantiate some List classes

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "idealspacing.hh"
#include "plist.hh"
#include "p-col.hh"
#include "item.hh"
#include "musical-request.hh"
#include "spanner.hh"
#include "cursor.tcc"
#include "pcursor.tcc"
#include "plist.tcc"


#define IPLC_INSTANTIATE(a) POINTERLIST_INSTANTIATE(a)

IPLC_INSTANTIATE(Score_element);
IPLC_INSTANTIATE(Spanner);
IPLC_INSTANTIATE(Idealspacing);
