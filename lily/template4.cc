/*
  template4.cc -- instantiate Link_list baseclass.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef __CYGWIN32__
#include "proto.hh"
#include "pcursor.hh"
#include "plist.hh"
#include "spanner.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "cursor.tcc"
#include "list.tcc"
#include "pcursor.tcc"
#include "plist.tcc"

POINTERLIST_INSTANTIATE(Audio_item);
POINTERLIST_INSTANTIATE(Audio_staff);
POINTERLIST_INSTANTIATE(Paper_column);
POINTERLIST_INSTANTIATE(Paper_score);
#endif
