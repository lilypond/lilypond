/*
  template2.cc -- instantiate some list templates. 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "symbol.hh"
#include "voice.hh"
#include "voice-element.hh"
#include "musicalrequest.hh"
#include "staff.hh"
#include "score-column.hh"
#include "staff-column.hh"
#include "spanner.hh"
#include "plist.tcc"
#include "pcursor.tcc"

IPL_instantiate(Request);
IPL_instantiate(Score_column);
IPL_instantiate(Staff_column);
IPL_instantiate(Staff);
IPL_instantiate(Voice_element);
IPL_instantiate(Voice);


