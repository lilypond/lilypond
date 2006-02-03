/*
  tie-column-format.cc -- implement formatting routines for Tie_column

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "stem.hh"
#include "note-head.hh"
#include "tie.hh"
#include "std-vector.hh"
#include "spanner.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "rhythmic-head.hh"
#include "tie-formatting-problem.hh"
#include "tie-configuration.hh"

#include <set>

