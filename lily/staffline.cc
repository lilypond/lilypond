/*
  staffline.cc -- implement Line_of_staff

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staffline.hh"
#include "scoreline.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "p-score.hh"



IMPLEMENT_IS_TYPE_B2(Line_of_staff,Spanner,Horizontal_vertical_group);
