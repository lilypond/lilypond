/*
  idealspacing.cc -- implement Idealspacing

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "idealspacing.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"


Idealspacing::Idealspacing()
{
  space_f_ = 0.0;
  hooke_f_ = 0.0;
  cols_drul_[LEFT] = cols_drul_[RIGHT] = -1;
}
