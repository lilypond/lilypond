/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2023 Juergen Reuter <reuter@ipd.uka.de>,
  Pal Benko <benkop@freestart.hu>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MENSURAL_LIGATURE_HH
#define MENSURAL_LIGATURE_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

// These are all possible mensural ligature primitives.
// FIXME: this should use an enum

// no output
#define MLP_NONE 0x00
// upward left stem
#define MLP_UP 0x01
// downward left stem
#define MLP_DOWN 0x02
// upward right stem (in the middle a left stem of the next note)
#define MLP_JOIN_UP 0x04
// downward right stem
#define MLP_JOIN_DOWN 0x08
// mensural brevis head
#define MLP_BREVIS 0x10
// mensural brevis head with right cauda
#define MLP_LONGA 0x20
// mensural maxima head without stem
#define MLP_MAXIMA 0x40
// start of obliqua
#define MLP_FLEXA_BEGIN 0x80
// end of obliqua
#define MLP_FLEXA_END 0x100

#define MLP_STEM (MLP_UP | MLP_DOWN)
#define MLP_RIGHT_STEM (MLP_JOIN_UP | MLP_JOIN_DOWN)
#define MLP_SINGLE_HEAD (MLP_BREVIS | MLP_LONGA | MLP_MAXIMA)
#define MLP_FLEXA (MLP_FLEXA_BEGIN | MLP_FLEXA_END)
#define MLP_ANY (MLP_FLEXA | MLP_SINGLE_HEAD)

struct Mensural_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif /* MENSURAL_LIGATURE_HH */
