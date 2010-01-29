/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef ACCIDENTAL_INTERFACE_HH
#define ACCIDENTAL_INTERFACE_HH

#include "std-vector.hh"

#include "box.hh"
#include "lily-proto.hh"
#include "grob-interface.hh"

class Accidental_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  
  DECLARE_GROB_INTERFACE();
  static string get_fontcharname (string style, int alteration);
  static vector<Box> accurate_boxes (Grob *me, Grob **common);
  static SCM get_stencil (Grob *me);
};

#endif
