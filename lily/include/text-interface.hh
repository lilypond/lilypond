/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef TEXT_ITEM
#define TEXT_ITEM

#include "stencil.hh"
#include "grob-interface.hh"


class Text_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (interpret_markup, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (interpret_string, (SCM, SCM, SCM));
  DECLARE_GROB_INTERFACE();
  static bool is_markup (SCM);
  static bool is_markup_list (SCM);
};

#endif /* TEXT_ITEM */
