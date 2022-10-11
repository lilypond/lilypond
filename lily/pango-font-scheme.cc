/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lookup.hh"
#include "dimensions.hh"
#include "pango-font.hh"
#include "warn.hh"

#include "stencil.hh"

#include <pango/pangoft2.h>

LY_DEFINE (ly_pango_font_p, "ly:pango-font?", 1, 0, 0, (SCM f),
           R"(
Is @var{f} a Pango font?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Pango_font> (f)));
}

LY_DEFINE (ly_pango_font_physical_fonts, "ly:pango-font-physical-fonts", 1, 0,
           0, (SCM f),
           R"(
Return alist of @code{(ps-name file-name font-index)} lists for Pango
font@tie{}@var{f}.
           )")
{
  Pango_font *pf = unsmob<Pango_font> (f);

  SCM alist = SCM_EOL;
  if (pf)
    alist = ly_hash2alist (pf->physical_font_tab ());

  return alist;
}
