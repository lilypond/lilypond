/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "font-interface.hh"

#include "all-font-metrics.hh"
#include "output-def.hh"
#include "warn.hh"
#include "grob.hh"

/* todo: split up this func, reuse in text_item?  */
Font_metric *
Font_interface::get_default_font (Grob *me)
{
  Font_metric *fm = unsmob<Font_metric> (get_property (me, "font"));
  if (!fm)
    {
      SCM chain = music_font_alist_chain (me);

      fm = select_font (me->layout (), chain);
      set_property (me, "font", fm->self_scm ());
    }

  return fm;
}

SCM
Font_interface::music_font_alist_chain (Grob *g)
{
  SCM defaults
    = g->layout ()->lookup_variable (ly_symbol2scm ("font-defaults"));
  if (SCM_UNBNDP (defaults))
    defaults = SCM_EOL;
  return g->get_property_alist_chain (defaults);
}

SCM
Font_interface::text_font_alist_chain (Grob *g)
{
  SCM defaults
    = g->layout ()->lookup_variable (ly_symbol2scm ("text-font-defaults"));
  if (SCM_UNBNDP (defaults))
    defaults = SCM_EOL;
  return g->get_property_alist_chain (defaults);
}

ADD_INTERFACE (Font_interface,
               R"(
Any symbol that is typeset through fixed sets of glyphs, (i.e., fonts).
               )",

               /* properties */
               R"(
font
font-encoding
font-family
font-name
font-series
font-shape
font-size
font-features
               )");
