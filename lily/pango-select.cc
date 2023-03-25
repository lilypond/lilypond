/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-guile.hh"

#include <pango/pango.h>

// Take a Pango font description, read some properties and use them to tweak
// the font search parameters (style, weight, etc.).
void
tweak_pango_description (PangoFontDescription *description, SCM chain)
{
  SCM variant
    = ly_chain_assoc_get (ly_symbol2scm ("font-shape"), chain, SCM_BOOL_F);
  PangoVariant pvariant = PANGO_VARIANT_NORMAL;
  if (scm_is_eq (variant, ly_symbol2scm ("caps")))
    pvariant = PANGO_VARIANT_SMALL_CAPS;
  pango_font_description_set_variant (description, pvariant);

  SCM style
    = ly_chain_assoc_get (ly_symbol2scm ("font-shape"), chain, SCM_BOOL_F);

  PangoStyle pstyle = PANGO_STYLE_NORMAL;
  if (scm_is_eq (style, ly_symbol2scm ("italic")))
    pstyle = PANGO_STYLE_ITALIC;
  else if (scm_is_eq (style, ly_symbol2scm ("oblique"))
           || scm_is_eq (style, ly_symbol2scm ("slanted")))
    pstyle = PANGO_STYLE_OBLIQUE;
  pango_font_description_set_style (description, pstyle);

  SCM weight
    = ly_chain_assoc_get (ly_symbol2scm ("font-series"), chain, SCM_BOOL_F);
  PangoWeight pw = PANGO_WEIGHT_NORMAL;
  if (scm_is_eq (weight, ly_symbol2scm ("bold")))
    pw = PANGO_WEIGHT_BOLD;
  if (scm_is_eq (weight, ly_symbol2scm ("heavy")))
    pw = PANGO_WEIGHT_HEAVY;
  if (scm_is_eq (weight, ly_symbol2scm ("ultrabold")))
    pw = PANGO_WEIGHT_ULTRABOLD;
  if (scm_is_eq (weight, ly_symbol2scm ("light")))
    pw = PANGO_WEIGHT_LIGHT;
  if (scm_is_eq (weight, ly_symbol2scm ("ultralight")))
    pw = PANGO_WEIGHT_ULTRALIGHT;
  pango_font_description_set_weight (description, pw);

  PangoStretch ps = PANGO_STRETCH_NORMAL; // TODO: configurability
  pango_font_description_set_stretch (description, ps);
}
