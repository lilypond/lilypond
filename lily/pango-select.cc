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
    = ly_chain_assoc_get (ly_symbol2scm ("font-variant"), chain, SCM_BOOL_F);
  PangoVariant pvariant = PANGO_VARIANT_NORMAL;
  if (scm_is_eq (variant, ly_symbol2scm ("small-caps")))
    pvariant = PANGO_VARIANT_SMALL_CAPS;
  // TODO: add the extra variants from Pango 1.50 when older
  // versions are no longer supported.

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
  if (scm_is_eq (weight, ly_symbol2scm ("thin")))
    pw = PANGO_WEIGHT_THIN;
  else if (scm_is_eq (weight, ly_symbol2scm ("ultralight")))
    pw = PANGO_WEIGHT_ULTRALIGHT;
  else if (scm_is_eq (weight, ly_symbol2scm ("light")))
    pw = PANGO_WEIGHT_LIGHT;
  else if (scm_is_eq (weight, ly_symbol2scm ("semilight")))
    pw = PANGO_WEIGHT_SEMILIGHT;
  else if (scm_is_eq (weight, ly_symbol2scm ("book")))
    pw = PANGO_WEIGHT_BOOK;
  else if (scm_is_eq (weight, ly_symbol2scm ("medium")))
    pw = PANGO_WEIGHT_MEDIUM;
  else if (scm_is_eq (weight, ly_symbol2scm ("semibold")))
    pw = PANGO_WEIGHT_SEMIBOLD;
  else if (scm_is_eq (weight, ly_symbol2scm ("bold")))
    pw = PANGO_WEIGHT_BOLD;
  else if (scm_is_eq (weight, ly_symbol2scm ("ultrabold")))
    pw = PANGO_WEIGHT_ULTRABOLD;
  else if (scm_is_eq (weight, ly_symbol2scm ("heavy")))
    pw = PANGO_WEIGHT_HEAVY;
  else if (scm_is_eq (weight, ly_symbol2scm ("ultraheavy")))
    pw = PANGO_WEIGHT_ULTRAHEAVY;
  pango_font_description_set_weight (description, pw);

  SCM stretch
    = ly_chain_assoc_get (ly_symbol2scm ("font-stretch"), chain, SCM_BOOL_F);
  PangoStretch pstretch = PANGO_STRETCH_NORMAL;
  if (scm_is_eq (stretch, ly_symbol2scm ("ultra-condensed")))
    pstretch = PANGO_STRETCH_ULTRA_CONDENSED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("extra-condensed")))
    pstretch = PANGO_STRETCH_EXTRA_CONDENSED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("condensed")))
    pstretch = PANGO_STRETCH_CONDENSED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("semi-condensed")))
    pstretch = PANGO_STRETCH_SEMI_CONDENSED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("semi-expanded")))
    pstretch = PANGO_STRETCH_SEMI_EXPANDED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("expanded")))
    pstretch = PANGO_STRETCH_EXPANDED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("extra-expanded")))
    pstretch = PANGO_STRETCH_EXTRA_EXPANDED;
  else if (scm_is_eq (stretch, ly_symbol2scm ("ultra-expanded")))
    pstretch = PANGO_STRETCH_ULTRA_EXPANDED;
  pango_font_description_set_stretch (description, pstretch);
}
