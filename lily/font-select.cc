/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>
                2023--2023 Jean Abou Samra <jean@abou-samra.fr>

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

#include "all-font-metrics.hh"
#include "dimensions.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "lily-guile.hh"
#include "ly-scm-list.hh"
#include "open-type-font.hh"
#include "output-def.hh"
#include "pango-font.hh"
#include "paper-def.hh"
#include "real.hh"

#include <cmath>  // std::pow
#include <string> // std::to_string

// The Emmentaler font and alternative music fonts come in several .otf files
// for different design sizes. feta-design-size-mapping is an alist mapping
// rounded sizes, namely suffixes in the font file names (like
// emmentaler-20.otf), to the actual, precise design sizes.  This function just
// chooses the closest to the requested size.
vsize
best_rounded_design_size (Real requested_size, Real &best_actual_size)
{
  SCM design_size_alist = Lily::feta_design_size_mapping;

  Real min_ratio = +infinity_f;
  vsize best_rounded_size = 0; // unimportant init value

  for (SCM rounded_size_actual_size_pair : as_ly_scm_list (design_size_alist))
    {
      assert (scm_is_pair (rounded_size_actual_size_pair));

      vsize rounded_size
        = from_scm<vsize> (scm_car (rounded_size_actual_size_pair));
      Real this_actual_size
        = from_scm<Real> (scm_cdr (rounded_size_actual_size_pair));

      Real this_ratio = (requested_size > this_actual_size
                           ? requested_size / this_actual_size
                           : this_actual_size / requested_size);

      if (this_ratio < min_ratio)
        {
          min_ratio = this_ratio;
          best_rounded_size = rounded_size;
          best_actual_size = this_actual_size;
        }
    }
  return best_rounded_size;
}

Font_metric *
select_font (Output_def *layout, SCM chain)
{
  SCM fonts = layout->lookup_variable (ly_symbol2scm ("fonts"));
  // If font-name is given, it is a Pango description string (only used for the
  // font family, shape, etc., but the size, if any, is disregarded).
  SCM string_desc
    = ly_chain_assoc_get (ly_symbol2scm ("font-name"), chain, SCM_BOOL_F);
  SCM encoding
    = ly_chain_assoc_get (ly_symbol2scm ("font-encoding"), chain, SCM_BOOL_F);
  if (scm_is_string (string_desc))
    {
      encoding = ly_symbol2scm ("latin1");
    }
  else if (!(scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
             || scm_is_eq (encoding, ly_symbol2scm ("fetaBraces"))
             || scm_is_eq (encoding, ly_symbol2scm ("fetaText"))
             || scm_is_eq (encoding, ly_symbol2scm ("latin1"))))
    {
      warning (
        _f ("font-encoding is invalid, should be 'fetaMusic, 'fetaBraces, "
            "'fetaText or 'latin1: %s",
            ly_scm_write_string (encoding).c_str ()));
      warning (_f ("falling back to latin1"));
      encoding = ly_symbol2scm ("latin1");
    }

  Real base_size;
  if (scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaBraces"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaText")))
    {
      base_size = from_scm<Real> (
                    layout->lookup_variable (ly_symbol2scm ("staff-height")))
                  / point_constant;
    }
  else // latin1
    {
      base_size = from_scm<Real> (
                    layout->lookup_variable (ly_symbol2scm ("text-font-size")))
                  * point_constant;
    }

  Real requested_step = from_scm<double> (
    ly_chain_assoc_get (ly_symbol2scm ("font-size"), chain, SCM_BOOL_F), 0.0);
  Real requested_size = base_size * pow (2.0, requested_step / 6.0);

  SCM family;
  if (scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaBraces"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaText")))
    {
      family
        = ly_chain_assoc_get (ly_symbol2scm ("font-family"), chain, SCM_BOOL_F);
      if (scm_is_false (family)
          // This is ugly and should be improved. It happens with things like
          // \markup \sans { più \dynamic p }
          || scm_is_eq (family, ly_symbol2scm ("roman"))
          || scm_is_eq (family, ly_symbol2scm ("sans"))
          || scm_is_eq (family, ly_symbol2scm ("typewriter")))
        {
          if (scm_is_eq (encoding, ly_symbol2scm ("fetaBraces")))
            family = ly_symbol2scm ("brace");
          else // fetaMusic, fetaText
            family = ly_symbol2scm ("music");
        }
    }
  else // latin1
    {
      family = ly_chain_assoc_get (ly_symbol2scm ("font-family"), chain,
                                   ly_symbol2scm ("roman"));
    }

  SCM name_scm = ly_assoc_get (family, fonts, SCM_BOOL_F);
  std::string name;
  if (scm_is_false (name_scm))
    {
      warning (_f ("no entry for font family %s in fonts alist",
                   ly_scm_write_string (family).c_str ()));
      name = "LilyPond Serif";
    }
  else if (!scm_is_string (name_scm))
    {
      warning (_f ("expected string for value in fonts alist, found: %s",
                   ly_scm_write_string (name_scm).c_str ()));
      name = "LilyPond Serif";
    }
  else
    {
      name = ly_scm2string (name_scm);
    }

  vsize rounded_size = 0;
  Real actual_size = 0; // dummy init value
  if (scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaBraces"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaText")))
    {
      rounded_size = best_rounded_design_size (requested_size,
                                               /* out-parameter */ actual_size);
      name += "-";
      if (scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
          || scm_is_eq (encoding, ly_symbol2scm ("fetaText")))
        {
          name += std::to_string (rounded_size);
        }
      else // fetaBraces
        {
          name += "brace";
        }
    }

  if (scm_is_eq (encoding, ly_symbol2scm ("fetaMusic"))
      || scm_is_eq (encoding, ly_symbol2scm ("fetaBraces")))
    {
      // This font has the design size of the OTF file.
      Font_metric *fm = all_fonts_global->find_otf_font (name);
      // This font is the previous one but scaled to get the exact size we want.
      return find_scaled_font (layout, fm, requested_size / actual_size);
    }
  else // latin1, fetaText
    {
      // Unlike music fonts, text fonts are scaled automatically by Pango.
      PangoFontDescription *description;
      if (scm_is_string (string_desc))
        {
          description = pango_font_description_from_string (
            ly_scm2string (string_desc).c_str ());
        }
      else
        {
          description = pango_font_description_new ();
          pango_font_description_set_family (description, name.c_str ());
          // font-shape, etc. make no sense in fetaText
          if (scm_is_eq (encoding, ly_symbol2scm ("latin1")))
            tweak_pango_description (description, chain);
        }
      if (scm_is_eq (encoding, ly_symbol2scm ("fetaText")))
        {
          requested_size *= point_constant;
        }
      int pango_size = static_cast<int> (
        std::lround (static_cast<Real> (requested_size) * PANGO_SCALE));
      pango_font_description_set_size (description, pango_size);
      return find_pango_font (layout, description);
    }
}
