/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "modified-font-metric.hh"
#include "pango-font.hh"
#include "warn.hh"
#include "stencil.hh"
#include "program-option.hh"

#include <cctype>
#include <tuple>
#include <utility>

using std::string;

Preinit_Modified_font_metric::Preinit_Modified_font_metric ()
{
  orig_ = 0;
}

Modified_font_metric::Modified_font_metric (Font_metric *fm, Real magnification)
{
  magnification_ = magnification;

  SCM desc = fm->description_;

  Real total_mag = magnification * from_scm<double> (scm_cdr (desc));
  assert (total_mag);

  description_ = scm_cons (scm_car (desc), to_scm (total_mag));
  orig_ = fm;
}

SCM
Modified_font_metric::make_scaled_font_metric (Font_metric *fm, Real scaling)
{
  Modified_font_metric *sfm = new Modified_font_metric (fm, scaling);
  return sfm->self_scm ();
}

Real
Modified_font_metric::design_size () const
{
  return orig_->design_size ();
}

Box
Modified_font_metric::get_indexed_char_dimensions (vsize i) const
{
  Box b = orig_->get_indexed_char_dimensions (i);
  b.scale (magnification_);
  return b;
}

Real
Modified_font_metric::get_magnification () const
{
  return magnification_;
}

vsize
Modified_font_metric::count () const
{
  return orig_->count ();
}

std::pair<Offset, bool>
Modified_font_metric::attachment_point (const string &s, Direction d) const
{
  Offset o;
  bool rotate;
  std::tie (o, rotate) = orig_->attachment_point (s, d);
  return std::make_pair (o * magnification_, rotate);
}

Offset
Modified_font_metric::get_indexed_wxwy (vsize k) const
{
  Offset o = orig_->get_indexed_wxwy (k);
  return o * magnification_;
}

size_t
Modified_font_metric::name_to_index (string s) const
{
  return orig_->name_to_index (s);
}

vsize
Modified_font_metric::index_to_charcode (vsize i) const
{
  return orig_->index_to_charcode (i);
}

void
Modified_font_metric::derived_mark () const
{
  if (orig_)
    scm_gc_mark (orig_->self_scm ());
}

Stencil
Modified_font_metric::text_stencil (Output_def *state, const string &text,
                                    bool feta, const string &features_str) const
{
  Box b;
  if (Pango_font *pf = dynamic_cast<Pango_font *> (orig_))
    {
      Stencil stc = pf->text_stencil (state, text, feta, features_str);

      Box b = stc.extent_box ();

      b.scale (magnification_);
      Stencil scaled (b, stc.expr ());
      return scaled;
    }

  return Font_metric::text_stencil (state, text, feta, features_str);
}

Font_metric *
Modified_font_metric::original_font () const
{
  return orig_;
}

SCM
Modified_font_metric::sub_fonts () const
{
  return orig_->sub_fonts ();
}

string
Modified_font_metric::font_name () const
{
  return original_font ()->font_name ();
}
