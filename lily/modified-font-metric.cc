/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include <cctype>
using namespace std;

#include "modified-font-metric.hh"
#include "pango-font.hh"
#include "warn.hh"
#include "stencil.hh"
#include "main.hh"
#include "program-option.hh"

Modified_font_metric::Modified_font_metric (Font_metric *fm,
					    Real magnification)
{
  magnification_ = magnification;

  SCM desc = fm->description_;

  Real total_mag = magnification * scm_to_double (scm_cdr (desc));
  assert (total_mag);

  description_ = scm_cons (scm_car (desc), scm_from_double (total_mag));
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
Modified_font_metric::get_indexed_char (vsize i) const
{
  Box b = orig_->get_indexed_char (i);
  b.scale (magnification_);
  return b;
}

Box
Modified_font_metric::get_ascii_char (vsize i) const
{
  Box b = orig_->get_ascii_char (i);
  b.scale (magnification_);
  return b;
}

vsize
Modified_font_metric::count () const
{
  return orig_->count ();
}

Offset
Modified_font_metric::attachment_point (string s) const
{
  Offset o = orig_->attachment_point (s);
  return o * magnification_;
}

Offset
Modified_font_metric::get_indexed_wxwy (vsize k) const
{
  Offset o = orig_->get_indexed_wxwy (k);
  return o * magnification_;
}

vsize
Modified_font_metric::name_to_index (string s) const
{
  return orig_->name_to_index (s);
}

vsize
Modified_font_metric::index_to_charcode (vsize i) const
{
  return orig_->index_to_charcode (i);
}

vsize
Modified_font_metric::index_to_ascii (vsize k) const
{
  return orig_->index_to_ascii (k);
}

void
Modified_font_metric::derived_mark () const
{
}

Stencil
Modified_font_metric::text_stencil (string text, bool feta) const
{
  Box b;
  if (Pango_font *pf = dynamic_cast<Pango_font *> (orig_))
    {
      Stencil stc = pf->text_stencil (text, feta);

      Box b = stc.extent_box ();

      b.scale (magnification_);
      Stencil scaled (b, stc.expr ());
      return scaled;
    }

  return Font_metric::text_stencil (text, feta);
}

Box
Modified_font_metric::text_dimension (string text) const
{
  Box b;
  Interval ydims;
  Real w = 0.0;

  for (ssize i = 0; i < text.length (); i++)
    {
      Box b = get_ascii_char ((unsigned char)text[i]);

      w += b[X_AXIS].length ();
      ydims.unite (b[Y_AXIS]);
    }
  if (ydims.is_empty ())
    ydims = Interval (0, 0);

  b = Box (Interval (0, w), ydims);
  return b;
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
