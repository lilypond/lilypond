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

#include "font-metric.hh"

#include "dimensions.hh"
#include "modified-font-metric.hh"
#include "open-type-font.hh"
#include "stencil.hh"
#include "warn.hh"

#include <cctype>
#include <cmath>
#include <utility>

using std::string;

Real
Font_metric::design_size () const
{
  return 1.0 * point_constant;
}

Stencil
Font_metric::find_by_name (string s) const
{
  replace_all (&s, '-', 'M');
  size_t idx = name_to_index (s);
  Box b;

  SCM expr = SCM_EOL;
  if (idx != GLYPH_INDEX_INVALID)
    {
      expr = ly_list (ly_symbol2scm ("named-glyph"), self_scm (),
                      ly_string2scm (s));
      b = get_indexed_char_dimensions (idx);
    }

  Stencil q (b, expr);
  return q;
}

Font_metric::Font_metric ()
{
  description_ = SCM_EOL;
  smobify_self ();
}

Font_metric::Font_metric (Font_metric const &)
  : Smob<Font_metric> ()
{
}

Font_metric::~Font_metric ()
{
}

size_t
Font_metric::count () const
{
  return 0;
}

Box
Font_metric::get_indexed_char_dimensions (size_t) const
{
  return Box (Interval (0, 0), Interval (0, 0));
}

Offset
Font_metric::get_indexed_wxwy (size_t) const
{
  return Offset (0, 0);
}

void
Font_metric::derived_mark () const
{
}

SCM
Font_metric::mark_smob () const
{
  derived_mark ();
  return description_;
}

int
Font_metric::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<", port);
  scm_puts (class_name (), port);
  scm_puts (" ", port);
  scm_write (description_, port);
  scm_puts (">", port);
  return 1;
}

const char *const Font_metric::type_p_name_ = "ly:font-metric?";

SCM
Font_metric::font_file_name () const
{
  return scm_car (description_);
}

string
Font_metric::font_name () const
{
  string s ("unknown");
  return s;
}

size_t
Font_metric::index_to_charcode (size_t i) const
{
  return i;
}

std::pair<Offset, bool>
Font_metric::attachment_point (const string &, Direction) const
{
  return std::make_pair (Offset (0, 0), false);
}

SCM
Font_metric::sub_fonts () const
{
  return SCM_EOL;
}

Stencil
Font_metric::text_stencil (Output_def *state, const string &, bool,
                           const string &) const
{
  (void) state;

  programming_error ("Cannot get a text stencil from this font");
  return Stencil (Box (), SCM_EOL);
}

Real
Font_metric::magnification () const
{
  return from_scm<Real> (scm_cdr (description_));
}
