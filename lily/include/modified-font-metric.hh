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

#ifndef MODIFIED_FONT_METRIC_HH
#define MODIFIED_FONT_METRIC_HH

#include "font-metric.hh"

/* Perhaps junk this, and move this to layout_def as interface? */
struct Modified_font_metric : public Font_metric
{
public:
  Box text_dimension (string) const;
  Box word_dimension (string) const;
  Stencil text_stencil (string, bool) const;

  static SCM make_scaled_font_metric (Font_metric *fm, Real magnification);
  size_t count () const;
  Offset get_indexed_wxwy (size_t) const;
  Offset attachment_point (string) const;
  size_t name_to_index (string) const;
  size_t index_to_charcode (size_t) const;
  Font_metric *original_font () const;

protected:
  Font_metric *orig_;
  Real magnification_;

  Modified_font_metric (Font_metric *fm, Real magnification);
  SCM sub_fonts () const;
  string font_name () const;
  Real design_size () const;
  void derived_mark () const;
  Box get_indexed_char (size_t) const;
  size_t index_to_ascii (size_t) const;
  Box get_ascii_char (size_t) const;
};

#endif /* MODIFIED_FONT_METRIC_HH */
