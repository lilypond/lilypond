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

#ifndef MODIFIED_FONT_METRIC_HH
#define MODIFIED_FONT_METRIC_HH

#include "font-metric.hh"

#include <utility>

struct Preinit_Modified_font_metric
{
  Font_metric *orig_;
  Preinit_Modified_font_metric ();
};

/* Perhaps junk this, and move this to layout_def as interface? */
class Modified_font_metric : Preinit_Modified_font_metric, public Font_metric
{
public:
  Stencil text_stencil (Output_def *output_state, const std::string &, bool,
                        const std::string &) const override;
  Real get_magnification () const;

  static SCM make_scaled_font_metric (Font_metric *fm, Real magnification);
  size_t count () const override;
  Offset get_indexed_wxwy (size_t) const override;
  std::pair<Offset, bool> attachment_point (const std::string &,
                                            Direction) const override;
  size_t name_to_index (std::string) const override;
  size_t index_to_charcode (size_t) const override;
  Font_metric *original_font () const;

protected:
  Real magnification_;

  OVERRIDE_CLASS_NAME (Modified_font_metric);
  Modified_font_metric (Font_metric *fm, Real magnification);
  SCM sub_fonts () const override;
  std::string font_name () const override;
  Real design_size () const override;
  void derived_mark () const override;
  Box get_indexed_char_dimensions (size_t) const override;
};

#endif /* MODIFIED_FONT_METRIC_HH */
