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

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
#include "freetype.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

#include <limits>
#include <map>
#include <utility>

static const size_t GLYPH_INDEX_INVALID (std::numeric_limits<size_t>::max ());

typedef std::map<FT_UInt, FT_ULong> Index_to_charcode_map;

class Font_metric : public Smob<Font_metric>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Font_metric ();

private:
  VIRTUAL_CLASS_NAME (Font_metric);

public:
  SCM description_;

  // Return stencil for given std::string. output_state may be modified to
  // record the font.
  virtual Stencil text_stencil (Output_def *output_state,
                                const std::string &text, bool music,
                                const std::string &features_str) const;

  virtual std::string font_name () const;
  virtual size_t count () const;
  virtual std::pair<Offset, bool> attachment_point (const std::string &,
                                                    Direction) const;
  virtual Offset get_indexed_wxwy (size_t) const;
  virtual Box get_indexed_char_dimensions (size_t index) const;
  virtual size_t name_to_index (std::string) const = 0;
  virtual size_t index_to_charcode (size_t) const;
  virtual Real design_size () const;
  virtual Stencil find_by_name (std::string) const;
  virtual SCM sub_fonts () const;
  virtual SCM font_file_name () const;

  Real magnification () const;

private:
  /* No copying, no implicit copy constructor.  */
  Font_metric (Font_metric const &);

protected:
  virtual void derived_mark () const;

  Font_metric ();
};

std::string pfb2pfa (const std::string &pfb);

#endif /* FONT_METRIC_HH */
