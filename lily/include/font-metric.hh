/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <map>

typedef std::map<FT_UInt, FT_ULong> Index_to_charcode_map;

class Font_metric
{
  DECLARE_CLASSNAME (Font_metric);

public:
  SCM description_;
  string file_name_;

  // Return stencil for given string. output_state may be modified to
  // record the font.
  virtual Stencil text_stencil (Output_def *output_state,
                                const string &text, bool music) const;

  virtual string font_name () const;
  virtual size_t count () const;
  virtual Offset attachment_point (const string&) const;
  virtual Offset get_indexed_wxwy (size_t) const;
  virtual Box get_indexed_char_dimensions (size_t index) const;
  virtual size_t name_to_index (string) const=0;
  virtual size_t index_to_charcode (size_t) const;
  virtual Real design_size () const;
  virtual Stencil find_by_name (string) const;
  virtual SCM sub_fonts () const;
  virtual SCM font_file_name () const;
  DECLARE_SMOBS (Font_metric);

private:
  /* No copying, no implicit copy constructor.  */
  Font_metric (Font_metric const &);

protected:
  virtual void derived_mark () const;

  Font_metric ();
};

DECLARE_UNSMOB (Font_metric, metrics);

char *pfb2pfa (Byte const *pfb, int length);

#endif /* FONT_METRIC_HH */
