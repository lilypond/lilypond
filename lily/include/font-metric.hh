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

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "virtual-methods.hh"
#include "freetype.hh"

#include <map>
using namespace std;

typedef map<FT_UInt, FT_ULong> Index_to_charcode_map;

class Font_metric
{
  DECLARE_CLASSNAME(Font_metric);

public:
  SCM description_;
  string file_name_;

  virtual Stencil text_stencil (string, bool) const;
  virtual Stencil word_stencil (string, bool) const;

  // ugh.
  virtual Box text_dimension (string) const;

  virtual string font_name () const;
  virtual size_t count () const;
  virtual Offset attachment_point (string) const;
  virtual Offset get_indexed_wxwy (size_t) const;
  virtual Box get_indexed_char (size_t index) const;
  virtual Box get_ascii_char (size_t ascii) const;

  /*
    WTF are these vsize ?

    Font_metric is not related to vector<> 
   */
  virtual size_t name_to_index (string) const;
  virtual size_t index_to_charcode (size_t) const;
  virtual size_t index_to_ascii (size_t) const;
  virtual Real design_size () const;
  virtual Stencil find_by_name (string) const;
  virtual Stencil get_indexed_char_stencil (size_t k) const;
  virtual Stencil get_ascii_char_stencil (size_t k) const;
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

int get_encoded_index (Font_metric *m, string input_coding, int code);

class Simple_font_metric : public Font_metric
{
  DECLARE_CLASSNAME(Simple_font_metric);
public:
};

DECLARE_UNSMOB (Font_metric, metrics);

char *pfb2pfa (Byte const *pfb, int length);

#endif /* FONT_METRIC_HH */
