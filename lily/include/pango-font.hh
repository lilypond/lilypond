/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PANGO_FONT_HH
#define PANGO_FONT_HH

#include "config.hh"

#if HAVE_PANGO_FT2

#include <pango/pango.h>
#include <pango/pangoft2.h>

#include "font-metric.hh"

class Pango_font : public Font_metric
{
  PangoContext *context_;
  PangoFontDescription *pango_description_;
  PangoAttrList *attribute_list_;
  Real scale_;
  Real output_scale_;
  SCM physical_font_tab_;
  Direction text_direction_;

public:
  SCM physical_font_tab () const;
  Pango_font (PangoFT2FontMap *,
	      PangoFontDescription const *,
	      Real);
  ~Pango_font ();

  string description_string () const; 
  SCM font_file_name () const;
  void register_font_file (string, string, int);
  Stencil text_stencil (string, bool, bool) const;

  Stencil pango_item_string_stencil (PangoItem const *, string, bool) const;

  virtual Stencil word_stencil (string, bool) const;
  virtual Stencil text_stencil (string, bool) const;
  virtual void derived_mark () const;
};

PangoFontDescription *
symbols_to_pango_font_description (SCM family,
				   SCM style,
				   SCM variant,
				   SCM weight,
				   SCM stretch);

Font_metric *
select_pango_font (Output_def *layout, SCM chain);

const int PANGO_RESOLUTION = 1200;
PangoFontDescription *properties_to_pango_description (SCM chain, Real text_size);

#endif /* HAVE_PANGO16 */
#endif /* PANGO_FONT_HH */

