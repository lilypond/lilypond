/*
  pango-font.hh -- declare Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PANGO_FONT_HH
#define PANGO_FONT_HH

#include "config.hh"

#if HAVE_PANGO16

#include <pango/pango.h>
#include <pango/pangoft2.h>
#include "font-metric.hh"

#include "open-type-font.hh"

class Pango_font : public Font_metric
{
  PangoContext *context_;
  PangoFontDescription *pango_description_;
  PangoAttrList *attribute_list_;
  Real scale_;
  Real output_scale_;
  SCM physical_font_tab_;

public:
  SCM physical_font_tab () const;
  Pango_font (PangoFT2FontMap *,
	      PangoFontDescription *,
	      Real);
  ~Pango_font ();

  SCM font_file_name () const;
  void register_font_file (String, String);
  Stencil text_stencil (String) const;
  Stencil pango_item_string_stencil (const PangoItem *, String) const;

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

