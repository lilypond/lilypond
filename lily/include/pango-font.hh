/*
  pango-font.hh -- declare Pango_font

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PANGO_FONT_HH
#define PANGO_FONT_HH

#include "config.hh"

#ifdef HAVE_PANGO16

#include <pango/pango.h>
#include <pango/pangoft2.h>
#include "font-metric.hh"

#include "open-type-font.hh"

class Pango_font : public Font_metric
{
  /** handle to face object */
  FT_Face face_;
  PangoContext *context_;
  PangoAttrList *attribute_list_;
  Real scale_;
  SCM subfonts_;
  Index_to_charcode_map index_to_charcode_map_;

public:
  Pango_font (PangoFT2FontMap *,
	      int dpi, 
	      Direction leftright,
	      PangoFontDescription *);
  ~Pango_font ();

  SCM font_file_name () const;
  void register_font_file (String, String);
  Stencil text_stencil (String) const;

  virtual SCM sub_fonts () const;
  virtual void derived_mark () const;
  virtual int name_to_index (String) const;
  virtual unsigned index_to_charcode (int) const;
};

PangoFontDescription *
symbols_to_pango_font_description (SCM family,
				   SCM style,
				   SCM variant,
				   SCM weight,
				   SCM stretch,
				   Real size);

Font_metric *
select_pango_font (Output_def *layout, SCM chain);

#endif /* HAVE_PANGO16 */

#endif /* PANGO_FONT_HH */

