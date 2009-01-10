/*
  modified-font-metric.hh -- declare Font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  Stencil text_stencil (string) const;

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
