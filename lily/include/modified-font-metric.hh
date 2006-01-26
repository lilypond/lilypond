/*
  modified-font-metric.hh -- declare Font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MODIFIED_FONT_METRIC_HH
#define MODIFIED_FONT_METRIC_HH

#include "font-metric.hh"

/* Perhaps junk this, and move this to layout_def as interface? */
struct Modified_font_metric : public Font_metric
{
public:
  virtual Box text_dimension (std::string) const;
  virtual Stencil text_stencil (std::string) const;

  static SCM make_scaled_font_metric (Font_metric *fm, Real magnification);
  virtual int count () const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual Offset attachment_point (std::string) const;
  virtual int name_to_index (std::string) const;
  virtual unsigned index_to_charcode (int) const;
  Font_metric *original_font () const;

protected:
  Font_metric *orig_;
  Real magnification_;

  Modified_font_metric (Font_metric *fm, Real magnification);
  virtual SCM sub_fonts () const;
  virtual std::string font_name () const;
  virtual Real design_size () const;
  virtual void derived_mark () const;
  virtual Box get_indexed_char (int) const;
  virtual int index_to_ascii (int) const;
  virtual Box get_ascii_char (int) const;
  Box tex_kludge (std::string) const;
};

#endif /* MODIFIED_FONT_METRIC_HH */
