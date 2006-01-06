/*
  font-metric.hh -- declare Font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FONT_METRIC_HH
#define FONT_METRIC_HH

#include "box.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Font_metric
{
  DECLARE_CLASSNAME(Font_metric);

public:
  SCM description_;
  String file_name_;

  virtual Stencil text_stencil (String) const;
  virtual Box text_dimension (String) const;
  virtual String font_name () const;
  virtual int count () const;
  virtual Offset attachment_point (String) const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual Box get_indexed_char (int index) const;
  virtual Box get_ascii_char (int ascii) const;
  virtual int name_to_index (String) const;
  virtual unsigned index_to_charcode (int) const;
  virtual int index_to_ascii (int) const;
  virtual Real design_size () const;
  virtual Stencil find_by_name (String) const;
  virtual Stencil get_indexed_char_stencil (int k) const;
  virtual Stencil get_ascii_char_stencil (int k) const;
  virtual SCM sub_fonts () const;
  virtual SCM font_file_name () const;
  DECLARE_SMOBS (Font_metric,);

private:
  /* No copying, no implicit copy constructor.  */
  Font_metric (Font_metric const &);

protected:
  virtual void derived_mark () const;

  Font_metric ();
};

int get_encoded_index (Font_metric *m, String input_coding, int code);

class Simple_font_metric : public Font_metric
{
  DECLARE_CLASSNAME(Simple_font_metric);
public:
};

DECLARE_UNSMOB (Font_metric, metrics);

Box lookup_tex_text_dimension (Font_metric *font, SCM text);

#endif /* FONT_METRIC_HH */
