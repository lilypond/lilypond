/*
  font-metric.hh -- declare Font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  virtual Stencil text_stencil (string) const;
  virtual Stencil word_stencil (string) const;

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
