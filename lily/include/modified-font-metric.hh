/*   
  modified-font-metric.hh -- declare Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MODIFIED_FONT_METRIC_HH
#define MODIFIED_FONT_METRIC_HH

#include "font-metric.hh"

/* Perhaps junk this, and move this to layout_def as interface? */
struct Modified_font_metric : public Font_metric
{
public:
  Box text_dimension (String);
  
  static SCM make_scaled_font_metric (Font_metric *fm, Real magnification,
				      SCM font_encoding, SCM input_encoding);
  virtual int count () const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual Offset attachment_point (String) const;
  virtual int name_to_index (String) const;
  virtual unsigned index_to_charcode (int) const;
  virtual String coding_scheme () const;
  virtual Font_metric *original_font () const;  
  
protected:
  SCM coding_vector_;
  SCM coding_table_;
  SCM coding_mapping_;
  SCM coding_description_;
  Font_metric *orig_;
  Real magnification_;
  String input_encoding_;
  friend SCM ly_font_encoding_alist (SCM);
    
  Modified_font_metric (Font_metric *fm, Real magnification,
			String font_encoding, String input_encoding);
  virtual SCM sub_fonts () const;
  virtual Real design_size () const;
  virtual void derived_mark () const; 
  virtual Box get_indexed_char (int) const;
  virtual int index_to_ascii (int) const;
  virtual Box get_ascii_char (int) const;
  Box tex_kludge (String) const;
};

#endif /* MODIFIED_FONT_METRIC_HH */
