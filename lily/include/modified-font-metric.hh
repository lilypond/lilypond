/*   
  modified-font-metric.hh -- declare Font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MODIFIED_FONT_METRIC_HH
#define MODIFIED_FONT_METRIC_HH

#include "font-metric.hh"

/* Perhaps junk this, and move this to paper_def as interface? */
struct Modified_font_metric : public Font_metric
{
public:
  Box text_dimension (String);
  
  
  static SCM make_scaled_font_metric (SCM, Font_metric*, Real);
  virtual int count () const;
  virtual Offset get_indexed_wxwy (int) const;
  virtual int name_to_index (String) const;
  virtual String coding_scheme () const;

  virtual Font_metric * original_font () const;  

protected:
  
  SCM coding_vector_;
  SCM coding_table_;
  SCM coding_mapping_;
  SCM coding_description_;
  friend SCM ly_font_encoding_alist (SCM);
    
  virtual Real design_size () const;
  virtual void derived_mark () const; 
  virtual Box get_indexed_char (int) const;
  virtual int index_to_ascii (int) const;
  virtual Box get_ascii_char (int) const;
  Font_metric *orig_;
  Real magnification_;
  String coding_scheme_;

  
  Modified_font_metric (String, Font_metric*, Real);
  Box tex_kludge (String) const;
};

#endif /* MODIFIED_FONT_METRIC_HH */
