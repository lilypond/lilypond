/*
  paper-def.hh -- declare Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PAPER_DEF_HH
#define PAPER_DEF_HH


#include "lily-proto.hh"
#include "lily-guile.hh"
#include "real.hh"
#include "array.hh"
#include "interval.hh"
#include "music-output-def.hh"


/** 

  Symbols, dimensions and constants pertaining to visual output.

  This struct takes care of all kinds of symbols, dimensions and
  constants. Most of them are related to the point-size of the fonts,
  so therefore, the lookup table for symbols is also in here.

  TODO: 
  
  add support for multiple fontsizes 

  remove all utility funcs 
  

  add support for other len->wid conversions.


  Interesting variables:
  
  /// The distance between lines
  interline
  
*/
class Paper_def : public Music_output_def 
{
protected:
  VIRTUAL_COPY_CONSTRUCTOR (Music_output_def, Paper_def);
  
public:
  Book_paper_def * bookpaper_;
  static int score_count_;
  
  Paper_def ();
  Paper_def (Paper_def const&);
  virtual ~Paper_def ();
  
  Paper_outputter* get_paper_outputter (String) const;
  SCM font_descriptions () const;
  void reinit ();
  Interval line_dimensions_int (int) const;
  void output_settings (Paper_outputter*) const;
  Font_metric *find_scaled_font (Font_metric *fm, Real mag, SCM enc_name);
  Real get_dimension (SCM symbol) const;
  
  friend int yyparse (void*);
};

Paper_def * unsmob_paper (SCM x);
Font_metric *select_encoded_font (Paper_def *paper, SCM input_encoding, SCM chain);
Font_metric *select_font (Paper_def *paper, SCM chain);

#endif /* PAPER_DEF_HH */
