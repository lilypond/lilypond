/*
  paper-def.hh -- declare Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  VIRTUAL_COPY_CONS (Music_output_def);


public:    
  SCM font_descriptions ()const;
  virtual ~Paper_def ();
  static int score_count_i_;
  
  /*
    JUNKME
   */
  Real get_realvar (SCM symbol) const;
  Real get_var (String id) const;
  SCM get_scmvar (String id)const;
  SCM get_scmvar_scm  (SCM sym) const;
  void reinit ();
  Paper_def ();
  Paper_def (Paper_def const&);

  Interval line_dimensions_int (int) const;

  virtual int get_next_score_count () const;
  static void reset_score_count ();
  void output_settings (Paper_outputter*) const;
  Paper_outputter* paper_outputter_p () ;

  Font_metric * find_font (SCM name, Real mag);
  
  // urg
  friend int yyparse (void*);
};

#endif // Paper_def_HH
