/*   
  all-fonts.hh -- declare All_font_metrics
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ALL_FONTS_HH
#define ALL_FONTS_HH

#include "dictionary.hh"
#include "file-path.hh"
#include "lily-proto.hh"
#include "font-metric.hh"
#include "scm-hash.hh"

/**
   Interface to all .afm files living in the filesystem.
 */
class All_font_metrics
{
  Scheme_hash_table afm_p_dict_;
  Scheme_hash_table tfm_p_dict_;
  Scheme_hash_table scaled_p_dict_;
  
  File_path search_path_;
public:
  
  Adobe_font_metric *find_afm (String name);
  Tex_font_metric *find_tfm (String);
  Font_metric *find_font (String name);  
  Scaled_font_metric* find_scaled (String , int);
  
  All_font_metrics (String search_path);
  
  SCM font_descriptions () const;
};

#endif /* ALL_FONTS_HH */

