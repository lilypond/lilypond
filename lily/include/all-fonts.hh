/*   
  all-fonts.hh -- declare All_font_metrics
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ALL_FONTS_HH
#define ALL_FONTS_HH

#include "dictionary.hh"
#include "file-path.hh"
#include "lily-proto.hh"
#include "font-metric.hh"

/**
   Interface to all .afm files living in the filesystem.
 */
class All_font_metrics
{
  Dictionary<Adobe_font_metric*> afm_p_dict_;
  Dictionary<Tex_font_metric*> tfm_p_dict_;  
  File_path search_path_;
public:
  
  Adobe_font_metric *find_afm (String name);
  Tex_font_metric *find_tfm (String);
  Font_metric *find_font (String name);  
  All_font_metrics (String search_path);
};




#endif /* ALL_FONTS_HH */

