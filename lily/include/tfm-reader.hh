/*   
  tfm-reader.hh -- declare Tex_font_metric_reader
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>


  revamped code from GNU Fontutils-0.6

 */

#ifndef TFM_READER_HH
#define TFM_READER_HH

#include "tfm.hh"
#include "binary-source-file.hh"

class Tex_font_metric_reader
{
public:
  Tex_font_metric_reader (String name);
  Tex_font_metric read_tfm ();
  
private:
  Real get_U32_fix_f ();
  Real get_U32_fix_scaled_f ();
  String get_bcpl_str ();
  void read_header ();
  void read_params ();
  void read_char_metrics ();
  Tex_font_char_metric read_char_metric (Char_code code);
  Tex_font_char_metric read_char ();
  void read_lig_kern_program (Array<Tfm_ligature>* ligature_arr_p, Array <Tfm_kern>* kern_arr_p);

  Tex_font_metric tfm_;
  Binary_source_file input_;
};


#endif /* TFM_READER_HH */

