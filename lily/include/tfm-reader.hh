/*
  tfm-reader.hh -- declare Tex_font_metric_reader

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Jan Nieuwenhuizen <janneke@gnu.org>


  revamped code from GNU Fontutils-0.6
*/

#ifndef TFM_READER_HH
#define TFM_READER_HH

#include "tfm.hh"
#include "binary-source-file.hh"

class Tex_font_metric_reader
{
private:
  Real get_U32_fix ();
  Real get_U32_fix_scaled ();
  std::string get_bcpl_string ();
  void read_header ();
  void read_params ();
  void read_char_metrics ();
  Tex_font_char_metric read_char_metric (Char_code code);
  Tex_font_char_metric read_char ();
  void read_lig_kern_program (std::vector<Tfm_ligature> *ligatures,
			      std::vector<Tfm_kern> *kerns);

  Binary_source_file input_;

public:
  Tex_font_metric_reader (std::string name);

  Tfm_info info_;
  Tfm_header header_;
  std::vector<Tex_font_char_metric> char_metrics_;
  std::vector<vsize> ascii_to_metric_idx_;
};

#endif /* TFM_READER_HH */

