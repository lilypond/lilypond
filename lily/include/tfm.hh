/*   
  tfm.hh -- declare Tex_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>


  revamped code from GNU Fontutils-0.6

 */

#ifndef TFM_HH
#define TFM_HH

#include "string.hh"
#include "array.hh"
#include "lily-proto.hh"
#include "font-metric.hh"

/* The type.  */
typedef long Fix;

/* A character code.  Perhaps someday we will allow for 16-bit
   character codes, but for now we are restricted to 256 characters per
   font (like TeX and PostScript).  */
typedef unsigned char Char_code;

/* Used in file formats.  */
typedef int Byte_count;

/* The restriction to 256 characters in a TFM file is part of the file
   format, so this number should only be changed in the (very unlikely)
   event that the file format changes.  */
#define TFM_SIZE 256

/* Fontwide information.  All real values are in printer's points:
   72.27 points = 1 inch.  */

/* TFM_MIN_DESIGNSIZE <= designsize < TFM_MAX_DESIGNSIZE.  */
#define TFM_MIN_DESIGNSIZE 1.0
#define TFM_MAX_DESIGNSIZE 2048

/* The maximum number of global font parameters we allow.  */
#define TFM_MAX_FONTDIMENS 30

/* The maximum length of a codingscheme string.  */
#define TFM_MAX_CODINGSCHEME_LENGTH 39

/* Define symbolic names for the numbers of the parameters we
   recognize.  Some numbers have more than one name.  */
#define TFM_SLANT_PARAMETER 1
#define TFM_SPACE_PARAMETER 2
#define TFM_STRETCH_PARAMETER 3
#define TFM_SHRINK_PARAMETER 4
#define TFM_XHEIGHT_PARAMETER 5
#define TFM_QUAD_PARAMETER 6
#define TFM_EXTRASPACE_PARAMETER 7
#define TFM_NUM1_PARAMETER 8
#define TFM_NUM2_PARAMETER 9
#define TFM_NUM3_PARAMETER 10
#define TFM_DENOM1_PARAMETER 11
#define TFM_DENOM2_PARAMETER 12
#define TFM_SUP1_PARAMETER 13
#define TFM_SUP2_PARAMETER 14
#define TFM_SUP3_PARAMETER 15
#define TFM_SUB1_PARAMETER 16
#define TFM_SUB2_PARAMETER 17
#define TFM_SUPDROP_PARAMETER 18
#define TFM_SUBDROP_PARAMETER 19
#define TFM_DELIM1_PARAMETER 20
#define TFM_DELIM2_PARAMETER 21
#define TFM_AXISHEIGHT_PARAMETER 22
#define TFM_DEFAULTRULETHICKNESS_PARAMETER 8
#define TFM_BIGOPSPACING1_PARAMETER 9
#define TFM_BIGOPSPACING2_PARAMETER 10
#define TFM_BIGOPSPACING3_PARAMETER 11
#define TFM_BIGOPSPACING4_PARAMETER 12
#define TFM_BIGOPSPACING5_PARAMETER 13

/* These are not in any of the standard TeX fonts, but the information
   is useful nevertheless.  */
#define TFM_LEADINGHEIGHT_PARAMETER 23
#define TFM_LEADINGDEPTH_PARAMETER 24
#define TFM_FONTSIZE_PARAMETER 25
#define TFM_VERSION_PARAMETER 26

struct Tfm_header
{
  Byte_count char_info_pos;
  Byte_count width_pos;
  Byte_count height_pos;
  Byte_count depth_pos;
  Byte_count italic_correction_pos;
  Byte_count lig_kern_pos;
  Byte_count kern_pos;
  unsigned param_word_count;
};

struct Tfm_info
{
  Char_code first_charcode, last_charcode;
  U32 checksum;
  Real design_size;
  String coding_scheme;
  unsigned parameter_count;
  // Real parameters [Tex_font_metric::MAX_FONTDIMENS];
  Real parameters [TFM_MAX_FONTDIMENS];
};

/* When typesetting, the current character + `character' leads to
   `ligature'.  The TFM format was extended in 1990 to allow for more
   complicated ligatures than this, but we do not make those
   distinctions.  */
struct Tfm_ligature
{
  Char_code character;
  Char_code ligature;
};

/* Similarly for kerns.  */
struct Tfm_kern
{
  Char_code character;
  Real kern;
};

struct Tex_font_char_metric : Character_metric
{
  bool exists_b_;
  Char_code code_;
  Real width_, height_, depth_, italic_correction_;
  Fix width_fix_, height_fix_, depth_fix_, italic_correction_fix_;
  Array<Tfm_kern> kern_arr_;
  Array<Tfm_ligature> ligature_arr_;

  String str () const;
  Tex_font_char_metric ();

  Box dimensions () const; 
};


class Tex_font_metric : public Font_metric
{
public:
  Tex_font_metric ();

  void clear (int n);
  Character_metric const *get_char (int, bool) const;
  Tex_font_char_metric const &find_ascii (int ascii, bool warn=true) const;

  String str () const;

  Tfm_info info_;
  Tfm_header header_;
  Array<Tex_font_char_metric> char_metrics_;
  Array<int> ascii_to_metric_idx_;
};


#endif /* TFM_HH */

