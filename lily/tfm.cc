/*   
  tfm.cc --  implement Tex_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  

  some code shamelessly copied from GNU fontutils-0.6/tfm/tfm_input.c
 */

#include "tfm.hh"
#include "tfm-reader.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "warn.hh"

Box
Tex_font_char_metric::dimensions () const
{
  Real d = -depth_;
  return Box (Interval (0, width_),Interval ( d <? height_, d >? height_));
}

Tex_font_char_metric::Tex_font_char_metric ()
{
  exists_b_ = false;
  code_ = 0;;
  width_ = height_ = depth_ = italic_correction_ = 0;
  width_fix_ = height_fix_ = depth_fix_ = italic_correction_fix_ = 0;
}

#define APPEND_CHAR_METRIC_ELT(k)  outstr += to_str (#k) + " "  + to_str (k ## _)  + "; "

String
Tex_font_char_metric::str () const
{
  String outstr ;

  APPEND_CHAR_METRIC_ELT (exists_b);
  APPEND_CHAR_METRIC_ELT (code);
  APPEND_CHAR_METRIC_ELT (width);
  APPEND_CHAR_METRIC_ELT (height);
  APPEND_CHAR_METRIC_ELT (depth);
  APPEND_CHAR_METRIC_ELT (italic_correction);
  
  return outstr + "\n";
}

Tex_font_metric::Tex_font_metric ()
{
}

static Tex_font_char_metric dummy_static_char_metric;

Tex_font_char_metric const &
Tex_font_metric::find_ascii (int ascii, bool warn) const
{
  if (ascii < ascii_to_metric_idx_.size() && ascii_to_metric_idx_[ascii] >= 0)
    return char_metrics_[ascii_to_metric_idx_ [ascii]];
  else if (warn)

    {
      warning (_f ("Can't find ascii character: `%d'", ascii));

    }
  return dummy_static_char_metric;  
}

Character_metric const*
Tex_font_metric::get_char (int a, bool w) const
{
  return &find_ascii (a, w);
}


String
Tex_font_metric::str () const
{
  String outstr;
  for (int i=0; i < char_metrics_.size (); i++)
    outstr += char_metrics_[i].str ();
  
  return outstr;
}

void
Tex_font_metric::clear (int n)
{
  for (int i=0; i < n; i++)
    ascii_to_metric_idx_.push (-1);
}

void
Tex_font_metric::read_file (String name)
{
  Tex_font_metric_reader tfm_reader (name);
  *this = tfm_reader.read_tfm ();
}

