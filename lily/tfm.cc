/*   
  tfm.cc -- implement Tex_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  

  some code shamelessly copied from GNU fontutils-0.6/tfm/tfm_input.c
 */

#include "tfm.hh"
#include "tfm-reader.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "warn.hh"
#include "dimensions.hh"

Box
Tex_font_char_metric::dimensions () const
{
  if (!exists_b_)
    {
      Box b;
      b.set_empty ();
      return b;
    }
  
  Real d = -depth_;

  Real point_constant = 1 PT;
  
  return Box (Interval (0, width_*point_constant ),
	      Interval ((d <? height_)*point_constant,
			(d >? height_)*point_constant));
}

Tex_font_char_metric::Tex_font_char_metric ()
{
  exists_b_ = false;
  code_ = 0;;
  width_ = 0;
  height_ = 0;
  depth_ = 0;
  italic_correction_ = 0;
  width_fix_ = 0;
  height_fix_ = 0;
  depth_fix_ = 0;
  italic_correction_fix_ = 0;
}

#define APPEND_CHAR_METRIC_ELT(k)  outstr += to_string (#k) + " "  + to_string (k ## _)  + "; "

String
Tex_font_char_metric::string () const
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

Tex_font_char_metric const *
Tex_font_metric::find_ascii (int ascii, bool warn) const
{
  if (ascii < ascii_to_metric_idx_.size () && ascii_to_metric_idx_[ascii] >= 0)
    return & char_metrics_[ascii_to_metric_idx_ [ascii]];
  else if (warn)
    {
      warning (_f ("can't find ascii character: %d", ascii));
    }
  return &dummy_static_char_metric;  
}


/*
  UGH: glyphs need not be consecutive in TFM.
 */
int
Tex_font_metric::count () const
{
  for (int i = ascii_to_metric_idx_.size (); i--;)
    {
      if (ascii_to_metric_idx_[i] != -1)
	return i + 1;
    }
  return 0;
}

Box
Tex_font_metric::get_char (int a) const
{
  Box b = find_ascii (a)->dimensions () ;
  return b;
}


String
Tex_font_metric::string () const
{
  String outstr;
  for (int i=0; i < char_metrics_.size (); i++)
    outstr += char_metrics_[i].string ();
  
  return outstr;
}




SCM
Tex_font_metric::make_tfm (String fn)
{
  Tex_font_metric * tfm = new Tex_font_metric;
  Tex_font_metric_reader reader (fn);

  tfm->info_ = reader.info_;
  tfm->header_ = reader.header_;
  tfm->char_metrics_ = reader.char_metrics_;
  tfm->ascii_to_metric_idx_ = reader.ascii_to_metric_idx_;
  
  return tfm->self_scm ();
}
