/*   
  tfm.cc -- implement Tex_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Jan Nieuwenhuizen <janneke@gnu.org>

  some code shamelessly copied from GNU fontutils-0.6/tfm/tfm_input.c
 */

#include "tfm.hh"
#include "tfm-reader.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "warn.hh"
#include "dimensions.hh"

static Tex_font_char_metric dummy_static_char_metric;

Tex_font_char_metric::Tex_font_char_metric ()
{
  exists_ = false;
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

Box
Tex_font_char_metric::dimensions () const
{
  if (!exists_)
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

Tex_font_metric::Tex_font_metric ()
{
  encoding_table_ = SCM_EOL;
}



void
Tex_font_metric::derived_mark () const
{
  scm_gc_mark (encoding_table_);
}

Tex_font_char_metric const *
Tex_font_metric::find_ascii (int ascii, bool warn) const
{
  if (ascii >= 0 && ascii < ascii_to_metric_idx_.size ()
      && ascii_to_metric_idx_[ascii] >= 0)
    return & char_metrics_[ascii_to_metric_idx_ [ascii]];
  else if (warn)
    warning (_f ("can't find ascii character: %d", ascii));
  return &dummy_static_char_metric;  
}

/* UGH: glyphs need not be consecutive in TFM. */
int
Tex_font_metric::count () const
{
  for (int i = ascii_to_metric_idx_.size (); i--;)
    if (ascii_to_metric_idx_[i] != -1)
      return i + 1;
  return 0;
}

Box
Tex_font_metric::get_ascii_char (int a) const
{
  Box b = find_ascii (a)->dimensions ();
  return b;
}

SCM
Tex_font_metric::make_tfm (String filename)
{
  Tex_font_metric *tfm = new Tex_font_metric;
  Tex_font_metric_reader reader (filename);

  tfm->info_ = reader.info_;
  tfm->header_ = reader.header_;
  tfm->char_metrics_ = reader.char_metrics_;
  tfm->ascii_to_metric_idx_ = reader.ascii_to_metric_idx_;

  
  tfm->encoding_table_ =
    scm_call_1 (ly_scheme_function ("get-coding-table"),
		scm_makfrom0str (tfm->info_.coding_scheme.to_str0 ()));

  return tfm->self_scm ();
}

Real
Tex_font_metric::design_size () const
{
  return info_.design_size;
}

String
Tex_font_metric::coding_scheme () const
{
  return info_.coding_scheme;
}

int
Tex_font_metric::name_to_index (String s) const
{
  SCM sym = ly_symbol2scm (s.to_str0 ());

  SCM idx = scm_hash_ref (encoding_table_, sym, SCM_BOOL_F);
  if (scm_integer_p (idx) == SCM_BOOL_T)
    {
      return gh_scm2int (idx);
    }
  else
    return -1;  
}
