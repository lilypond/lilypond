/*
  font-metric.cc -- implement Font_metric

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

  Mats Bengtsson <matsb@s3.kth.se> (the ugly TeX parsing in text_dimension)
*/

#include "font-metric.hh"

#include <cmath>
#include <cctype>
using namespace std;

#include "dimensions.hh"
#include "modified-font-metric.hh"
#include "open-type-font.hh"
#include "stencil.hh"
#include "virtual-methods.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Real
Font_metric::design_size () const
{
  return 1.0 * point_constant;
}

Stencil
Font_metric::find_by_name (std::string s) const
{
  replace_all (s, '-', 'M');
  int idx = name_to_index (s);
  Box b;

  SCM expr = SCM_EOL;
  if (idx >= 0)
    {
      expr = scm_list_3 (ly_symbol2scm ("named-glyph"),
			 self_scm (),
			 scm_makfrom0str (s.c_str ()));
      b = get_indexed_char (idx);
    }

  Stencil q (b, expr);
  return q;
}

Font_metric::Font_metric ()
{
  description_ = SCM_EOL;
  self_scm_ = SCM_EOL;
  smobify_self ();
}

Font_metric::Font_metric (Font_metric const &)
{
}

Font_metric::~Font_metric ()
{
}

vsize
Font_metric::count () const
{
  return 0;
}

Box
Font_metric::get_ascii_char (vsize) const
{
  return Box (Interval (0, 0), Interval (0, 0));
}

Box
Font_metric::get_indexed_char (vsize k) const
{
  return get_ascii_char (k);
}

vsize
Font_metric::name_to_index (std::string) const
{
  return VPOS;
}

Offset
Font_metric::get_indexed_wxwy (vsize) const
{
  return Offset (0, 0);
}

void
Font_metric::derived_mark () const
{
}

SCM
Font_metric::mark_smob (SCM s)
{
  Font_metric *m = (Font_metric *) SCM_CELL_WORD_1 (s);
  m->derived_mark ();
  return m->description_;
}

int
Font_metric::print_smob (SCM s, SCM port, scm_print_state *)
{
  Font_metric *m = unsmob_metrics (s);
  scm_puts ("#<", port);
  scm_puts (m->class_name (), port);
  scm_puts (" ", port);
  scm_write (m->description_, port);
  scm_puts (">", port);
  return 1;
}

IMPLEMENT_SMOBS (Font_metric);
IMPLEMENT_DEFAULT_EQUAL_P (Font_metric);
IMPLEMENT_TYPE_P (Font_metric, "ly:font-metric?");

SCM
Font_metric::font_file_name () const
{
  return scm_car (description_);
}

std::string
Font_metric::font_name () const
{
  std::string s ("unknown");
  return s;
}

vsize
Font_metric::index_to_ascii (vsize i) const
{
  return i;
}

vsize
Font_metric::index_to_charcode (vsize i) const
{
  return index_to_ascii (i);
}

Stencil
Font_metric::get_ascii_char_stencil (vsize code) const
{
  SCM at = scm_list_3 (ly_symbol2scm ("char"), self_scm (),
		       scm_from_unsigned (code));
  Box b = get_ascii_char (code);
  return Stencil (b, at);
}

Stencil
Font_metric::get_indexed_char_stencil (vsize code) const
{
  vsize idx = index_to_ascii (code);
  SCM at = scm_list_3 (ly_symbol2scm ("char"), self_scm (),
		       scm_from_unsigned (idx));
  Box b = get_indexed_char (code);
  return Stencil (b, at);
}

Offset
Font_metric::attachment_point (std::string) const
{
  return Offset (0, 0);
}

SCM
Font_metric::sub_fonts () const
{
  return SCM_EOL;
}

Stencil
Font_metric::text_stencil (std::string str) const
{
  SCM lst = scm_list_3 (ly_symbol2scm ("text"),
			this->self_scm (),
			scm_makfrom0str (str.c_str ()));

  Box b = text_dimension (str);
  return Stencil (b, lst);
}

Box
Font_metric::text_dimension (std::string) const
{
  return Box (Interval (0, 0), Interval (0, 0));
}

