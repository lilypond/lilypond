/*   
     scaled-font-metric.cc -- declare Scaled_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "scaled-font-metric.hh"
#include "string.hh"
#include "stencil.hh"


Scaled_font_metric::Scaled_font_metric (Font_metric* m, Real magn)
{
  magnification_ = magn;
  SCM desc = m->description_;

  Real total_mag = magn * gh_scm2double (ly_cdr (desc));
  assert (total_mag);
  
  description_ = gh_cons (ly_car (desc), gh_double2scm (total_mag));
  orig_ = m;
}

SCM
Scaled_font_metric::make_scaled_font_metric (Font_metric*m, Real s)
{
  Scaled_font_metric *sfm = new Scaled_font_metric (m,s);
  return sfm->self_scm ();
}

Real
Scaled_font_metric::design_size () const
{
  return orig_->design_size ();
}

Stencil
Scaled_font_metric::find_by_name (String s) const
{
  Stencil m = orig_->find_by_name (s);
  Box b = m.extent_box ();
  b.scale (magnification_);
  Stencil q (b,fontify_atom ((Font_metric*) this, m.get_expr ()));

  return q ;
}

Box 
Scaled_font_metric::get_indexed_char (int i) const
{
  Box b = orig_->get_indexed_char (i);
  b.scale (magnification_);
  return b;  
}

Box 
Scaled_font_metric::get_ascii_char (int i) const
{
  Box b = orig_->get_ascii_char (i);
  b.scale (magnification_);
  return b;  
}

Box
Scaled_font_metric::text_dimension (String t) const
{
  Box b (orig_->text_dimension (t));

  b.scale (magnification_);
  return b;
}

int
Scaled_font_metric::count () const
{
  return orig_->count ();
}

Offset
Scaled_font_metric::get_indexed_wxwy (int k) const
{
  Offset o = orig_->get_indexed_wxwy (k);
  return o * magnification_;
}

int
Scaled_font_metric::name_to_index (String s)const
{
  return orig_->name_to_index (s);
}
