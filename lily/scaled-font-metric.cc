#include "font-metric.hh"
#include "string.hh"
#include "molecule.hh"


Scaled_font_metric::Scaled_font_metric (Font_metric* m, Real magn)
{
  magnification_f_ = magn;
  SCM desc = m->description_;

  Real total_mag = magn * gh_scm2double (gh_cdr (desc));
  description_ = gh_cons (gh_car (desc), gh_double2scm (total_mag));
  orig_l_ = m;
}

SCM
Scaled_font_metric::make_scaled_font_metric (Font_metric*m, Real s)
{
  Scaled_font_metric *sfm = new Scaled_font_metric (m,s);
  return sfm->self_scm ();
}

Molecule
Scaled_font_metric::find_by_name (String s) const
{
  Molecule m = orig_l_->find_by_name (s);
  Box b = m.extent_box ();
  b.scale (magnification_f_);
  Molecule q(b,fontify_atom ((Font_metric*) this, m.get_expr ()));

  return q ;
}
