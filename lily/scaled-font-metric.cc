#include "font-metric.hh"
#include "string.hh"
#include "molecule.hh"


Scaled_font_metric::Scaled_font_metric (Font_metric* m, Real magn)
{
  magnification_f_ = magn;
  orig_l_ = m;
}

SCM
Scaled_font_metric::make_scaled_font_metric (Font_metric*m, Real s)
{
  Scaled_font_metric *sfm = new Scaled_font_metric (m,s);
  sfm->name_ = m->name_;
  
  return sfm->self_scm ();
}

SCM
Scaled_font_metric::description () const
{
  SCM od = orig_l_->description ();
  // todo:
  //  gh_set_cdr_x (od, gh_int2scm (magstep_i_));
  return od;
}


Molecule
Scaled_font_metric::find_by_name (String s, Real mag) const
{
  return orig_l_->find_by_name (s, magnification_f_ * mag);	// ugh.
}
